using System.Data;
using System.Net;
using CodeHollow.FeedReader;
using CodeHollow.FeedReader.Feeds;
using LiteDB;
using Microsoft.Extensions.Logging;
using FeedItem = Powermail.Data.FeedItem;

namespace Powermail.Processors;

public class Feeds
{
    private readonly Data.Data db;
    private readonly HttpClient client;
    private readonly ILogger<Feeds> logger;

    public Feeds(Data.Data db, HttpClient client, ILogger<Feeds> logger)
    {
        this.db = db;
        this.client = client;
        this.logger = logger;
    }

    public async Task<IEnumerable<FeedItem>> GetFeed(Powermail.Data.Feed feed)
    {
        try
        {
            using var request = new HttpRequestMessage(HttpMethod.Get, feed.Url);
            request.Headers.IfModifiedSince = feed.Timestamp;

            // Don't hang on slow feeds
            using var tokenSource = new CancellationTokenSource();
            tokenSource.CancelAfter(TimeSpan.FromSeconds(30));

            var response = await client.SendAsync(request, tokenSource.Token);
            feed.LastAccessCode = response.StatusCode;
            if (response.StatusCode == HttpStatusCode.NotModified)
            {
                feed.ErrorCount = 0;
                feed.Timestamp = DateTimeOffset.UtcNow;
                return Enumerable.Empty<FeedItem>();
            }

            if (!response.IsSuccessStatusCode)
            {
                feed.ErrorCount++;
                return Enumerable.Empty<FeedItem>();
            }

            // Select all feed items since the last contact
            var feedContent = FeedReader.ReadFromString(await response.Content.ReadAsStringAsync(tokenSource.Token));
            feed.Name ??= feedContent.Title;
            feed.ErrorCount = 0;
            feed.Timestamp = DateTimeOffset.UtcNow;
            return feedContent.Items.Select(item =>
            {
                // Try to determine a published date
                var published = DateTimeOffset.UtcNow;

                if (item.SpecificItem is AtomFeedItem atomFeedItem)
                {
                    // ATOM feeds use 'updated' as the publish date
                    if (atomFeedItem.UpdatedDate.HasValue)
                        published = new DateTimeOffset(atomFeedItem.UpdatedDate.Value);
                    else if (DateTimeOffset.TryParse(atomFeedItem.UpdatedDateString, out var updatedDate))
                        published = updatedDate;
                }
                else
                {
                    // RSS feeds use the publishing date
                    if (item.PublishingDate.HasValue)
                        published = new DateTimeOffset(item.PublishingDate.Value);
                    else if (DateTimeOffset.TryParse(item.PublishingDateString, out var publishingDate))
                        published = publishingDate;
                }

                return new FeedItem
                {
                    Id = ObjectId.NewObjectId(),
                    FeedId = feed.Id,
                    Title = item.Title,
                    Url = item.Link,
                    Timestamp = published
                };
            });
        }
        catch (OperationCanceledException)
        {
            feed.ErrorCount++;
            feed.LastAccessCode = HttpStatusCode.RequestTimeout;
            return Enumerable.Empty<FeedItem>();
        }
    }

    public async Task<Data.Feed> AddFeed(string url)
    {
        // Already subscribed?
        var feed = db.Feeds.FindOne(f => f.Url == url);
        if (feed == null)
        {
            feed = new Data.Feed
            {
                Id = ObjectId.NewObjectId(),
                Url = url
            };
            foreach (var item in await GetFeed(feed))
                db.FeedItems.Insert(item);
            db.Feeds.Insert(feed);
        }

        return feed;
    }
}