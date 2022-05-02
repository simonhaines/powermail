using System.Net;
using System.Net.Http.Headers;
using CodeHollow.FeedReader;
using CodeHollow.FeedReader.Feeds;
using Microsoft.Extensions.Logging;
using Powermail.Data;

namespace Powermail.Services;

public class Syndication
{
    private readonly HttpClient client;
    private readonly ILogger<Syndication> logger;

    public Syndication(HttpClient client, ILogger<Syndication> logger)
    {
        this.client = client ?? throw new ArgumentNullException(nameof(client));
        this.logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    public async Task Sync(DataContext data, Data.Models.Feed feed, CancellationToken token)
    {
        try
        {
            using var request = new HttpRequestMessage(HttpMethod.Get, feed.Url);
            request.Headers.UserAgent.Add(new ProductInfoHeaderValue("Powermail", "1.0"));
            request.Headers.IfModifiedSince = feed.Timestamp;

            // Don't hang on slow feeds
            using var tokenSource = CancellationTokenSource.CreateLinkedTokenSource(token);
            tokenSource.CancelAfter(TimeSpan.FromSeconds(30));

            var response = await client.SendAsync(request, tokenSource.Token);
            feed.LastAccessCode = response.StatusCode;
            if (response.StatusCode == HttpStatusCode.NotModified)
            {
                feed.ErrorCount = 0;
                feed.Timestamp = DateTime.UtcNow;
                return;
            }

            if (!response.IsSuccessStatusCode)
            {
                feed.ErrorCount++;
                return;
            }

            // Select all feed items since the last contact
            var feedContent = FeedReader.ReadFromString(await response.Content.ReadAsStringAsync(tokenSource.Token));
            feed.Name ??= feedContent.Title;
            feed.ErrorCount = 0;
            feed.Timestamp = DateTime.UtcNow;

            // Remove old items
            data.FeedItems.RemoveRange(feed.Items);
            feed.Items.Clear();

            foreach (var item in feedContent.Items)
            {
                // Try to determine a published date
                var published = DateTime.UtcNow;
                
                if (item.SpecificItem is AtomFeedItem atomFeedItem)
                {
                    // ATOM feeds use 'updated' as the publish date
                    if (atomFeedItem.UpdatedDate.HasValue)
                        published = atomFeedItem.UpdatedDate.Value.ToUniversalTime();
                    else if (DateTime.TryParse(atomFeedItem.UpdatedDateString, out var updatedDate))
                        published = updatedDate.ToUniversalTime();
                }
                else
                {
                    // RSS feeds use the publishing date
                    if (item.PublishingDate.HasValue)
                        published = item.PublishingDate.Value.ToUniversalTime();
                    else if (DateTime.TryParse(item.PublishingDateString, out var publishingDate))
                        published = publishingDate.ToUniversalTime();
                }

                feed.Items.Add(new Data.Models.FeedItem
                {
                    FeedId = feed.Id,
                    Title = item.Title,
                    Url = item.Link,
                    Timestamp = published
                });
            }
        }
        catch (OperationCanceledException)
        {
            feed.ErrorCount++;
            feed.LastAccessCode = HttpStatusCode.RequestTimeout;
        }
    }
}