using System.Diagnostics;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;
using Powermail.Data;
using Powermail.Services;

namespace Powermail.Activities;

public class UpdateFeeds : IActivity
{
    private readonly DataContext data;
    private readonly Syndication syndication;
    private readonly ILogger<UpdateFeeds> logger;
    
    public UpdateFeeds(DataContext data, Syndication syndication, ILogger<UpdateFeeds> logger)
    {
        this.data = data;
        this.syndication = syndication;
        this.logger = logger;
    }

    public async Task Execute(TimeSpan period, CancellationToken token)
    {
        logger.LogDebug("Updating feeds");
        var stopwatch = Stopwatch.StartNew();

        try
        {
            var feeds = await data.Feeds
                .Include(f => f.Items)
                .ToListAsync(token);
            
            foreach (var feed in feeds)
            {
                // Filter out feeds that have been updated in the last day
                if (feed.Timestamp.HasValue && feed.Timestamp.Value.Add(TimeSpan.FromDays(1)) > DateTime.UtcNow)
                    continue;

                try
                {
                    var start = stopwatch.ElapsedMilliseconds;

                    // Gather and insert the new feed items
                    await syndication.UpdateFeed(feed, token);
                    logger.LogInformation("Feed '{feed}' ({time}ms): {items} items, {errors} errors",
                        feed.Name, stopwatch.ElapsedMilliseconds - start, feed.Items.Count, feed.ErrorCount);
                }
                catch (OperationCanceledException)
                {
                    logger.LogInformation("Operation cancelled while updating feeds");
                    return;
                }
                catch (Exception e)
                {
                    logger.LogError(e, "Exception updating feed '{name}'", feed.Name);
                }
            }
        }
        finally
        {
            await data.SaveChangesAsync(token);
            logger.LogDebug("Feeds updated in {time}ms", stopwatch.ElapsedMilliseconds);
        }
    }
}