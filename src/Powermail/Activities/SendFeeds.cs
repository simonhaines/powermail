using System.Diagnostics;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;
using Powermail.Data;
using Powermail.Data.Models;
using Powermail.Services;
using Powermail.Templates;

namespace Powermail.Activities;

public class SendFeeds : IActivity
{
    private readonly DataContext data;
    private readonly PostOffice postOffice;
    private readonly ILogger<SendFeeds> logger;

    public SendFeeds(DataContext data, PostOffice postOffice, ILogger<SendFeeds> logger)
    {
        this.data = data;
        this.postOffice = postOffice;
        this.logger = logger;
    }
    
    public async Task Execute(TimeSpan period, CancellationToken token)
    {
        var stopwatch = Stopwatch.StartNew();
        var nextSchedule = DateTime.UtcNow + period;

        try
        {
            // Send all feeds that are scheduled
            foreach (var user in await data.Users.ToListAsync())
            {
                // Ignore this schedule if it is not due
                var due = (user.FeedTimestamp ?? DateTime.MinValue.ToUniversalTime()) + user.FeedInterval; 
                if (due > nextSchedule)
                    continue;

                var templates = await RenderUpdates(user);
                if (templates.Any())
                {
                    logger.LogDebug("Sending {count} feeds to {user}", templates.Count, user.Name);
                    await postOffice.Send(user, "Feed updates", templates, token);

                    // Update the time the feed items were sent (or not)
                    user.FeedTimestamp = DateTime.UtcNow;
                }
                else logger.LogDebug("No items to send for {user}", user.Name);
            }
        }
        catch (Exception e)
        {
            logger.LogError(e, "Exception sending feeds");
        }
        finally
        {
            await data.SaveChangesAsync(token);
            logger.LogDebug("User feeds processed in {time}ms", stopwatch.ElapsedMilliseconds);
        }
    }
    
    private async Task<List<FeedTemplate>> RenderUpdates(User user)
    {
        var lastUpdate = user.FeedTimestamp ?? DateTime.MinValue.ToUniversalTime();
        var result = new List<FeedTemplate>();
        var userFeeds = await data.UserFeeds
            .Where(uf => uf.User == user)
            .Include(uf => uf.Feed).ThenInclude(f => f.Items)
            .ToListAsync();

        foreach (var userFeed in userFeeds)
        {
            var items = userFeed.Feed.Items
                .Where(i => i.Timestamp > lastUpdate)
                .ToList();
            if (items.Count > 0)
                result.Add(new FeedTemplate
                {
                    Name = userFeed.Name ?? userFeed.Feed.Name ?? "The feed with no name",
                    Items = items
                });
        }

        return result;
    }
}