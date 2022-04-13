using System.Diagnostics;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using Powermail.Data;
using Powermail.Processors;

namespace Powermail.Server;

public class SchedulerConfiguration
{
    public TimeSpan UpdateFeeds { get; set; } = TimeSpan.FromDays(1);
    public TimeSpan SendFeeds { get; set; } = TimeSpan.FromMinutes(30);
}

public class Scheduler : IHostedService
{
    private readonly IOptions<SchedulerConfiguration> configuration;
    private readonly Data.Data data;
    private readonly ILogger<Scheduler> logger;
    private readonly CancellationTokenSource tokenSource;

    private readonly Feeds feeds;
    private Timer? updateFeedTimer;
    private Timer? sendFeedTimer;
    
    public Scheduler(IOptions<SchedulerConfiguration> configuration, Data.Data data, Feeds feeds,
        ILogger<Scheduler> logger)
    {
        this.configuration = configuration;
        this.data = data;
        this.logger = logger;
        this.feeds = feeds;
        tokenSource = new CancellationTokenSource();
    }

    public Task StartAsync(CancellationToken cancellationToken)
    {
        // Update feeds every 24 hours
        var utc = DateTime.UtcNow;
        var start = new DateTime(utc.Year, utc.Month, utc.Day, utc.Hour, 50, 0, DateTimeKind.Utc);
        while (start < utc)
            start = start.AddHours(1);
        
        updateFeedTimer = new Timer(UpdateFeeds, null, start - utc, configuration.Value.UpdateFeeds);
        logger.LogInformation("Update feed timer started: next = {start}, interval = {interval}",
            start.ToString("O"), configuration.Value.UpdateFeeds);
        
        // Send feed items to subscribers every interval
        sendFeedTimer = new Timer(SendSubscriberFeeds, null, start - utc, configuration.Value.SendFeeds);
        logger.LogInformation("Send feed timer started: next = {start}, interval = {interval}",
            start.ToString("O"), configuration.Value.SendFeeds);

        logger.LogInformation("Scheduler started");
        return Task.CompletedTask;
    }

    public Task StopAsync(CancellationToken cancellationToken)
    {
        tokenSource.Cancel();
        updateFeedTimer?.Dispose();
        sendFeedTimer?.Dispose();
        logger.LogInformation("Scheduler stopped");
        return Task.CompletedTask;
    }

    private async void UpdateFeeds(object? state)
    {
        logger.LogInformation("Updating feeds");
        var stopwatch = Stopwatch.StartNew();

        foreach (var feed in data.Feeds.FindAll())
        {
            try
            {
                var feedStart = stopwatch.ElapsedMilliseconds;

                // Gather and insert the new feed items
                var items = await feeds.UpdateFeed(feed, tokenSource.Token);
                data.FeedItems.InsertBulk(items);
                data.Feeds.Update(feed);
                logger.LogInformation("Feed '{feed}' ({time}ms): {items} items, {errors} errors",
                    stopwatch.ElapsedMilliseconds - feedStart, feed.Name, items.Count(), feed.ErrorCount);
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

    private void SendSubscriberFeeds(object? state)
    {
        try
        {
            // Check all subscriber feed schedules and see if any are due
            foreach (var schedule in data.SubscriberSchedules.FindAll())
            {
                if (!IsScheduleDue(schedule, configuration.Value.SendFeeds))
                    continue;

                var subscriber = data.Subscribers.FindById(schedule.SubscriberId);
                if (subscriber == null)
                {
                    logger.LogWarning("No subscriber with Id '{subscriber}'", schedule.SubscriberId);
                    continue;
                }
                    
                logger.LogInformation("Sending feed items to subscriber '{subscriber}'", subscriber.Name);
                // TODO

                // Update the time the feed items were sent
                schedule.FeedDelivery = DateTimeOffset.UtcNow;
                data.SubscriberSchedules.Update(schedule);
            }
        }
        catch (Exception e)
        {
            logger.LogError(e, "Exception sending feeds");
        }
    }

    /// <summary>Determine if a schedule becomes due in the next interval</summary>
    private static bool IsScheduleDue(SubscriberSchedule schedule, TimeSpan interval)
    {
        var period = DateTime.UtcNow + interval;
        return schedule.FeedDelivery.HasValue
            && schedule.FeedDeliveryInterval.HasValue
            && schedule.FeedDelivery + schedule.FeedDeliveryInterval < period;
    }
}