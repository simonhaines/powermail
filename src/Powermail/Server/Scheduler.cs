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
    private readonly Mailer mailer;
    private readonly ILogger<Scheduler> logger;
    private readonly CancellationTokenSource tokenSource;

    private readonly Feeds feeds;
    private Timer? updateFeedTimer;
    private Timer? sendFeedTimer;
    
    public Scheduler(IOptions<SchedulerConfiguration> configuration, Data.Data data, Feeds feeds,
        Mailer mailer, ILogger<Scheduler> logger)
    {
        this.configuration = configuration ?? throw new ArgumentNullException(nameof(configuration));
        this.data = data ?? throw new ArgumentNullException(nameof(data));
        this.feeds = feeds ?? throw new ArgumentNullException(nameof(feeds));
        this.mailer = mailer ?? throw new ArgumentNullException(nameof(mailer));
        this.logger = logger ?? throw new ArgumentNullException(nameof(logger));
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
            start.ToLocalTime().ToString("s"), configuration.Value.UpdateFeeds);
        
        // Send feed items to users every interval
        sendFeedTimer = new Timer(SendUserFeeds, null, start - utc, configuration.Value.SendFeeds);
        logger.LogInformation("Send feed timer started: next = {start}, interval = {interval}",
            start.ToLocalTime().ToString("s"), configuration.Value.SendFeeds);

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

        try
        {
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
                        feed.Name, stopwatch.ElapsedMilliseconds - feedStart, items.Count(), feed.ErrorCount);
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
            logger.LogInformation("Feeds updates in {time}ms", stopwatch.ElapsedMilliseconds);
        }
    }

    private void SendUserFeeds(object? state)
    {
        logger.LogInformation("Sending user feeds");
        var stopwatch = Stopwatch.StartNew();

        try
        {
            // Check all user feed schedules and see if any are due
            var schedules = data.UserSchedules.FindAll().ToList();
            logger.LogDebug("Processing {count} schedules", schedules.Count());
            foreach (var schedule in schedules)
            {
                if (!IsScheduleDue(schedule, configuration.Value.SendFeeds))
                    continue;

                var user = data.Users.FindById(schedule.UserId);
                if (user == null)
                {
                    logger.LogWarning("No user with Id '{user}'", schedule.UserId);
                    continue;
                }

                var templates = feeds.RenderUpdates(user, schedule.FeedTimestamp).ToList();
                if (templates.Any())
                {
                    logger.LogInformation("Sending {count} feeds to {user}", templates.Count, user.Name);
                    mailer.Send(user, "Feed updates", templates).Wait();

                    // Update the time the feed items were sent (or not)
                    schedule.FeedTimestamp = DateTime.UtcNow;
                    data.UserSchedules.Update(schedule);
                }
                else logger.LogInformation("No items to send for {user}", user.Name);
            }
        }
        catch (Exception e)
        {
            logger.LogError(e, "Exception sending feeds");
        }
        finally
        {
            logger.LogDebug("User feeds processed in {time}ms", stopwatch.ElapsedMilliseconds);
        }
    }

    /// <summary>Determine if a schedule becomes due in the next interval</summary>
    private static bool IsScheduleDue(UserSchedule schedule, TimeSpan interval)
    {
        var period = DateTime.UtcNow + interval;
        return !schedule.FeedTimestamp.HasValue
            || schedule.FeedTimestamp.Value + schedule.FeedInterval < period;
    }
}