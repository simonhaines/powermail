using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using Powermail.Activities;

namespace Powermail.Servers;

public class SchedulerConfiguration
{
    public TimeSpan Interval { get; set; } = TimeSpan.FromMinutes(30);
}

public class Scheduler : IHostedService
{
    private readonly IOptions<SchedulerConfiguration> configuration;
    private readonly IServiceProvider serviceProvider;
    private readonly ILogger<Scheduler> logger;
    private readonly CancellationTokenSource tokenSource;
    private Timer? timer;
    
    public Scheduler(IOptions<SchedulerConfiguration> configuration,
        IServiceProvider serviceProvider, ILogger<Scheduler> logger)
    {
        this.configuration = configuration;
        this.serviceProvider = serviceProvider;
        this.logger = logger;
        tokenSource = new CancellationTokenSource();
    }

    public Task StartAsync(CancellationToken cancellationToken)
    {
        // Immediately schedule all activities on startup
        timer = new Timer(Execute, null, TimeSpan.FromSeconds(10), Timeout.InfiniteTimeSpan);
        
        logger.LogInformation("Scheduler started: interval = {interval}", configuration.Value.Interval);
        return Task.CompletedTask;
    }

    public Task StopAsync(CancellationToken cancellationToken)
    {
        tokenSource.Cancel();
        timer?.Dispose();
        logger.LogInformation("Scheduler stopped");
        return Task.CompletedTask;
    }

    private void Execute(object? _)
    {
        var trigger = DateTime.UtcNow;

        try
        {
            using var scope = serviceProvider.CreateAsyncScope();
            var activities = scope.ServiceProvider.GetServices<IActivity>();
            var tasks = activities.Select(a => a.Execute(configuration.Value.Interval, tokenSource.Token));
            Task.WhenAll(tasks).Wait();
        }
        catch (Exception e)
        {
            logger.LogError(e, "Error executing schedule");
        }

        // Re-trigger the timer if required
        if (!tokenSource.IsCancellationRequested)
        {
            while (trigger < DateTime.UtcNow)
                trigger = trigger.Add(configuration.Value.Interval);
            var delta = trigger - DateTime.UtcNow;
            timer?.Change(delta, Timeout.InfiniteTimeSpan);
            logger.LogDebug("Next schedule: {trigger}", trigger.ToLocalTime().ToString("s"));
        }
    }
}
