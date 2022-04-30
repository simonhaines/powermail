using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Powermail.Activities;
using Powermail.Data;
using Powermail.Handlers;
using Powermail.Services;
using Powermail.Servers;

Host.CreateDefaultBuilder()
    .UseSystemd()
    .ConfigureServices((context, services) =>
    {
        services
            .AddLogging()
            .AddDbContext<DataContext>(options =>
            {
                options.UseSqlite(context.Configuration.GetConnectionString("SQLite"));
            })
            .AddSingleton<HttpClient>()
            .AddTransient<Syndication>()
            .Configure<PostOfficeConfiguration>(context.Configuration.GetSection("PostOffice"))
            .AddTransient<PostOffice>()

            // Scheduled activities
            .AddTransient<IActivity, SendFeeds>()
            .AddTransient<IActivity, UpdateFeeds>()
            
            // Mail handlers
            .AddTransient<IMailHandler, Echo>()

            // Servers
            .Configure<SchedulerConfiguration>(context.Configuration.GetSection("Scheduler"))
            .AddHostedService<Scheduler>()
            .Configure<InboxConfiguration>(context.Configuration.GetSection("Inbox"))
            .AddHostedService<Inbox>();
    })
    .Build()
    .Run();
