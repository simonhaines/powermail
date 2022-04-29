using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Powermail.Data;
using Powermail.Processors;
using Powermail.Server;

Host.CreateDefaultBuilder()
    .UseSystemd()
    .ConfigureServices((context, services) =>
    {
        services
            .AddLogging()
            .AddSingleton(new Data("data.db"))
            .AddSingleton<HttpClient>()
            .AddTransient<Feeds>()
            .Configure<MailerConfiguration>(context.Configuration.GetSection("Mailer"))
            .AddTransient<Mailer>()
            .Configure<SchedulerConfiguration>(context.Configuration.GetSection("Scheduler"))
            .AddHostedService<Scheduler>()
            .Configure<ServerConfiguration>(context.Configuration.GetSection("Server"))
            .AddHostedService<Server>();
    })
    .Build()
    .Run();
