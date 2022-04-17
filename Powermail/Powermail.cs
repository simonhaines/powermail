using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Powermail.Data;
using Powermail.Processors;
using Powermail.Server;
using Powermail.Storage;

Host.CreateDefaultBuilder()
    .UseSystemd()
    .ConfigureServices((context, services) =>
    {
        if (context.HostingEnvironment.EnvironmentName == "systemd")
        {
            // Running as a SystemD service
            var runtimeDirectory = Environment.GetEnvironmentVariable("RUNTIME_DIRECTORY");
            if (runtimeDirectory != null)
            {
                services.Configure<FileSystemConfiguration>(config => config.Path = runtimeDirectory);
                services.AddSingleton<IStorage, FileSystem>();
            }
        }

        services
            .AddLogging()
            .AddSingleton<HttpClient>()
            .AddSingleton(new Data("data.db"))
            .AddTransient<IStorage, FileSystem>()
            .AddTransient<Feeds>()
            .Configure<MailerConfiguration>(config => context.Configuration.Bind("Mailer", config))
            .AddTransient<Mailer>()
            .Configure<SchedulerConfiguration>(config => context.Configuration.Bind("Scheduler", config))
            .AddHostedService<Scheduler>()
            .Configure<ServerConfiguration>(config => context.Configuration.Bind("Server", config))
            .AddHostedService<Server>();
    })
    .Build()
    .Run();
