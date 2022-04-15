using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Powermail.Data;
using Powermail.Processors;
using Powermail.Server;
using Powermail.Storage;

Host.CreateDefaultBuilder()
    .ConfigureServices((context, services) =>
    {
        if (context.HostingEnvironment.IsProduction())
        {
            // Running as a SystemD service
            var runtimeDirectory = Environment.GetEnvironmentVariable("RUNTIME_DIRECTORY");
            if (runtimeDirectory != null)
            {
                services.Configure<FileSystemConfiguration>(config => config.Path = runtimeDirectory);
                services.AddSingleton<IStorage, FileSystem>();
            }
            
            // Retrieve the mailer secrets from environment variables (migrate to systemd-creds)
            services.Configure<MailerConfiguration>(config =>
            {
                config.Host = Environment.GetEnvironmentVariable("Mailer.Host") ??
                    throw new Exception("Mailer.Host environment variable not set");
                config.Port = int.Parse(Environment.GetEnvironmentVariable("Mailer.Port") ??
                    throw new Exception("Mailer.Port environment variable not set"));
                config.User = Environment.GetEnvironmentVariable("Mailer.User") ??
                    throw new Exception("Mailer.User environment variable not set");
                config.Password = Environment.GetEnvironmentVariable("Mailer.Password") ??
                    throw new Exception("Mailer.Password environment variable not set");
            });
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
