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
        else
        {
            services.Configure<FileSystemConfiguration>(config => 
                config.Path = Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "powermail"));
            services.AddSingleton<IStorage, FileSystem>();
        }

        services
            .AddLogging()
            .AddSingleton<HttpClient>()
            .AddSingleton(new Data("data.db"))
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
