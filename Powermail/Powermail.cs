using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Powermail.Data;
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
        }

        services
            .AddLogging()
            .AddSingleton<HttpClient>()
            .AddSingleton(new Data("data.db"))
            .Configure<ServerConfiguration>(config => context.Configuration.Bind("Server", config))
            .AddHostedService<Server>();
    })
    .Build()
    .Run();
