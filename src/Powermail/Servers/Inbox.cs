using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using MimeKit;
using Powermail.Handlers;

namespace Powermail.Servers;

public class InboxConfiguration
{
    public int Port { get; set; }
}

public class Inbox : BackgroundService
{
    private readonly IOptions<InboxConfiguration> configuration;
    private readonly IServiceProvider serviceProvider;
    private readonly ILogger<Inbox> logger;
    
    public Inbox(IOptions<InboxConfiguration> configuration, IServiceProvider serviceProvider, ILogger<Inbox> logger)
    {
        this.configuration = configuration;
        this.serviceProvider = serviceProvider;
        this.logger = logger;
    }

    protected async override Task ExecuteAsync(CancellationToken token)
    {
        var listener = new TcpListener(IPAddress.Any, configuration.Value.Port);
        listener.Start();
        logger.LogInformation("Server started on port {port}", configuration.Value.Port);

        try
        {
            while (!token.IsCancellationRequested)
            {
                var client = await listener.AcceptTcpClientAsync(token);
                _ = Task.Run(() => Process(client, token), token);
            }
        }
        catch (Exception e)
        {
            if (e is not OperationCanceledException)
                logger.LogCritical("Terminating server: {exception}", e.Message);
        }
        
        logger.LogInformation("Server stopped");
    }

    private void Process(TcpClient client, CancellationToken token)
    {
        var stopwatch = Stopwatch.StartNew();
        try
        {
            using var message = MimeMessage.Load(client.GetStream(), token);
            if (message != null)
            {
                // Load the processing pipeline
                using var scope = serviceProvider.CreateScope();
                var handlers = scope.ServiceProvider.GetServices<IMailHandler>();
                var tasks = handlers.Select(h => h.Process(message, token));
                Task.WhenAll(tasks).Wait(token);
            }
        }
        catch (Exception e)
        {
            logger.LogError(e, "Error processing client connection");
        }
        finally
        {
            logger.LogInformation("Message from {remote} processed in {time}ms",
                client.Client.RemoteEndPoint, stopwatch.ElapsedMilliseconds);

            client.Dispose();
        }
    }
}