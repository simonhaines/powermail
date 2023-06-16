using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using MimeKit;
using Powermail.Handlers;
using Powermail.Services;
using Powermail.Templates;

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
                _ = Task.Run(async () =>
                {
                    await Process(client, token);
                    client.Dispose();
                }, token);
            }
        }
        catch (Exception e)
        {
            if (e is not OperationCanceledException)
                logger.LogCritical("Terminating server: {exception}", e.Message);
        }
        
        logger.LogInformation("Server stopped");
    }

    private async Task Process(TcpClient client, CancellationToken token)
    {
        var stopwatch = Stopwatch.StartNew();
        try
        {
            using var message = await MimeMessage.LoadAsync(client.GetStream(), token);
            if (message != null)
            {
                using var scope = serviceProvider.CreateScope();
                
                // Load the processing pipeline and gather the templates
                var handlers = scope.ServiceProvider.GetServices<IMailHandler>();

                var templates = new List<ITemplate>();
                foreach (var handler in handlers)
                {
                    var template = await handler.Process(message, token);
                    if (template != null)
                        templates.Add(template);
                }
                
                // If no processor could handle the mail, just return it
                // FIXME disabled for now so don't reply to spam etc (need to check for user)
                //if (!templates.Any())
                //    templates.Add(new NoAction(message));
                
                // Create a reply and send it
                if (templates.Any())
                {
                    var bodyBuilder = new BodyBuilder();
                    foreach (var template in templates)
                        await template.Render(bodyBuilder, token);

                    var postOffice = scope.ServiceProvider.GetRequiredService<PostOffice>();
                    var reply = postOffice.CreateReply(message);
                    reply.Body = bodyBuilder.ToMessageBody();
                    await postOffice.Send(reply, token);
                }
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
        }
    }
}