using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using MimeKit;
using Powermail.Storage;

namespace Powermail.Server;

public class ServerConfiguration
{
    public int Port { get; set; }
}

public class Server : BackgroundService
{
    private readonly IOptions<ServerConfiguration> configuration;
    private readonly IStorage storage;
    private readonly ILogger<Server> logger;
    
    public Server(IOptions<ServerConfiguration> configuration, IStorage storage, ILogger<Server> logger)
    {
        this.configuration = configuration;
        this.storage = storage;
        this.logger = logger;
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        var listener = new TcpListener(IPAddress.Any, configuration.Value.Port);
        listener.Start();
        logger.LogInformation("Server started on port {port}", configuration.Value.Port);

        try
        {
            while (!stoppingToken.IsCancellationRequested)
            {
                var client = await listener.AcceptTcpClientAsync(stoppingToken);
                _ = Task.Run(async () => await Process(client, stoppingToken), stoppingToken);
            }
        }
        catch (Exception e)
        {
            if (e is not OperationCanceledException)
                logger.LogCritical("Terminating server: {exception}", e.Message);
        }
    }

    private async Task Process(TcpClient client, CancellationToken token)
    {
        var stopwatch = Stopwatch.StartNew();

        await using var stream = client.GetStream();
        using var message = await MimeMessage.LoadAsync(stream, token);
        if (message != null)
        {
            var guido = Guid.NewGuid().ToString();

            // Save the whole message
            await using var messageStream = storage.GetStream($"{guido}.mime");
            await message.WriteToAsync(messageStream, token);
                
            foreach (var bodyPart in message.BodyParts)
            {
                // Save the text/plain part
                if (bodyPart is not MimePart mimePart || !mimePart.ContentType.IsMimeType("text", "plain")) continue;

                await using var fileStream = storage.GetStream($"{guido}.txt");
                await mimePart.Content.WriteToAsync(fileStream, token);
                break;
            }
        }
        
        logger.LogInformation("Message from {remote} processed in {time}ms",
            client.Client.RemoteEndPoint, stopwatch.ElapsedMilliseconds);
    }
}