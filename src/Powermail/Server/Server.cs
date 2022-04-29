using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using LiteDB;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using MimeKit;

namespace Powermail.Server;

public class ServerConfiguration
{
    public int Port { get; set; }
}

public class Server : BackgroundService
{
    private readonly IOptions<ServerConfiguration> configuration;
    private readonly Data.Data data;
    private readonly ILogger<Server> logger;
    
    public Server(IOptions<ServerConfiguration> configuration, Data.Data data, ILogger<Server> logger)
    {
        this.configuration = configuration;
        this.data = data;
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
                // Save the whole message
                await using var messageStream = new MemoryStream();
                await message.WriteToAsync(messageStream, token);
                messageStream.Position = 0;

                var id = ObjectId.NewObjectId();
                data.InboxStorage.Upload(id, id.ToString(), messageStream);
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