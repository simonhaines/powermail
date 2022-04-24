using MailKit.Net.Smtp;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using MimeKit;
using Powermail.Data;
using Powermail.Templates;

namespace Powermail.Processors;

public class MailerConfiguration
{
    public string Host { get; set; } = default!;
    public int Port { get; set; } = default!;
    public string Name { get; set; } = default!;
    public string Address { get; set; } = default!;
    public string User { get; set; } = default!;
    public string Password { get; set; } = default!;
}

public class Mailer
{
    private readonly IOptions<MailerConfiguration> configuration;
    private readonly ILogger<Mailer> logger;

    public Mailer(IOptions<MailerConfiguration> configuration, ILogger<Mailer> logger)
    {
        this.configuration = configuration ?? throw new ArgumentNullException(nameof(configuration));
        this.logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    public async Task Send(Subscriber subscriber, string subject, IEnumerable<ITemplate> templates)
    {
        // Create the message
        var message = new MimeMessage();
        message.From.Add(new MailboxAddress(configuration.Value.Name, configuration.Value.Address));
        message.To.Add(new MailboxAddress(subscriber.Name, subscriber.Email));
        message.Subject = subject;
        
        // Build the body
        var builder = new BodyBuilder();
        foreach (var template in templates)
            template.Render(builder);
        message.Body = builder.ToMessageBody();
        
        // Send
        using var client = new SmtpClient();
        await client.ConnectAsync(configuration.Value.Host, configuration.Value.Port);
        await client.AuthenticateAsync(configuration.Value.User, configuration.Value.Password);
        await client.SendAsync(message);
        await client.DisconnectAsync(true);
    }
}
