using MailKit.Net.Smtp;
using MailKit.Security;
using Microsoft.Extensions.Options;
using MimeKit;
using Powermail.Data.Models;
using Powermail.Templates;

namespace Powermail.Services;

public class PostOfficeConfiguration
{
    public string Host { get; set; } = default!;
    public int Port { get; set; }
    public string Name { get; set; } = default!;
    public string Address { get; set; } = default!;
    public string User { get; set; } = default!;
    public string Password { get; set; } = default!;
}

public class PostOffice
{
    private readonly IOptions<PostOfficeConfiguration> configuration;

    public PostOffice(IOptions<PostOfficeConfiguration> configuration)
    {
        this.configuration = configuration;
    }

    public async Task Send(User user, string subject, IEnumerable<ITemplate> templates, CancellationToken token)
    {
        // Create the message
        var message = new MimeMessage();
        message.From.Add(new MailboxAddress(configuration.Value.Name, configuration.Value.Address));
        message.To.Add(new MailboxAddress(user.Name, user.Email));
        message.Subject = subject;
        
        // Build the body
        var builder = new BodyBuilder();
        foreach (var template in templates)
            template.Render(builder);
        message.Body = builder.ToMessageBody();
        
        // Send
        using var client = new SmtpClient();
        await client.ConnectAsync(configuration.Value.Host, configuration.Value.Port, SecureSocketOptions.Auto, token);
        await client.AuthenticateAsync(configuration.Value.User, configuration.Value.Password, token);
        await client.SendAsync(message, token);
        await client.DisconnectAsync(true, token);
    }

    public async Task Echo(MimeMessage source, CancellationToken token)
    {
        // Create the message
        var message = new MimeMessage();
        message.From.Add(new MailboxAddress(configuration.Value.Name, configuration.Value.Address));
        message.To.Add(source.From.First());
        message.Subject = string.Concat("Re: ", source.Subject);
        message.InReplyTo = source.MessageId;

        // Create an attachment stream
        await using var stream = new MemoryStream();
        await source.WriteToAsync(stream, token);
        stream.Position = 0;

        // Build the body
        var builder = new BodyBuilder {
            TextBody = "Email message attached",
        };
        await builder.Attachments.AddAsync("email.eml", stream, token);
        message.Body = builder.ToMessageBody();

        // Send
        using var client = new SmtpClient();
        await client.ConnectAsync(configuration.Value.Host, configuration.Value.Port, SecureSocketOptions.Auto, token);
        await client.AuthenticateAsync(configuration.Value.User, configuration.Value.Password, token);
        await client.SendAsync(message, token);
        await client.DisconnectAsync(true, token);
    }
}
