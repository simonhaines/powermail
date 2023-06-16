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
            await template.Render(builder, token);
        message.Body = builder.ToMessageBody();
        await Send(message, token);
    }

    public async Task Send(MimeMessage message, CancellationToken token)
    {
        using var client = new SmtpClient();
        await client.ConnectAsync(configuration.Value.Host, configuration.Value.Port, SecureSocketOptions.Auto, token);
        await client.AuthenticateAsync(configuration.Value.User, configuration.Value.Password, token);
        await client.SendAsync(message, token);
        await client.DisconnectAsync(true, token);
    }

    public MimeMessage CreateReply(MimeMessage source)
    {
        var reply = new MimeMessage();
        reply.From.Add(new MailboxAddress(configuration.Value.Name, configuration.Value.Address));
        reply.To.Add(source.From.First());
        reply.Subject = string.Concat("Re: ", source.Subject);
        reply.InReplyTo = source.MessageId;
        return reply;
    }
}
