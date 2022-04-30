using Microsoft.EntityFrameworkCore;
using MimeKit;
using Powermail.Data;
using Powermail.Services;

namespace Powermail.Handlers;

public class Echo : IMailHandler
{
    private readonly DataContext data;
    private readonly PostOffice postOffice;

    public Echo(DataContext data, PostOffice postOffice)
    {
        this.data = data;
        this.postOffice = postOffice;
    }

    public async Task Process(MimeMessage message, CancellationToken token)
    {
        if (message.Subject.ToLowerInvariant().StartsWith("echo"))
            await postOffice.Echo(message, token);  // Echo back to sender
    }
}