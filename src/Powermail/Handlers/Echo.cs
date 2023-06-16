using System.Runtime.CompilerServices;
using MimeKit;
using Powermail.Templates;

namespace Powermail.Handlers;

public class Echo : IMailHandler
{
    public Task<ITemplate?> Process(MimeMessage message, CancellationToken token)
        => Task.FromResult<ITemplate?>(new Powermail.Templates.Echo(message));
}