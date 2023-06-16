using MimeKit;
using Powermail.Templates;

namespace Powermail.Handlers;

public interface IMailHandler
{
    Task<ITemplate?> Process(MimeMessage message, CancellationToken token);
}