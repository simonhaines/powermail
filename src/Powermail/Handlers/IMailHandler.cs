using MimeKit;

namespace Powermail.Handlers;

public interface IMailHandler
{
    Task Process(MimeMessage message, CancellationToken token);
}