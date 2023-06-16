using MimeKit;

namespace Powermail.Templates;

public interface ITemplate
{
    Task Render(BodyBuilder builder, CancellationToken token);
}