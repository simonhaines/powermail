using MimeKit;

namespace Powermail.Templates;

public interface ITemplate
{
    void Render(BodyBuilder builder);
}