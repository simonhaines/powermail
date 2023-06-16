using MimeKit;

namespace Powermail.Templates;

public class NoAction : ITemplate
{
    private readonly MimeMessage source;

    public NoAction(MimeMessage source)
    {
        this.source = source ?? throw new ArgumentNullException(nameof(source));
    }

    public async Task Render(BodyBuilder builder, CancellationToken token)
    {
        builder.TextBody += "Your email produced no results.";
        builder.HtmlBody += "<div dir=\"ltr\">Your email produced no results.</div>";
        
        // Create an attachment stream
        await using var stream = new MemoryStream();
        await source.WriteToAsync(stream, token);
        stream.Position = 0;

        // Add the attachment
        await builder.Attachments.AddAsync("email.eml", stream, token);
    }
}