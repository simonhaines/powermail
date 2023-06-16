using MimeKit;

namespace Powermail.Templates;

public class Echo : ITemplate
{
    private readonly MimeMessage source;

    public Echo(MimeMessage source)
    {
        this.source = source;
    }

    public async Task Render(BodyBuilder builder, CancellationToken token)
    {
        // Create an attachment stream
        await using var stream = new MemoryStream();
        await source.WriteToAsync(stream, token);
        stream.Position = 0;

        // Add the attachment
        await builder.Attachments.AddAsync("email.eml", stream, token);
    }
}