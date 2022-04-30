using MimeKit;
using Powermail.Data;
using Powermail.Data.Models;
using Scriban;

namespace Powermail.Templates;

public class FeedTemplate : ITemplate
{
    private static readonly Template Html = Template.Parse(File.ReadAllText("Templates/Feeds.html"));
    private static readonly Template Text = Template.Parse(File.ReadAllText("Templates/Feeds.txt"));

    public string Name { get; init; } = string.Empty;
    public IEnumerable<FeedItem> Items { get; init; } = Enumerable.Empty<FeedItem>();

    public void Render(BodyBuilder builder)
    {
        builder.TextBody += Text.Render(this, member => member.Name);
        builder.HtmlBody += Html.Render(this, member => member.Name);
    }
}