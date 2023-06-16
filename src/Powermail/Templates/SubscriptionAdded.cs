using MimeKit;
using Powermail.Data.Models;
using Scriban;

namespace Powermail.Templates;

public class SubscriptionAdded : ITemplate
{
    private static readonly Template Html = Template.Parse(File.ReadAllText("Templates/SubscriptionAdded.html"));
    private static readonly Template Text = Template.Parse(File.ReadAllText("Templates/SubscriptionAdded.txt"));

    public Feed Feed { get; }
    public IEnumerable<UserFeed> UserFeeds { get; }

    public SubscriptionAdded(Feed feed, IEnumerable<UserFeed> userFeeds)
    {
        Feed = feed ?? throw new ArgumentNullException(nameof(feed));
        UserFeeds = userFeeds ?? throw new ArgumentNullException(nameof(userFeeds));
    }

    public async Task Render(BodyBuilder builder, CancellationToken token)
    {
        builder.TextBody += await Text.RenderAsync(this, member => member.Name);
        builder.HtmlBody += await Html.RenderAsync(this, member => member.Name);

        if (!Feed.Items.Any()) return;
        
        var feedTemplate = new FeedTemplate { Name = Feed.Name ?? "New feed", Items = Feed.Items };
        await feedTemplate.Render(builder, token);
    }
}
