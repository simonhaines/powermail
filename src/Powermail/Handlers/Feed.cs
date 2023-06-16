using System.Runtime.CompilerServices;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;
using MimeKit;
using Powermail.Data;
using Powermail.Data.Models;
using Powermail.Parsers;
using Powermail.Services;
using Powermail.Templates;
using Superpower;

namespace Powermail.Handlers;

public class Feed : IMailHandler
{
    private readonly DataContext dataContext;
    private readonly Syndication syndication;
    private readonly ILogger<Feed> logger;

    public Feed(DataContext dataContext, Syndication syndication, ILogger<Feed> logger)
    {
        this.dataContext = dataContext;
        this.syndication = syndication;
        this.logger = logger;
    }

    public async Task<ITemplate?> Process(MimeMessage request, CancellationToken token)
    { 
        // Check for subscribes
        var subscribe = Feeds.Subscribe.TryParse(request.TextBody);
        if (!subscribe.HasValue) return null;

        // Check authentication
        var user = await dataContext.Users
            .Where(u => u.Email.Contains(request.Sender.Address))
            .Include(u => u.Feeds)
            .FirstOrDefaultAsync(token);
        if (user == default)
        {
            logger.LogInformation("Feed subscription request from non-user");
            return null;
        }

        var feed = new Data.Models.Feed
        {
            Url = subscribe.Value.Url.ToStringValue(),
            Timestamp = DateTime.MinValue
        };

        // FIXME: check for duplicate feeds (already added)
        await syndication.UpdateFeed(feed, token);
        dataContext.UserFeeds.Add(new UserFeed { Feed = feed });
        await dataContext.SaveChangesAsync(token);
        logger.LogInformation($"Subscription added: user = {user.Name}, feed = {feed.Name}");

        return new SubscriptionAdded(feed, user.Feeds);
    }
}
