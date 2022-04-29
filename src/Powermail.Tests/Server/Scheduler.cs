using System;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Threading.Tasks;
using LiteDB;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Powermail.Data;
using Powermail.Processors;

namespace Powermail.Tests.Server;

[TestClass]
public class Scheduler
{
    [TestMethod]
    public async Task TestSubscriberSchedule()
    {
        await using var services = GetServices();
        using var data = services.GetRequiredService<Data.Data>(); ;

        // User
        var user = new User
        {
            Id = ObjectId.NewObjectId(),
            Name = "Test",
            Email = "test@scalardata.com"
        };
        data.Users.Insert(user);
        
        // Schedule for feeds is 24 hours, last sent a week ago
        data.UserSchedules.Insert(new UserSchedule
        {
            Id = ObjectId.NewObjectId(),
            UserId = user.Id,
            FeedTimestamp = DateTime.UtcNow.Subtract(TimeSpan.FromDays(7)),
            FeedInterval = TimeSpan.FromDays(1)
        });
        
        // Add a test feed
        var testFeed = new Feed
        {
            Id = ObjectId.NewObjectId(),
            Name = "Test feed",
            Timestamp = DateTime.UtcNow,
            Url = "https://scalardata.com/feeds/test"
        };
        data.Feeds.Insert(testFeed);
        data.UserFeeds.Insert(new UserFeed
        {
            Id = new ObjectId(),
            UserId = user.Id,
            FeedId = testFeed.Id
        });
        
        // And an item
        data.FeedItems.Insert(new FeedItem
        {
            Id = new ObjectId(),
            FeedId = testFeed.Id,
            Title = "Test item",
            Url = "https://scalardata.com/feeds/test/item",
            Timestamp = DateTime.UtcNow
        });

        // Ensure updates are rendered
        var feeds = services.GetRequiredService<Feeds>();
        var templates = feeds.RenderUpdates(user, null);
        Assert.IsTrue(templates.Any());
    }
    
    private ServiceProvider GetServices()
    {
        var services = new ServiceCollection();
        services
            .AddLogging()
            .AddSingleton<HttpClient>()
            .AddSingleton(new Data.Data(new MemoryStream()))
            .AddSingleton<Feeds>();

        return services.BuildServiceProvider();
    }
}
