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
        var data = services.GetRequiredService<Powermail.Data.Data>();

        // Subscriber
        var subscriber = new Subscriber
        {
            Id = ObjectId.NewObjectId(),
            Name = "Test",
            Email = "test@scalardata.com"
        };
        data.Subscribers.Insert(subscriber);
        
        // Schedule for feeds is 24 hours, last sent a week ago
        data.SubscriberSchedules.Insert(new SubscriberSchedule
        {
            Id = ObjectId.NewObjectId(),
            SubscriberId = subscriber.Id,
            FeedTimestamp = DateTimeOffset.UtcNow.Subtract(TimeSpan.FromDays(7)),
            FeedInterval = TimeSpan.FromDays(1)
        });
        
        // Add a test feed
        var testFeed = new Feed
        {
            Id = ObjectId.NewObjectId(),
            Name = "Test feed",
            Timestamp = DateTimeOffset.UtcNow,
            Url = "https://scalardata.com/feeds/test"
        };
        data.Feeds.Insert(testFeed);
        data.SubscriberFeeds.Insert(new SubscriberFeed
        {
            Id = new ObjectId(),
            SubscriberId = subscriber.Id,
            FeedId = testFeed.Id
        });
        
        // And an item
        data.FeedItems.Insert(new FeedItem
        {
            Id = new ObjectId(),
            FeedId = testFeed.Id,
            Title = "Test item",
            Url = "https://scalardata.com/feeds/test/item",
            Timestamp = DateTimeOffset.UtcNow
        });
        
        // Ensure updates are rendered
        var feeds = services.GetRequiredService<Feeds>();
        var templates = feeds.RenderUpdates(subscriber, DateTimeOffset.MinValue);
        Assert.IsTrue(templates.Any());
    }
    
    private ServiceProvider GetServices()
    {
        var services = new ServiceCollection();
        services
            .AddLogging()
            .AddSingleton<HttpClient>()
            .AddSingleton(new Powermail.Data.Data(new MemoryStream()))
            .AddSingleton<Feeds>();

        return services.BuildServiceProvider();
    }
}