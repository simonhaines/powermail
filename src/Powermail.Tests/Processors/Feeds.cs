using System;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Powermail.Tests.Processors;

[TestClass]
public class Feeds
{
    [TestMethod]
    public async Task TestAdd()
    {
        const string wikipediaNewPagesAtom = "https://en.wikipedia.org/w/index.php?title=Special:NewPages&feed=atom";
        const string wikipediaNewPagesRss = "https://en.wikipedia.org/w/index.php?title=Special:NewPages&feed=rss";

        var feed = new Powermail.Data.Feed { Name = "Wikipedia" };
        var services = GetServices();
        var feedsService = services.GetRequiredService<Powermail.Processors.Feeds>();

        feed.Url = wikipediaNewPagesRss;
        feed.Timestamp = DateTime.MinValue;
        var rssItems = await feedsService.UpdateFeed(feed, CancellationToken.None);
        Assert.IsTrue(rssItems.Any());

        feed.Url = wikipediaNewPagesAtom;
        feed.Timestamp = DateTime.MinValue;
        var atomItems = await feedsService.UpdateFeed(feed, CancellationToken.None);
        Assert.IsTrue(atomItems.Any());
    }

    private ServiceProvider GetServices()
    {
        var services = new ServiceCollection();
        services
            .AddLogging()
            .AddSingleton<HttpClient>()
            .AddSingleton(new Powermail.Data.Data(new MemoryStream()))
            .AddSingleton<Powermail.Processors.Feeds>();
        return services.BuildServiceProvider();
    }
}