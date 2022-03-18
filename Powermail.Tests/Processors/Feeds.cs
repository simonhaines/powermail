using System.IO;
using System.Linq;
using System.Net.Http;
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

        var services = GetServices();
        var feedsService = services.GetRequiredService<Powermail.Processors.Feeds>();

        var feed = await feedsService.AddFeed(wikipediaNewPagesRss);
        Assert.IsNotNull(feed);

        var db = services.GetRequiredService<Data.Data>();
        var items = db.FeedItems.Find(item => item.FeedId == feed.Id);
        Assert.IsTrue(items.Any());
    }

    private ServiceProvider GetServices()
    {
        var services = new ServiceCollection();
        services
            .AddLogging()
            .AddSingleton<HttpClient>()
            .AddSingleton(new Data.Data(new MemoryStream()))
            .AddSingleton<Powermail.Processors.Feeds>();
        return services.BuildServiceProvider();
    }
}