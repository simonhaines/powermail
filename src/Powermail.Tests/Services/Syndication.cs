using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Powermail.Data;
using Powermail.Data.Models;
using Powermail.Services;

namespace Powermail.Tests.Services;

[TestClass]
public class Syndication
{
    [TestMethod]
    public async Task TestAdd()
    {
        const string wikipediaNewPagesAtom = "https://en.wikipedia.org/w/index.php?title=Special:NewPages&feed=atom";
        const string wikipediaNewPagesRss = "https://en.wikipedia.org/w/index.php?title=Special:NewPages&feed=rss";

        var feed = new Feed { Name = "Wikipedia", Items = new List<FeedItem>() };
        var services = GetServices();
        var syndication = services.GetRequiredService<Powermail.Services.Syndication>();
        var db = services.GetRequiredService<DataContext>();
        await db.Database.EnsureCreatedAsync();

        feed.Url = wikipediaNewPagesRss;
        feed.Timestamp = DateTime.MinValue.ToUniversalTime();
        await syndication.Sync(db, feed, CancellationToken.None);
        Assert.IsTrue(feed.Items.Any());
        feed.Items.Clear();

        feed.Url = wikipediaNewPagesAtom;
        feed.Timestamp = DateTime.MinValue.ToUniversalTime();
        await syndication.Sync(db, feed, CancellationToken.None);
        Assert.IsTrue(feed.Items.Any());
    }

    private static ServiceProvider GetServices()
        => new ServiceCollection()
            .AddLogging()
            .AddDbContext<DataContext>(options => options.UseSqlite("Data Source=:memory:"))
            .AddSingleton<HttpClient>()
            .AddTransient<Powermail.Services.Syndication>()
            .BuildServiceProvider();
}