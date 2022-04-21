using Microsoft.VisualStudio.TestTools.UnitTesting;
using Powermail.Parsers;
using Superpower;
using Superpower.Model;
using Superpower.Parsers;

namespace Powermail.Tests.Parsers;

[TestClass]
public class TestFeeds
{
    [TestMethod]
    public void TestUntil()
    {
        const string source = "This is a test, designed to provoke an emotional response";

        var result = Character.AnyChar.Until(Span.EqualTo("test"),
            preamble => Assert.AreEqual("This is a ", preamble.ToStringValue()))
            .Parse(source);
        Assert.AreEqual("test", result.ToStringValue());
    }

    [TestMethod]
    public void TestSubscribe()
    {
        Feed feed;
        Result<Feed> result;
        
        feed = Feeds.Subscribe.Parse("subscribe me to feed https://example.org/feed.xml");
        Assert.AreEqual("https://example.org/feed.xml", feed.Url.ToStringValue());

        feed = Feeds.Subscribe.Parse("subscribe to feed https://example.org/feed.xml");
        Assert.AreEqual("https://example.org/feed.xml", feed.Url.ToStringValue());

        feed = Feeds.Subscribe.Parse("subscribe to the feed https://example.org/feed.xml");
        Assert.AreEqual("https://example.org/feed.xml", feed.Url.ToStringValue());

        feed = Feeds.Subscribe.Parse("subscribe feed https://example.org/feed.xml");
        Assert.AreEqual("https://example.org/feed.xml", feed.Url.ToStringValue());

        feed = Feeds.Subscribe.Parse("subscribe https://example.org/feed.xml");
        Assert.AreEqual("https://example.org/feed.xml", feed.Url.ToStringValue());

        feed = Feeds.Subscribe.Parse("subscribe https://example.org/feed.xml but ignore this");
        Assert.AreEqual("https://example.org/feed.xml", feed.Url.ToStringValue());

        feed = Feeds.Subscribe.Parse("subscribe https://127.0.0.1/feed.xml");
        Assert.AreEqual("https://127.0.0.1/feed.xml", feed.Url.ToStringValue());

        result = Feeds.Subscribe.TryParse("subscribe telnet://127.0.0.1/feed.xml");
        Assert.IsFalse(result.HasValue);
    }
}