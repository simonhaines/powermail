using Microsoft.VisualStudio.TestTools.UnitTesting;
using Powermail.Parsers;
using Superpower;
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
        
        feed = Feeds.Subscribe().Parse("subscribe me to feed Rachael By The Bay https://rachael.org/feed.xml");
        Assert.AreEqual("Rachael By The Bay", feed.Name.ToStringValue().TrimEnd());
        Assert.AreEqual("https://rachael.org/feed.xml", feed.Url.ToStringValue());

        feed = Feeds.Subscribe().Parse("subscribe to feed Rachael By The Bay https://rachael.org/feed.xml");
        Assert.AreEqual("Rachael By The Bay", feed.Name.ToStringValue().TrimEnd());
        Assert.AreEqual("https://rachael.org/feed.xml", feed.Url.ToStringValue());

        feed = Feeds.Subscribe().Parse("subscribe to the feed Rachael By The Bay https://rachael.org/feed.xml");
        Assert.AreEqual("Rachael By The Bay", feed.Name.ToStringValue().TrimEnd());
        Assert.AreEqual("https://rachael.org/feed.xml", feed.Url.ToStringValue());
    }
}