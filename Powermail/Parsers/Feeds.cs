using System.Resources;
using Org.BouncyCastle.Asn1.Cms;
using Superpower;
using Superpower.Model;
using Superpower.Parsers;

namespace Powermail.Parsers;

public class Feed
{
    public TextSpan Url { get; set; }
    public TextSpan Name { get; set; }
}

public static class Feeds
{
    // subscribe me to feed Rachael By The Bay https://rachael.org/feed.xml
    
    private static readonly TextParser<Unit> WordBoundary
        = Character.WhiteSpace
            .Or(Character.AnyChar
                .Where(char.IsPunctuation))
            .IgnoreMany()
            .Named("word boundary");

    private static TextParser<TextSpan> Word(string value)
        => Span.EqualToIgnoreCase(value).Named("word");

    private static readonly TextParser<Unit> Preamble
        = Span.EqualToIgnoreCase("subscribe")
            .IgnoreThen(WordBoundary)
            .IgnoreThen(Word("me")
                .IgnoreThen(WordBoundary).Optional())
            .IgnoreThen(Word("to"))
            .IgnoreThen(WordBoundary)
            .IgnoreThen(Word("the")
                .IgnoreThen(WordBoundary).Optional())
            .IgnoreThen(Word("feed"))
            .IgnoreThen(WordBoundary)
            .Value(Unit.Value);

    private static readonly TextParser<TextSpan> Url =
        Span.EqualTo("https://rachael.org/feed.xml");

    public static TextParser<Feed> Subscribe()
    {
        var feed = new Feed();
        return Preamble
            .IgnoreThen(Character.AnyChar.Until(Url, content => feed.Name = content))
            .Then(url =>
            {
                feed.Url = url;
                return Parse.Return(feed);
            });
    }
}