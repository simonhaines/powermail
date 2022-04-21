using System.Resources;
using Org.BouncyCastle.Asn1.Cms;
using Superpower;
using Superpower.Model;
using Superpower.Parsers;

namespace Powermail.Parsers;

public class Feed
{
    public TextSpan Url { get; set; }
}

public static class Feeds
{
    private static readonly TextParser<Unit> WordBoundary
        = Character.WhiteSpace
            .Or(Character.AnyChar
                .Where(char.IsPunctuation))
            .Many()
            .Where(chars => chars.Any(char.IsWhiteSpace))
            .Value(Unit.Value)
            .Named("word boundary");

    private static TextParser<TextSpan> Word(string value)
        => Span.EqualToIgnoreCase(value).Named(value);
    private static TextParser<Unit?> OptionalWord(string value)
        => Word(value).IgnoreThen(WordBoundary).Optional();

    private static readonly TextParser<Unit> Preamble
        = Word("subscribe")
            .IgnoreThen(WordBoundary)
            .IgnoreThen(OptionalWord("me"))
            .IgnoreThen(OptionalWord("to"))
            .IgnoreThen(OptionalWord("the"))
            .IgnoreThen(OptionalWord("feed"))
            .Value(Unit.Value);

    public static TextParser<Feed> Subscribe
        = Preamble
            .IgnoreThen(Common.Url)
            .Then(url => Parse.Return(new Feed { Url = url }));
}