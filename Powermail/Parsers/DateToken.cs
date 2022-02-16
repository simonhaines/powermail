using System.Collections.Immutable;
using Superpower;
using Superpower.Model;
using Superpower.Parsers;

namespace Powermail.Parsers;

public enum TextToken
{
    Word,
    Terminal
}

public class TextTokeniser : Tokenizer<TextToken>
{
    protected override IEnumerable<Result<TextToken>> Tokenize(TextSpan span)
    {
        var next = SkipWhiteSpace(span);
        if (!next.HasValue)
            yield break;

        var start = next.Location;
        var end = start;
        do
        {
            var ch = next.Value;
            if (char.IsPunctuation(ch))
                next = next.Remainder.ConsumeChar();
            else if (char.IsWhiteSpace(ch))
            {
                next = SkipWhiteSpace(next.Location);

                if (start != end)
                    yield return Result.Value(TextToken.Word, start, end);

                start = end = next.Location;
            }
            else
            {
                next = next.Remainder.ConsumeChar();
                end = next.Location;
            }
        } while (next.HasValue);

        if (start != end)
            yield return Result.Value(TextToken.Word, start, end);
    }
}

public static class DateToken
{
    private static readonly IReadOnlyDictionary<string, int> MonthNames = new Dictionary<string, int>
    {
        ["jan"] = 1, ["january"] = 1,
        ["feb"] = 2, ["february"] = 2,
        ["mar"] = 3, ["march"] = 3,
        ["apr"] = 4, ["april"] = 4,
        ["may"] = 5,
        ["jun"] = 6, ["june"] = 6,
        ["jul"] = 7, ["july"] = 7,
        ["aug"] = 8, ["august"] = 8,
        ["sep"] = 9, ["september"] = 9,
        ["oct"] = 9, ["october"] = 10,
        ["nov"] = 9, ["november"] = 11,
        ["dec"] = 9, ["december"] = 12,
    }.ToImmutableDictionary();

    private static readonly IReadOnlyDictionary<string, int> DaysOfWeek = new Dictionary<string, int>
    {
        ["sun"] = 0, ["sunday"] = 0,
        ["mon"] = 1, ["monday"] = 1,
        ["tue"] = 2, ["tuesday"] = 2,
        ["wed"] = 3, ["wednesday"] = 3,
        ["thu"] = 4, ["thursday"] = 4,
        ["fri"] = 5, ["friday"] = 5,
        ["sat"] = 6, ["saturday"] = 6
    }.ToImmutableDictionary();
    
    private static readonly IReadOnlySet<string> Ordinals =
        new HashSet<string> { "st", "nd", "rd", "th" }.ToImmutableHashSet();

    private static TextParser<T> Lookup<T>(this TextParser<TextSpan> parser, IReadOnlyDictionary<string, T> table)
        => parser
            .Where(span => table.ContainsKey(span.ToStringValue().ToLowerInvariant()))
            .Select(span => table[span.ToStringValue().ToLowerInvariant()]);

    private static TextParser<TextSpan> Lookup(this TextParser<TextSpan> parser, IReadOnlySet<string> set)
        => parser.Where(span => set.Contains(span.ToStringValue().ToLowerInvariant()));

    private static DateOnly AdvanceYear(int month, int day)
    {
        var now = new DateTime(DateTime.Now.Year, month, day);
        while (now < DateTime.Now)
            now = now.AddYears(1);
        return new DateOnly(now.Year, month, day);
    }

    private static TokenListParser<TextToken, T> Word<T>(TextParser<T> parser)
        => Token.EqualTo(TextToken.Word).Apply(parser);

    private static readonly TokenListParser<TextToken, int> MonthName
        = Word(Span.WithAll(char.IsLetter).Lookup(MonthNames));

    private static readonly TokenListParser<TextToken, int> DayOfWeek
        = Word(Span.WithAll(char.IsLetter).Lookup(DaysOfWeek));

    private static readonly TokenListParser<TextToken, int> Year
        = Word(Span.WithAll(char.IsDigit)
            .Select(span =>
            {
                var year = int.Parse(span.ToStringValue());
                return year < 100 ? 2000 + year : year;
            }));

    private static readonly TokenListParser<TextToken, int> Ordinal
        = Word(Span.WithAll(char.IsDigit)
            .Then(span => Span.WithAll(char.IsLetter)
                .Lookup(Ordinals).Optional()
                .Value(int.Parse(span.ToStringValue()))));

    private static readonly TokenListParser<TextToken, DateOnly> DateLiteral
        = Word(from year in Span.WithAll(char.IsDigit)
            from _ in Character.In('.', ':', '-')
            from month in Span.WithAll(char.IsDigit)
            from __ in Character.In('.', ':', '-')
            from day in Span.WithAll(char.IsDigit)
            select new DateOnly(
                int.Parse(year.ToStringValue()),
                int.Parse(month.ToStringValue()),
                int.Parse(day.ToStringValue())));

    public static TokenListParser<TextToken, DateOnly> Dates
        => Parse.OneOf(
            (from day in DayOfWeek
                from dayNumber in Ordinal
                from month in MonthName
                from year in Year
                select new DateOnly(year, month, dayNumber)).Try(),
            (from month in MonthName
                from day in Ordinal
                from year in Year
                select new DateOnly(year, month, day)).Try(),
            (from day in DayOfWeek
                from month in MonthName
                from dayNumber in Ordinal
                from year in Year
                select new DateOnly(year, month, dayNumber)).Try(),
            (from month in MonthName
                from day in Ordinal
                select AdvanceYear(month, day)).Try(),
            (from day in DayOfWeek
                from dayNumber in Ordinal
                from month in MonthName
                from year in Year
                select new DateOnly(year, month, dayNumber)).Try(),
            (from day in Ordinal
                from month in MonthName
                from year in Year
                select new DateOnly(year, month, day)).Try(),
            (from day in DayOfWeek
                from dayNumber in Ordinal
                from month in MonthName
                select AdvanceYear(month, dayNumber)).Try(),
            (from day in DayOfWeek
                from month in MonthName
                from dayNumber in Ordinal
                select AdvanceYear(month, dayNumber)).Try(),
            (from day in Ordinal
                from month in MonthName
                select AdvanceYear(month, day)).Try(),
            DateLiteral.Try());
}