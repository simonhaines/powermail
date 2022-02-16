using System.Collections.Immutable;
using System.Globalization;
using System.Security.AccessControl;
using Superpower;
using Superpower.Model;
using Superpower.Parsers;

namespace Powermail.Parsers;

public static class Date
{
    private static readonly Dictionary<string, int> MonthNames = new()
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
    };

    private static readonly Dictionary<string, int> DaysOfWeek = new()
    {
        ["sun"] = 0, ["sunday"] = 0,
        ["mon"] = 1, ["monday"] = 1,
        ["tue"] = 2, ["tuesday"] = 2,
        ["wed"] = 3, ["wednesday"] = 3,
        ["thu"] = 4, ["thursday"] = 4,
        ["fri"] = 5, ["friday"] = 5,
        ["sat"] = 6, ["saturday"] = 6
    };

    private static readonly HashSet<string> Ordinals = new() { "st", "nd", "rd", "th" };

    private static TextParser<T> Lookup<T>(this TextParser<TextSpan> parser, Dictionary<string, T> table)
        => parser
            .Where(span => table.ContainsKey(span.ToStringValue().ToLowerInvariant()))
            .Select(span => table[span.ToStringValue().ToLowerInvariant()]);

    private static TextParser<TextSpan> Lookup(this TextParser<TextSpan> parser, HashSet<string> set)
        => parser.Where(span => set.Contains(span.ToStringValue().ToLowerInvariant()));

    static TextParser<TextSpan> WordBoundary()
        => Span.WithAll(ch => char.IsWhiteSpace(ch) || char.IsPunctuation(ch)).Named("word boundary");

    static DateOnly AdvanceYear(int month, int day)
    {
        var now = new DateTime(DateTime.Now.Year, month, day);
        while (now < DateTime.Now)
            now = now.AddYears(1);
        return new DateOnly(now.Year, month, day);
    }
    
    static TextParser<int> MonthName()
        => Span.WithAll(char.IsLetter).Lookup(MonthNames);
    
    public static TextParser<int> DayOfWeek()
        => Span.WithAll(char.IsLetter).Lookup(DaysOfWeek);

    static TextParser<int> Year()
        => Span.WithAll(char.IsDigit)
            .Select(span =>
            {
                var year = int.Parse(span.ToStringValue());
                return year < 100 ? 2000 + year : year;
            });

    static TextParser<int> Ordinal()
        => Span.WithAll(char.IsDigit)
            .Then(span => Span.WithAll(char.IsLetter)
                .Lookup(Ordinals).Optional()
                .Value(int.Parse(span.ToStringValue())));

    public static TextParser<DateOnly> Dates
        => Parse.OneOf(
            (from day in DayOfWeek()
                from _ in WordBoundary()
                from dayNumber in Ordinal()
                from __ in WordBoundary()
                from month in MonthName()
                from ___ in WordBoundary()
                from year in Year()
                select new DateOnly(year, month, dayNumber)).Try(),
            (from day in DayOfWeek()
                from _ in WordBoundary()
                from month in MonthName()
                from __ in WordBoundary()
                from dayNumber in Ordinal()
                from ___ in WordBoundary()
                from year in Year()
                select new DateOnly(year, month, dayNumber)).Try(),
            (from month in MonthName()
                from _ in WordBoundary()
                from day in Ordinal()
                select AdvanceYear(month, day)).Try()
        );
}