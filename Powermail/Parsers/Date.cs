using System.Security.AccessControl;
using Superpower;
using Superpower.Model;
using Superpower.Parsers;

namespace Powermail.Parsers;

public static class Date
{
    static DateOnly AdvanceYear(int month, int day)
    {
        var now = new DateTime(DateTime.Now.Year, month, day);
        while (now < DateTime.Now)
            now = now.AddYears(1);
        return new DateOnly(now.Year, month, day);
    }

    static TextParser<Unit> CommaWhitespace()
        => Span.WhiteSpace.Or(Span.EqualTo(',')).IgnoreMany();

    static TextParser<int> MonthName()
        => Span.EqualToIgnoreCase("january").Value(1)
            .Or(Span.EqualToIgnoreCase("jan").Value(1))
            .Or(Span.EqualToIgnoreCase("february").Value(2))
            .Or(Span.EqualToIgnoreCase("feb").Value(2))
            .Or(Span.EqualToIgnoreCase("march").Value(3))
            .Or(Span.EqualToIgnoreCase("mar").Value(3))
            .Or(Span.EqualToIgnoreCase("april").Value(4))
            .Or(Span.EqualToIgnoreCase("apr").Value(4))
            .Or(Span.EqualToIgnoreCase("may").Value(5))
            .Or(Span.EqualToIgnoreCase("june").Value(6))
            .Or(Span.EqualToIgnoreCase("jun").Value(6))
            .Or(Span.EqualToIgnoreCase("july").Value(7))
            .Or(Span.EqualToIgnoreCase("jul").Value(7))
            .Or(Span.EqualToIgnoreCase("august").Value(8))
            .Or(Span.EqualToIgnoreCase("aug").Value(8))
            .Or(Span.EqualToIgnoreCase("september").Value(9))
            .Or(Span.EqualToIgnoreCase("sep").Value(9))
            .Or(Span.EqualToIgnoreCase("october").Value(10))
            .Or(Span.EqualToIgnoreCase("oct").Value(10))
            .Or(Span.EqualToIgnoreCase("november").Value(11))
            .Or(Span.EqualToIgnoreCase("nov").Value(11))
            .Or(Span.EqualToIgnoreCase("december").Value(12))
            .Or(Span.EqualToIgnoreCase("dec").Value(12));

    static TextParser<int> DayOfWeek()
        => Span.EqualToIgnoreCase("sunday").Value(0)
            .Or(Span.EqualToIgnoreCase("sun").Value(0))
            .Or(Span.EqualToIgnoreCase("monday").Value(1))
            .Or(Span.EqualToIgnoreCase("mon").Value(1))
            .Or(Span.EqualToIgnoreCase("tuesday").Value(2))
            .Or(Span.EqualToIgnoreCase("tue").Value(2))
            .Or(Span.EqualToIgnoreCase("wednesday").Value(3))
            .Or(Span.EqualToIgnoreCase("wed").Value(3))
            .Or(Span.EqualToIgnoreCase("thursday").Value(4))
            .Or(Span.EqualToIgnoreCase("thu").Value(4))
            .Or(Span.EqualToIgnoreCase("friday").Value(5))
            .Or(Span.EqualToIgnoreCase("fri").Value(5))
            .Or(Span.EqualToIgnoreCase("saturday").Value(6))
            .Or(Span.EqualToIgnoreCase("sat").Value(6));

    static TextParser<int> Year()
        => Common.Digits(4)
            .Or(Common.Digits(2).Select(year => 2000 + year));

    static TextParser<int> Ordinal()
        => Character.Digit
            .AtLeastOnce()
            .Then(digits =>
                Span.EqualTo("st")
                    .Or(Span.EqualTo("nd"))
                    .Or(Span.EqualTo("rd"))
                    .Or(Span.EqualTo("th"))
                    .Optional()
                    .Select(_ => int.Parse(digits)));
    
    public static IEnumerable<TextParser<DateOnly>> Dates
        => new[]
        {
            from day in DayOfWeek()
            from _ in CommaWhitespace()
            from dayNumber in Ordinal()
            from __ in CommaWhitespace()
            from month in MonthName()
            from ___ in CommaWhitespace()
            from year in Year()
            select new DateOnly(year, month, dayNumber),
            from day in DayOfWeek()
            from _ in CommaWhitespace()
            from month in MonthName()
            from __ in CommaWhitespace()
            from dayNumber in Ordinal()
            from ___ in CommaWhitespace()
            from year in Year()
            select new DateOnly(year, month, dayNumber)
        };

    public static TextParser<DateOnly> DateParser
        => Parse.OneOf(
            (from day in DayOfWeek()
                from _ in CommaWhitespace()
                from dayNumber in Ordinal()
                from __ in CommaWhitespace()
                from month in MonthName()
                from ___ in CommaWhitespace()
                from year in Year()
                select new DateOnly(year, month, dayNumber)).Try(),
            (from day in DayOfWeek()
                from _ in CommaWhitespace()
                from month in MonthName()
                from __ in CommaWhitespace()
                from dayNumber in Ordinal()
                from ___ in CommaWhitespace()
                from year in Year()
                select new DateOnly(year, month, dayNumber)).Try(),
            (from month in MonthName()
                from _ in CommaWhitespace()
                from day in Ordinal()
                select AdvanceYear(month, day)).Try()
            
        );
}