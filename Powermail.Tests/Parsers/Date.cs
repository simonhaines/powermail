using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Superpower;

namespace Powermail.Tests.Parsers;

[TestClass]
public class Date
{
    [TestMethod]
    public void ParseDate()
    {
        ParseEqual(Powermail.Parsers.Date.Dates, new Dictionary<string, DateOnly>
        {
            ["sunday, feb 24, 2013"] = new(2013, 2, 24),
            ["sunday, feb 24th, 2013"] = new(2013, 2, 24)
        });

        ParseEqual(Powermail.Parsers.Date.DateParser, new Dictionary<string, DateOnly>
        {
            ["sunday, feb 24, 2013"] = new(2013, 2, 24),
            ["sunday, feb 24th, 2013"] = new(2013, 2, 24)
        });
    }

    static void ParseEqual<T>(TextParser<T> parser, Dictionary<string, T> tests)
    {
        foreach (var (key, value) in tests)
            Assert.AreEqual(value, parser.Parse(key));
    }

    static void ParseEqual<T>(IEnumerable<TextParser<T>> parsers, Dictionary<string, T> tests)
    {
        foreach (var (key, value) in tests)
        {
            foreach (var parser in parsers)
            {
                var result = parser.TryParse(key);
                if (result.HasValue)
                {
                    Assert.AreEqual(value, result.Value);
                    return;
                }
            }
            Assert.Fail();
        }
    }
}