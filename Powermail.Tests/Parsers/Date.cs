using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Powermail.Parsers;
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
            ["sunday, feb 24,   2013"] = new(2013, 2, 24),
            ["Sunday, Feb 24th, 2013"] = new(2013, 2, 24),
            ["saturday, feb 24, 2013"] = new(2013, 2, 24),
            ["february 24 2013"] = new(2013, 2, 24),
            ["feb   24,   2013"] = new(2013, 2, 24),
            ["saturday, jan 24"] = new(2023, 1, 24),
            ["march 23rd"] = new(2022, 3, 23),
            ["Sunday, 24th February, 2013"] = new(2013, 2, 24),
            ["saturday 24 feb 2013"] = new(2013, 2, 24),
            ["24 february 2013"] = new(2013, 2, 24),
            ["saturday 24 feb"] = new(2022, 2, 24),
            ["24th february"] = new(2022, 2, 24),
            ["3rd february"] = new(2023, 2, 3),
            ["2013-10-26"] = new(2013, 10, 26),
            ["2013.10.26"] = new(2013, 10, 26)
        });
    }

    [TestMethod]
    public void ParseDateTokens()
    {
        ParseTokens(new Dictionary<string, DateOnly>
        {
            ["sunday, feb 24,   2013"] = new(2013, 2, 24),
            ["Sunday, Feb 24th, 2013"] = new(2013, 2, 24),
            ["saturday, feb 24, 2013"] = new(2013, 2, 24),
            ["february 24 2013"] = new(2013, 2, 24),
            ["feb   24,   2013"] = new(2013, 2, 24),
            ["saturday, jan 24"] = new(2023, 1, 24),
            ["march 23rd"] = new (2022, 3, 23),
            ["Sunday, 24th February, 2013"] = new(2013, 2, 24),
            ["saturday 24 feb 2013"] = new(2013, 2, 24),
            ["24 february 2013"] = new(2013, 2, 24),
            ["saturday 24 feb"] = new(2022, 2, 24),
            ["24th february"] = new(2022, 2, 24),
            ["3rd february"] = new(2023, 2, 3),
            ["2013-10-26"] = new(2013, 10, 26),
            ["2013.10.26"] = new(2013, 10, 26)
        });
    }
    
    static void ParseTokens(Dictionary<string, DateOnly> tests)
    {
        foreach (var (key, value) in tests)
        {
            var tokens = new TextTokeniser().Tokenize(key);
            var result = DateToken.Dates.TryParse(tokens);
            if (!result.HasValue)
                Assert.Fail($"{key}: {result}");
        }
    }

    static void ParseEqual<T>(TextParser<T> parser, Dictionary<string, T> tests)
    {
        foreach (var (key, value) in tests) {
            var result = parser.TryParse(key);
            if (!result.HasValue)
                Assert.Fail($"{key}: {result}");
        }
    }
}