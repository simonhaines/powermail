using Superpower;
using Superpower.Model;

namespace Powermail.Parsers;

public static class Extensions
{
    public static TextParser<TextSpan> Until(this TextParser<char> self, TextParser<TextSpan> parser,
        Action<TextSpan> action)
    {
        return input =>
        {
            var next = input;
            while (!next.IsAtEnd)
            {
                var result = parser(next);
                if (result.HasValue)
                {
                    var preamble = input.Until(result.Location);
                    action(preamble);
                    return result;
                }

                var consumed = self(next);
                if (consumed.HasValue)
                    next = next.Skip(1);
                else
                    return Result.Empty<TextSpan>(input);
            }
            return Result.Empty<TextSpan>(input);
        };
    }
}