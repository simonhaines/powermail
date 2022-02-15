using Superpower;
using Superpower.Model;
using Superpower.Parsers;

namespace Powermail.Parsers;

public static class Common
{
    public static TextParser<int> Digits(int count)
        => Character.Digit.Repeat(count).Select(chars => int.Parse(chars));
    
    public static TextParser<int> OneOrTwoDigits()
        => Digits(1).Or(Digits(2));

    public static TextParser<int> TwoOrFourDigits()
        => Digits(2).Or(Digits(4));

}
