using LiteDB;

namespace Powermail.Data;

public class User
{
    public ObjectId Id { get; init; } = ObjectId.Empty;
    public string Email { get; init; } = string.Empty;
    public string? Name { get; init; }
    public string TimeZone { get; set; } = "UTC";
}