using LiteDB;

namespace Powermail.Data;

public class Subscriber
{
    public ObjectId Id { get; init; } = ObjectId.Empty;
    public string Email { get; init; } = string.Empty;
    public string? Name { get; init; }
}