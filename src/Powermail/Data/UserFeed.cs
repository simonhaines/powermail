using LiteDB;

namespace Powermail.Data;

public class UserFeed
{
    public ObjectId Id { get; init; } = ObjectId.Empty;
    public ObjectId UserId { get; init; } = ObjectId.Empty;
    public ObjectId FeedId { get; init; } = ObjectId.Empty;
    public string? Name { get; set; }
    
    /// <summary>Whether this feed is discoverable in the user's profile</summary>
    public bool IsPrivate { get; set; }
}