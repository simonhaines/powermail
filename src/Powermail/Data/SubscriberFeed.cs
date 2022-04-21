using LiteDB;

namespace Powermail.Data;

public class SubscriberFeed
{
    public ObjectId Id { get; init; } = ObjectId.Empty;
    public ObjectId SubscriberId { get; init; } = ObjectId.Empty;
    public ObjectId FeedId { get; init; } = ObjectId.Empty;
    
    /// <summary>Whether this feed is discoverable in the subscriber's profile</summary>
    public bool IsPrivate { get; set; }
}