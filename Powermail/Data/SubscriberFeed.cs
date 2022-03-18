using LiteDB;

namespace Powermail.Data;

public class SubscriberFeed
{
    public ObjectId Id { get; set; }
    public ObjectId SubscriberId { get; set; }
    public ObjectId FeedId { get; set; }
    
    /// <summary>Whether this feed is discoverable in the subscriber's profile</summary>
    public bool IsPrivate { get; set; }
    
    /// <summary>The time at which items from this feed were sent to the subscriber</summary>
    public DateTimeOffset Timestamp { get; set; }
}