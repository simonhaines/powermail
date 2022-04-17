using LiteDB;

namespace Powermail.Data;

public class SubscriberSchedule
{
    public ObjectId Id { get; init; } = ObjectId.Empty;
    public ObjectId SubscriberId { get; init; } = ObjectId.Empty;
    
    /// <summary>The time at which feed items were last sent to the subscriber</summary>
    public DateTimeOffset FeedTimestamp { get; set; } = DateTimeOffset.MinValue;

    /// <summary>The schedule of feed delivery</summary>
    public TimeSpan FeedInterval { get; set; } = TimeSpan.Zero;
}