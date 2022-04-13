using LiteDB;

namespace Powermail.Data;

public class SubscriberSchedule
{
    public ObjectId Id { get; set; }
    public ObjectId SubscriberId { get; set; }
    
    /// <summary>The time at which feed items were last sent to the subscriber</summary>
    public DateTimeOffset? FeedDelivery { get; set; }
    /// <summary>The schedule of feed delivery</summary>
    public TimeSpan? FeedDeliveryInterval { get; set; }
}