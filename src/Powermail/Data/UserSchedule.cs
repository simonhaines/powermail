using LiteDB;

namespace Powermail.Data;

public class UserSchedule
{
    public ObjectId Id { get; init; } = ObjectId.Empty;
    public ObjectId UserId { get; init; } = ObjectId.Empty;
    
    /// <summary>The time at which feed items were last sent to the user</summary>
    public DateTime? FeedTimestamp { get; set; }

    /// <summary>The schedule of feed delivery</summary>
    public TimeSpan FeedInterval { get; set; } = TimeSpan.Zero;
}