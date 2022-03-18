using LiteDB;

namespace Powermail.Data;

public class FeedItem
{
    public ObjectId Id { get; set; }
    public ObjectId FeedId { get; set; }
    public string Title { get; set; }
    public string Url { get; set; }
    public DateTimeOffset Timestamp { get; set; }
}