using LiteDB;

namespace Powermail.Data;

public class FeedItem
{
    public ObjectId Id { get; init; } = ObjectId.Empty;
    public ObjectId FeedId { get; init; } = ObjectId.Empty;
    public string Url { get; init; } = string.Empty;
    public string Title { get; init; } = string.Empty;
    public DateTimeOffset Timestamp { get; init; } = DateTimeOffset.MinValue;
}