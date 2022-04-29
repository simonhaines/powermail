using LiteDB;
using Microsoft.Extensions.DependencyInjection;

namespace Powermail.Data;

public sealed class Data : IDisposable
{
    private readonly LiteDatabase db;

    public Data(string filename)
        => db = new LiteDatabase(filename);

    public Data(Stream stream)
        => db = new LiteDatabase(stream);

    public ILiteCollection<User> Users => db.GetCollection<User>();
    public ILiteCollection<Feed> Feeds => db.GetCollection<Feed>();
    public ILiteCollection<FeedItem> FeedItems => db.GetCollection<FeedItem>();
    public ILiteCollection<UserFeed> UserFeeds => db.GetCollection<UserFeed>();
    public ILiteCollection<UserSchedule> UserSchedules => db.GetCollection<UserSchedule>();

    public ILiteStorage<ObjectId> InboxStorage => db.GetStorage<ObjectId>("inbox", "inboxChunks");

    public void Dispose()
        => db.Dispose();
}
