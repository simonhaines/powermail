using LiteDB;

namespace Powermail.Data;

public class Data : IDisposable
{
    private readonly LiteDatabase db;

    public Data(string filename)
        => db = new LiteDatabase(filename);

    public Data(Stream stream)
        => db = new LiteDatabase(stream);

    public ILiteCollection<Subscriber> Subscribers => db.GetCollection<Subscriber>();
    public ILiteCollection<Feed> Feeds => db.GetCollection<Feed>();
    public ILiteCollection<FeedItem> FeedItems => db.GetCollection<FeedItem>();
    public ILiteCollection<SubscriberFeed> SubscriberFeeds => db.GetCollection<SubscriberFeed>();
    public ILiteCollection<SubscriberSchedule> SubscriberSchedules => db.GetCollection<SubscriberSchedule>();

    public void Dispose()
        => db.Dispose();
}