using LiteDB;

namespace Powermail.Data;

public class Data : IDisposable
{
    private readonly LiteDatabase db;

    public Data(string filename)
        => db = new LiteDatabase(filename);

    public Data(Stream stream)
        => db = new LiteDatabase(stream);

    public ILiteCollection<Feed> Feeds => db.GetCollection<Feed>();
    public ILiteCollection<FeedItem> FeedItems => db.GetCollection<FeedItem>();

    public void Dispose()
        => db.Dispose();
}