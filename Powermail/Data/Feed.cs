using System.Net;
using LiteDB;

namespace Powermail.Data;

public class Feed
{
    public ObjectId Id { get; set; }
    public string Url { get; set; }
    public string? Name { get; set; }
    public DateTimeOffset? Timestamp { get; set; }
    
    public int ErrorCount { get; set; }
    public HttpStatusCode? LastAccessCode { get; set; }
}