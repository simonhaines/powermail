using System.Net;
using LiteDB;

namespace Powermail.Data;

public class Feed
{
    public ObjectId Id { get; set; } = ObjectId.Empty;
    public string Url { get; set; } = string.Empty;
    public DateTime? Timestamp { get; set; }
    public string? Name { get; set; }
    
    public int ErrorCount { get; set; }
    public HttpStatusCode? LastAccessCode { get; set; }
}