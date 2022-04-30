using System.Collections.ObjectModel;
using System.ComponentModel.DataAnnotations;
using System.Net;

namespace Powermail.Data.Models;

public class Feed
{
    public int Id { get; set; }
    [Required]
    public string Url { get; set; } = string.Empty;
    public DateTime? Timestamp { get; set; }
    public string? Name { get; set; }
    
    public int ErrorCount { get; set; }
    public HttpStatusCode? LastAccessCode { get; set; }

    public ICollection<FeedItem> Items { get; set; } = null!;
}