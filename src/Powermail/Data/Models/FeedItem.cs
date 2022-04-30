using System.ComponentModel.DataAnnotations;

namespace Powermail.Data.Models;

public class FeedItem
{
    public int Id { get; set; }
    public int FeedId { get; set; }
    [Required]
    public string Url { get; set; } = string.Empty;
    [Required]
    public string Title { get; set; } = string.Empty;
    [Required]
    public DateTime Timestamp { get; set; } = DateTime.MinValue.ToUniversalTime();
    [Required]
    public string InternalId { get; set; } = string.Empty;
}