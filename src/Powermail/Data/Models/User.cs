using System.ComponentModel.DataAnnotations;

namespace Powermail.Data.Models;

public class User
{
    public int Id { get; set; }
    [Required]
    public string Email { get; init; } = string.Empty;
    [Required]
    public string TimeZone { get; set; } = "UTC";
    public string? Name { get; init; }
    public bool IsAdmin { get; set; }

    /// <summary>The time at which feed items were last sent</summary>
    public DateTime? FeedTimestamp { get; set; }
    /// <summary>The schedule of feed delivery</summary>
    public TimeSpan FeedInterval { get; set; } = TimeSpan.FromDays(1);

    public ICollection<UserFeed> Feeds { get; set; } = null!;
}