
namespace Powermail.Data.Models;

public class UserFeed
{
    public int Id { get; set; }
    public int UserId { get; set; }
    public int FeedId { get; set; }

    /// <summary>The user-defined name for this feed</summary>
    public string? Name { get; set; }
    /// <summary>Whether this feed is discoverable in the user's profile</summary>
    public bool IsPrivate { get; set; }

    public User User { get; set; } = null!;
    public Feed Feed { get; set; } = null!;
}