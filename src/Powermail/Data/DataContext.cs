using Microsoft.EntityFrameworkCore;
using Powermail.Data.Models;

namespace Powermail.Data;

public class DataContext : DbContext
{
    public DbSet<User> Users { get; set; } = null!;
    public DbSet<UserFeed> UserFeeds { get; set; } = null!;
    
    public DbSet<Feed> Feeds { get; set; } = null!;
    public DbSet<FeedItem> FeedItems { get; set; } = null!;
    
    public DataContext(DbContextOptions<DataContext> options)
        : base(options)
    { }
}