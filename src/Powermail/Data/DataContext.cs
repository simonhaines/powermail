using Microsoft.EntityFrameworkCore;
using Powermail.Data.Models;

namespace Powermail.Data;

public class DataContext : DbContext
{
    public DbSet<User> Users { get; set; }
    public DbSet<UserFeed> UserFeeds { get; set; }
    
    public DbSet<Feed> Feeds { get; set; }
    public DbSet<FeedItem> FeedItems { get; set; }
    
    public DataContext(DbContextOptions<DataContext> options)
        : base(options)
    { }
}