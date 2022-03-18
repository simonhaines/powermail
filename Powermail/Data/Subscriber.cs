using LiteDB;

namespace Powermail.Data;

public class Subscriber
{
    public ObjectId Id { get; set; }
    public string Email { get; set; }
    public string Name { get; set; }
}