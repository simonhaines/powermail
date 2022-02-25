namespace Powermail.Storage;

public interface IStorage
{
    Stream GetStream(string name);
}