using Microsoft.Extensions.Options;

namespace Powermail.Storage;

public class FileSystemConfiguration
{
    public string Path { get; set; } = null!;
}

public class FileSystem : IStorage
{
    private readonly IOptions<FileSystemConfiguration> configuration;

    public FileSystem(IOptions<FileSystemConfiguration> configuration)
    {
        this.configuration = configuration;
    }
    
    public Stream GetStream(string name)
    {
        var path = Path.Combine(configuration.Value.Path, name);
        return new FileStream(path, FileMode.OpenOrCreate, FileAccess.Write);
    }
}