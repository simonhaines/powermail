namespace Powermail.Activities;

public interface IActivity
{
    /// <summary>
    /// Execute the activity.
    /// </summary>
    /// <param name="period">The time before this activity is next scheduled.</param>
    /// <param name="token">A cancellation token.</param>
    /// <returns>A Task that can be awaited.</returns>
    Task Execute(TimeSpan period, CancellationToken token);
}