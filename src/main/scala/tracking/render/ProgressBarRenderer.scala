package tracking.render

object ProgressBarRenderer {
  def apply(completed: Int, inProgress: Int, notStarted: Int) = {
    val total = completed + inProgress + notStarted
    val completedPercentage = percentage(completed, total)
    val inProgressPercentage = percentage(inProgress, total)
    val unstartedPercentage = 100 - completedPercentage - inProgressPercentage

    <div class="progress">
      <div class="progress-bar progress-bar-success" style={ s"width: $completedPercentage%" }>
        <span>Completed</span>
      </div>
      <div class="progress-bar progress-bar-warning progress-bar" style={ s"width: $inProgressPercentage%}" }>
        <span>In Progress</span>
      </div>
      <div class="progress-bar progress-bar-danger" style={ s"width: $unstartedPercentage%" }>
        <span>Not Started</span>
      </div>
    </div>
  }
  
  private def percentage(numerator: Int, denominator: Int): Int = (numerator * 100) / denominator
}