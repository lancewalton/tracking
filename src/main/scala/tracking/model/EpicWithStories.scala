package tracking.model

case class EpicWithStories(epic: Epic, completedStories: Int, unstartedStories: Int, storiesInProgress: Int)
