tracking
========

Run tracking.Tracking

The data is is the data directory.

The output is report.html in the root directory.

Folders in the data directory are 'projects'. The naming scheme is projectName.projectId.
If no projectId is specified, it'll be the same as the project name. The projectId is used for inter-project dependencies.

In each project directory there are 'status' files. Each file is the project status on a particular date.

When tracking.Tracking is run, it'll load up all the statuses for all projects, do some basic validation, and then produce a
single HTML file with a section per project. each section will show the project completion, epics not started, completed and
in progress, a burndown chart and (still to do) the relationships between projects and the degree of completion of things each project
depends on, from the point of view of that project.
