# Project TeamBuilder

Download self-contained binaries from [GitHub Actions](https://github.com/qhinf/project_teambuilder/actions/workflows/build.yml).

```
Description:
  Create teams based on students' preferred roles and projects

Usage:
  TeamBuilder <preferences file> [options]

Arguments:
  <preferences file>  Path to an Excel or CSV file containing student preferences

Options:
  --version                                Show version information
  -?, -h, --help                           Show help and usage information
  --no-header                              The preferences file does not contain a header row [default: False]
  --name-column <name-column>              Index of the column in the preferences file where the student's name is recorded [default: 4]
  --pref-start-column <pref-start-column>  Index of the column in the preferences file where the columns containing preferences start [default: 5]
  --min-team-size <min-team-size>          Minimum size of a team [default: 4]
  --max-team-size <max-team-size>          Maximum size of a team [default: 6]
  --minimize-team-count                    Minimize the number of teams (after optimizing students' preferences) [default: False]
  --solve-timeout <solve-timeout>          Maximum duration for running the solver in each round (in ms) [default: 10000]
```

The preferences file should be an Excel (.xlsx or .xls) or CSV (.csv) file with rows for each student, like this:

| A    | B                 | C                 | D                 | E           | F             | G               | H            | I               | J         | K            |
| ---- | ----------------- | ----------------- | ----------------- | ----------- | ------------- | --------------- | ------------ | --------------- | --------- | ------------ |
| Id   | Start time        | Finish time       | E-mail            | Name        | Role 1        | Project 1       | Role 2       | Project 2       | Role 3    | Project 3    |
| 1    | 1-1-1970 00:00:00 | 1-1-1970 00:12:34 | test@example.com  | Anne Test   | Product owner | Platformer game | Developer    | Platformer game | Developer | Some website |
| 2    | 1-1-1970 00:00:00 | 1-1-1970 00:12:34 | other@example.com | Peter Other | Scrum master  | Some website    | Scrum master | Platformer game |           |              |

By default, the tool looks for the students name in the column with index 4 (E in Excel) and preferences starting in column 5 (F), which is the format used when preferences are submitted using Microsoft Forms. This can be configured using the `--name-column` and `--pref-start-column` options for other formats. Students are not required to submit the same number of preferences, but each preference should consist of both a role and a project.
