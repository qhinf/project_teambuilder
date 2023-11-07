open System
open System.IO

let studentCount = 30
let projectCount = 7
let minPreferences = 3
let maxPreferences = 6

let random = Random()

let roles = [| "Scrum master"; "Product owner"; "Developer" |]

let csvLines =
    [ for student = 1 to studentCount do
        [ yield $"S%02d{student}"
          for _ = 1 to random.Next(minPreferences, maxPreferences + 1) do
            yield roles[random.Next(roles.Length)]
            yield $"P%02d{random.Next(projectCount)}" ]
        |> String.concat ";" ]

File.WriteAllLines("testdata.csv", csvLines)
