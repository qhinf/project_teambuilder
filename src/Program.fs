open System
open System.IO
open System.Text
open ExcelDataReader
open Flips
open Flips.SliceMap
open Flips.Types
open FSharp.SystemCommandLine

type Student = Student of string with
    member this.Name = match this with Student name -> name
    override this.ToString() = $"Student %s{this.Name}"

type Role = ScrumMaster | ProductOwner | Developer with
    override this.ToString() = 
        match this with 
        | ScrumMaster -> "Scrum master" 
        | ProductOwner -> "Product owner" 
        | Developer -> "Developer"
    static member Values = [ ScrumMaster; ProductOwner; Developer ]
    static member TryParse (str: string) =
        match str.ToLowerInvariant() with
        | "scrum master" | "scrummaster" | "sm" -> Some ScrumMaster
        | "product owner" | "productowner" | "po" -> Some ProductOwner
        | "developer" | "dev" -> Some Developer
        | _ -> None

type Project = Project of string with
    member this.Name = match this with Project name -> name
    override this.ToString() = $"Project %s{this.Name}"

type Assignment = Assignment of Role * Project with
    member this.Role = match this with Assignment (role, _) -> role
    member this.Project = match this with Assignment (_, project) -> project
    override this.ToString() = $"%O{this.Role} in %O{this.Project}"

// Students get to state their preference for a combined role and project, for
// example if they want to be a product owner for one project, but just a
// developer for another
type Preferences = Map<Student, Assignment list>

module TeamBuilder =
    type Settings =
        { MinTeamSize: int
          MaxTeamSize: int
          MinimizeTeamCount: bool
          SolverSettings: SolverSettings }

    let private getProjects (preferences: Preferences) = 
        preferences 
        |> Map.values 
        |> Seq.collect (List.map (fun (Assignment (_, project)) -> project))
        |> Seq.distinct
        |> Seq.toList

    let private getAllAssignments projects =
        List.allPairs Role.Values projects 
        |> List.map Assignment

    let private getStudents (preferences: Preferences) =
        preferences
        |> Map.keys
        |> Seq.toList

    // To give ourselves more alternatives beyond what the student submitted, we
    // create a cartesian product of the roles and projects listed by the
    // student. Ideally, a valid assignment could be found before these are
    // needed, but if we do get this deep into their preferences, the student is
    // at least constrained to the roles and projects they have indicated an
    // interest in, albeit not in the combinations they specified
    let private extendPreferences (preferences: Preferences) =
        preferences |> Map.map (fun _ preferences ->
            let roles = preferences |> List.map (fun (Assignment (role, _)) -> role)
            let projects = preferences |> List.map (fun (Assignment (_, project)) -> project)
            let extension =
                List.allPairs roles projects
                |> List.map Assignment
                |> List.filter (fun pref -> not <| List.contains pref preferences)
            preferences @ extension
        )

    let inline private roleIs role = Where (fun (Assignment (r, _)) -> r = role)
    let inline private projectIs project = Where (fun (Assignment (_, p)) -> p = project)

    let rec private buildTeams' maxNthPreference (settings: Settings) (preferences: Map<Student, Assignment list>) (students: Student list) (projects: Project list) (allAssignments: Assignment list) =
        // The decision is to map each student to one assignment (role and
        // project), so there is a boolean decision for each combination of
        // student and role+project
        let studentAssignment =
            DecisionBuilder "StudentAssignment" {
                for student in students do
                    for assignment in allAssignments ->
                            Boolean
            } |> SMap2.ofSeq
        
        let constraints = seq {
            // Students should only have a single assignment, so the sum of all
            // decisions per student should be 1
            yield! ConstraintBuilder "StudentHasOneAssignment" {
                for student in students ->
                    sum (studentAssignment[student, All]) == 1.
            }

            // We allow projects to be picked by 0 or more teams, but each team needs
            // one scrum master and one product owner, so a project needs an equal
            // amount of scrum masters and product owners
            yield! ConstraintBuilder "ProjectHasAsManyScrumMastersAsProductOwners" {
                for project in projects ->
                    sum studentAssignment[All, Assignment (ScrumMaster, project)] 
                    == sum studentAssignment[All, Assignment (ProductOwner, project)]
            }

            // Teams have a minimum size, so the sum of all decisions within a
            // project (for all roles) should be at least the minimum size.
            // Since there may be multiple teams for a project, the minimum size
            // is multiplied with the number of teams for that project
            yield! ConstraintBuilder "MinimumTeamSize" {
                for project in projects ->
                    // Count the number of scrum masters in this project as a proxy for
                    // the number of teams in this project
                    let teamsForProject = sum studentAssignment[All, Assignment (ScrumMaster, project)]
                    sum studentAssignment[All, projectIs project] >== float settings.MinTeamSize * teamsForProject
            }

            // Teams have a maximum size, similar to the minimum size
            yield! ConstraintBuilder "MaximumTeamSize" {
                for project in projects ->
                    let teamsForProject = sum studentAssignment[All, Assignment (ScrumMaster, project)]
                    sum (studentAssignment[All, projectIs project]) <== float settings.MaxTeamSize * teamsForProject
            }

            // Each student should get an assignment that is within in their top
            // preferences
            for student in students do
                // If the given preferences were not sufficient for a solution
                // until now and the student has given no more alternatives,
                // then their preferences are ignored. (This is to prevent
                // students only giving a single option and thus guaranteeing
                // themselves that position, which can lead to conflicts if two
                // students choose the same.)
                if List.length preferences[student] >= maxNthPreference + 1 then
                    // If there are enough preferences, take the top n for this iteration
                    let topNPreferences = preferences[student] |> List.take (maxNthPreference + 1) |> Set.ofList
                    yield Constraint.create $"WithinPreferences_%O{student}" 
                        (sum studentAssignment[student, In topNPreferences] >== 1.)
        }

        // We optimize for students to get their highest possible preference, so
        // we optimize for the minimal total index of the chosen preferences.
        let totalNthPreference =
            [ for student in students do
                let maxN = min maxNthPreference (List.length preferences[student] - 1)
                // For each of a students preferences, multiply that index by
                // the decision if they get that assignment. This results in the
                // index of their chosen preferred assignment, as all other
                // assignments are decided as 0
                for n = 0 to maxN do
                    yield studentAssignment[student, preferences[student][n]] * float n
                // If we needed more alternatives than the student has given, we
                // also go through all other possible assignments (since we
                // discarded the constraints for this student) and mark the one
                // they got with the index one beyond the maximum index of their
                // preference list
                if maxN < maxNthPreference then
                    let nonPreferredAssignments =
                        allAssignments |> List.filter (fun ass -> not <| List.contains ass preferences[student])
                    for assignment in nonPreferredAssignments do
                        yield studentAssignment[student, assignment] * float (maxN + 1) ]
            |> List.sum

        let minimizeTotalNthPreference = Objective.create "MinimizeTotalNthPreference" Minimize totalNthPreference

        let numberOfTeams = sum studentAssignment[All, roleIs ScrumMaster]
        let minimizeNumberOfTeams = Objective.create "MinimizeNumberOfTeams" Minimize numberOfTeams

        let model =
            Model.create minimizeTotalNthPreference
            |> if settings.MinimizeTeamCount
               then Model.addObjective minimizeNumberOfTeams
               else id
            |> Model.addConstraints constraints

        let result = Solver.solve settings.SolverSettings model

        match result with
        | Optimal solution ->
            Ok [ for student in students do
                    for assignment in allAssignments do
                        if studentAssignment[student, assignment]
                            |> LinearExpression.OfDecision
                            |> Solution.evaluate solution
                            = 1.
                        then yield (assignment, student) ]
        | Infeasible _ when maxNthPreference < 10 -> 
            buildTeams' (maxNthPreference + 1) settings preferences students projects allAssignments
        | Infeasible message -> 
            Error $"No feasable solution found within all students' top 10 preferences. Maybe some constraints are too strict. Solver message: {message}"
        | Unbounded message -> 
            Error $"The model is unbounded. This should not happen. Solver message: {message}"
        | Unknown message -> 
            Error $"An unknown error occurred. Solver message: {message}"

    let buildTeams (settings: Settings) (preferences: Preferences) =
        // Recursively try to find a solution where each student is assigned a role
        // and project, within the given constraints. Each iteration, we add one
        // more alternative of the given preferences, until we can find a solution.
        // This means that if there is a solution possible with everyone's top 2,
        // this is preferred to a solution where a single student gets their fourth
        // preferred option and all others their first.
        let students = getStudents preferences
        let projects = getProjects preferences
        let allAssignments = getAllAssignments projects
        buildTeams' 0 settings preferences students projects allAssignments

module Params =
    let preferencesFile = Input.Argument<FileInfo>(
        "preferences file",
        "Path to an Excel or CSV file containing student preferences" )
    let noHeader = Input.Option<bool>(
        [ "--no-header" ],
        false,
        "The preferences file does not contain a header row" )
    let nameColumn = Input.Option<int>(
        [ "--name-column" ],
        4,
        "Index of the column in the preferences file where the student's name is recorded" )
    let prefStartColumn = Input.Option<int>(
        [ "--pref-start-column" ],
        5,
        "Index of the column in the preferences file where the columns containing preferences start" )
    let minTeamSize = Input.Option<int>(
        [ "--min-team-size" ],
        4,
        "Minimum size of a team" )
    let maxTeamSize = Input.Option<int>(
        [ "--max-team-size" ],
        6,
        "Maximum size of a team" )
    let minimizeTeamCount = Input.Option<bool>(
        [ "--minimize-team-count" ],
        false,
        "Minimize the number of teams (after optimizing students' preferences)" )
    let solveTimeout = Input.Option<int64>(
        [ "--solve-timeout" ],
        10_000L,
        "Maximum duration for running the solver in each round (in ms)" )

let run (ctx: CommandLine.Invocation.InvocationContext) =
    try
        let file = Params.preferencesFile.GetValue ctx
        let noHeader = Params.noHeader.GetValue ctx
        let nameColumn = Params.nameColumn.GetValue ctx
        let prefStartColumn = Params.prefStartColumn.GetValue ctx

        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
        use fileStream = file.Open(FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        use fileReader = 
            if file.Extension = ".csv"
            then ExcelReaderFactory.CreateCsvReader(fileStream)
            else ExcelReaderFactory.CreateReader(fileStream)

        let fileDataSet = fileReader.AsDataSet(
            ExcelDataSetConfiguration
                ( FilterSheet = (fun tableReader sheetIndex -> sheetIndex = 0),
                  ConfigureDataTable = fun tableReader -> 
                    ExcelDataTableConfiguration
                        ( UseHeaderRow = not noHeader ) ) 
        )

        let preferences = 
            [ for row in fileDataSet.Tables[0].Rows do
                let items = row.ItemArray
                let student = Student (items[nameColumn] :?> string)
                let preferences = 
                    items[prefStartColumn..]
                    |> Array.choose (function
                        | :? String as str when not (String.IsNullOrWhiteSpace str) -> 
                            Some str
                        | _ -> 
                            None
                    )
                    |> Array.chunkBySize 2
                    |> Array.map (function 
                        | [| role; project |] -> 
                            match Role.TryParse role with
                            | Some role -> Assignment (role, Project project)
                            | None -> raise (InvalidDataException $"\"{role}\" is not a valid role")
                        | _ ->
                            raise (InvalidDataException $"Uneven amount of preference columns found. Please check that the correct starting column is set and that the data is complete.")
                    )
                    |> Array.distinct
                    |> Array.toList
                student, preferences ]
            |> Map.ofList

        let settings =
            { TeamBuilder.Settings.MinTeamSize = 
                Params.minTeamSize.GetValue ctx
              TeamBuilder.Settings.MaxTeamSize = 
                Params.maxTeamSize.GetValue ctx
              TeamBuilder.Settings.MinimizeTeamCount = 
                Params.minimizeTeamCount.GetValue ctx
              TeamBuilder.Settings.SolverSettings = 
                { Settings.basic with MaxDuration = Params.solveTimeout.GetValue ctx } }
        
        match TeamBuilder.buildTeams settings preferences with
        | Ok result ->
            let nthPreferences =
                [ for assignment, student in result do
                    let nthPref = 
                        preferences[student] 
                        |> List.tryFindIndex (fun a -> a = assignment)
                        |> Option.defaultWith (fun () -> List.length preferences[student])
                    student, nthPref + 1 ]
                |> Map.ofList

            let avgNthPreference = 
                nthPreferences |> Map.values |> Seq.map float |> Seq.average
            let worstPreference =
                nthPreferences |> Map.values |> Seq.max

            printfn $"Average preference: %.2f{avgNthPreference}"
            printfn $"Worst preference: %d{worstPreference}"

            result 
            |> List.groupBy (fun (assignment, _) -> assignment.Project)
            |> List.iter (fun (project, assignments) ->
                printfn ""
                printfn $"- %s{project.Name}"
                for assignment, student in assignments |> List.sort do
                    let nthPref = nthPreferences[student]
                    printfn $"  - %O{assignment.Role}: %s{student.Name} (Pref: %d{nthPref})"
            ) 

            0
        | Error message ->
            printfn "%s" message
            1
    with e ->
        printfn "%s" e.Message
        1

[<EntryPoint>]
let main args =
    rootCommand args {
        description "Create teams based on students' preferred roles and projects"
        inputs (Input.Context())
        setHandler run
        addInputs 
            [ Params.preferencesFile
              Params.noHeader
              Params.nameColumn
              Params.prefStartColumn
              Params.minTeamSize
              Params.maxTeamSize
              Params.minimizeTeamCount
              Params.solveTimeout ]
    }
