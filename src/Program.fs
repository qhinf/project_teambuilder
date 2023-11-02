open Flips
open Flips.Types
open Flips.SliceMap

type Student = Student of string
type Role = ScrumMaster | ProductOwner | Developer
    with static member Values = [ ScrumMaster; ProductOwner; Developer ]
type Project = Project of string
type Assignment = Assignment of Role * Project

type Result =
    | Solved of totalNthPreference: float * Map<Student, Assignment>
    | Infeasible of string
    | Unbounded of string
    | Unknown of string

let maxTeamSize = 6
let minTeamSize = 3

let solverSettings = {
    SolverType = SolverType.CBC
    MaxDuration = 10_000L
    WriteLPFile = None
    WriteMPSFile = None
}

// Students get to state their preference for a combined role and project, for
// example if they want to be a product owner for one project, but just a
// developer for another
let studentPreferences =
    [ Student "1", [ Assignment (ScrumMaster, Project "1"); Assignment (ProductOwner, Project "2") ]
      Student "2", [ Assignment (ProductOwner, Project "1"); Assignment (Developer, Project "2") ]
      Student "3", [ Assignment (ScrumMaster, Project "2"); Assignment (ScrumMaster, Project "1") ] ]
    |> Map.ofList

let projects = 
    studentPreferences 
    |> Map.values 
    |> Seq.collect (List.map (fun (Assignment (_, project)) -> project))
    |> Seq.distinct
    |> Seq.toList

let assignments =
    List.allPairs Role.Values projects 
    |> List.map Assignment

let students =
    studentPreferences
    |> Map.keys
    |> Seq.toList

// To give ourselves more alternatives beyond what the student submitted, we
// create a cartesian product of the roles and projects listed by the student.
// Ideally, a valid assignment could be found before these are needed, but if we
// do get this deep into their preferences, the student is at least constrained
// to the roles and projects they have indicated an interest in, albeit not in
// the combinations they specified
let extendPreferences preferenceMap =
    preferenceMap |> Map.map (fun _ preferences ->
        let roles = preferences |> List.map (fun (Assignment (role, _)) -> role)
        let projects = preferences |> List.map (fun (Assignment (_, project)) -> project)
        let extension =
            List.allPairs roles projects
            |> List.map Assignment
            |> List.filter (fun pref -> not <| List.contains pref preferences)
        preferences @ extension
    )

let extendedPreferences = extendPreferences studentPreferences

let roleIs role = Where (fun (Assignment (r, _)) -> r = role)
let roleIn roleSet = Where (fun (Assignment (r, _)) -> Set.contains r roleSet)
let projectIs project = Where (fun (Assignment (_, p)) -> p = project)
let projectIn projectSet = Where (fun (Assignment (_, p)) -> Set.contains p projectSet)

let findSolution (studentPreferences: Map<Student, Assignment list>) =
    // Recursively try to find a solution where each student is assigned a role
    // and project, within the given constraints. Each iteration, we add one
    // more alternative of the given preferences, until we can find a solution.
    // This means that if there is a solution possible with everyone's top 2,
    // this is preferred to a solution where a single student gets their fourth
    // preferred option and all others their first.
    let rec findSolution maxNthPreference (studentPreferences: Map<Student, Assignment list>) =
        // The decision is to map each student to one assignment (role and
        // project), so there is a boolean decision for each combination of
        // student and role+project
        let studentAssignment =
            DecisionBuilder "StudentAssignment" {
                for student in students do
                    for assignment in assignments ->
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
                    sum studentAssignment[All, projectIs project] >== float minTeamSize * teamsForProject
            }

            // Teams have a maximum size, similar to the minimum size
            yield! ConstraintBuilder "MaximumTeamSize" {
                for project in projects ->
                    let teamsForProject = sum studentAssignment[All, Assignment (ScrumMaster, project)]
                    sum (studentAssignment[All, projectIs project]) <== float maxTeamSize * teamsForProject
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
                if List.length studentPreferences[student] >= maxNthPreference + 1 then
                    // If there are enough preferences, take the top n for this iteration
                    let topNPreferences = studentPreferences[student] |> List.take (maxNthPreference + 1) |> Set.ofList
                    yield Constraint.create $"WithinPreferences_%A{student}" 
                        (sum studentAssignment[student, In topNPreferences] >== 1.)
        }

        // We optimize for students to get their highest possible preference, so
        // we optimize for the minimal total index of the chosen preferences.
        let totalNthPreference =
            [ for student in students do
                let maxN = min maxNthPreference (List.length studentPreferences[student] - 1)
                // For each of a students preferences, multiply that index by
                // the decision if they get that assignment. This results in the
                // index of their chosen preferred assignment, as all other
                // assignments are decided as 0
                for n = 0 to maxN do
                    yield studentAssignment[student, studentPreferences[student][n]] * float n
                // If we needed more alternatives than the student has given, we
                // also go through all other possible assignments (since we
                // discarded the constraints for this student) and mark the one
                // they got with the index one beyond the maximum index of their
                // preference list
                if maxN < maxNthPreference then
                    let nonPreferredAssignments =
                        assignments |> List.filter (fun ass -> not <| List.contains ass studentPreferences[student])
                    for assignment in nonPreferredAssignments do
                        yield studentAssignment[student, assignment] * float (maxN + 1) ]
            |> List.sum

        let minimizeTotalNthPreference = Objective.create "MinimizeTotalNthPreference" Minimize totalNthPreference

        let model =
            Model.create minimizeTotalNthPreference
            |> Model.addConstraints constraints

        let result = Solver.solve solverSettings model

        match result with
        | Optimal solution ->
            let totalNthPreference = Objective.evaluate solution minimizeTotalNthPreference
            let assignments =
                [ for student in students do
                    for assignment in assignments do
                        if 
                            studentAssignment[student, assignment]
                            |> LinearExpression.OfDecision
                            |> Solution.evaluate solution
                            = 1.
                        then yield (student, assignment) ]
                |> Map.ofList
            Solved (totalNthPreference, assignments)
        | SolveResult.Infeasible _ when maxNthPreference < 10 -> 
            printfn $"Infeasible for maxNthPreference={maxNthPreference}, trying with +1"
            findSolution (maxNthPreference + 1) studentPreferences
        | SolveResult.Infeasible message -> 
            Infeasible message
        | SolveResult.Unbounded message -> 
            Unbounded message
        | SolveResult.Unknown message -> 
            Unknown message
    
    findSolution 0 studentPreferences

match findSolution extendedPreferences with
| Solved (totalNthPreference, assignments) ->
    printfn "Total nth preference: %f" totalNthPreference
    printfn "Average nth preference: %f" (totalNthPreference / float (List.length students))

    Map.toSeq assignments
    |> Seq.map (fun (student, Assignment (role, project)) -> project, role, student)
    |> Seq.sort
    |> Seq.iter (fun (project, role, student) -> printfn $"{project} {role}: {student}")
    
| result -> printfn $"Unable to solve. Error: %A{result}"
