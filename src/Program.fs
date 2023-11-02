open Flips
open Flips.Types
open Flips.SliceMap

type Role =
    | ScrumMaster
    | ProductOwner
    | Developer

    static member Values = [ ScrumMaster; ProductOwner; Developer ]

type Student = Student of string
type Project = Project of string

type Preferences =
    { Roles: Role list
      Projects: Project list }

let maxTeamSize = 6
let minTeamSize = 3

let studentPreferences =
    [ Student "1", { Roles = [ ScrumMaster; Developer ]; Projects = [ Project "1"; Project "2" ]}
      Student "2", { Roles = [ ProductOwner; Developer ]; Projects = [ Project "1"; Project "2" ] }
      Student "3", { Roles = [ ScrumMaster; Developer ]; Projects = [ Project "2"; Project "1" ]} ]
    |> Map.ofList

let shortlist = 
    studentPreferences 
    |> Map.values 
    |> Seq.collect (fun pref -> pref.Projects)
    |> Seq.distinct
    |> Seq.toList

let students =
    studentPreferences
    |> Map.keys
    |> Seq.toList

let studentHasRoleInProject =
    DecisionBuilder "StudentHasRoleInProject" {
        for student in students do
            for role in Role.Values do
                for project in shortlist ->
                    Boolean
    } |> SMap3.ofSeq

let totalHappiness =
    [ for student in students ->    
        let topPreferredRole = studentPreferences[student].Roles |> List.head
        let happyWithRole = sum studentHasRoleInProject[student, topPreferredRole, All]
        let topPrefferedProject = studentPreferences[student].Projects |> List.head
        let happyWithProject = sum studentHasRoleInProject[student, All, topPrefferedProject]
        happyWithRole + happyWithProject ]
    |> List.sum

let unhappyStudents =
    [ for student in students ->    
        let leastPreferredRole = studentPreferences[student].Roles |> List.last
        let leastPrefferedProject = studentPreferences[student].Projects |> List.last
        studentHasRoleInProject[student, leastPreferredRole, leastPrefferedProject]
        |> LinearExpression.OfDecision ]
    |> List.sum

let maximizeTotalHappiness = Objective.create "MaximizeTotHappiness" Maximize totalHappiness
let minimizeUnhappyStudents = Objective.create "MinimizeUnhappyStudents" Minimize unhappyStudents

let constraints = seq {
    yield! ConstraintBuilder "StudentHasOneRoleInOneProject" {
        for student in students ->
            sum (studentHasRoleInProject[student, All, All]) == 1.
    }

    // We allow projects to be picked by 0 or more teams, but each team needs
    // one scrum master and one product owner, so a project needs an equal
    // amount of scrum masters and product owners
    yield! ConstraintBuilder "ProjectHasAsManyScrumMastersAsProductOwners" {
        for project in shortlist ->
            sum studentHasRoleInProject[All, ScrumMaster, project] == sum studentHasRoleInProject[All, ProductOwner, project]
    }

    yield! ConstraintBuilder "MinimumTeamSize" {
        for project in shortlist ->
            // Count the number of scrum masters in this project as a proxy for
            // the number of teams in this project
            let teamsForProject = sum studentHasRoleInProject[All, ScrumMaster, project]
            sum (studentHasRoleInProject[All, All, project]) >== float minTeamSize * teamsForProject
    }

    yield! ConstraintBuilder "MaximumTeamSize" {
        for project in shortlist ->
            let teamsForProject = sum studentHasRoleInProject[All, ScrumMaster, project]
            sum (studentHasRoleInProject[All, All, project]) <== float maxTeamSize * teamsForProject
    }

    yield! ConstraintBuilder "ProjectInPreferences" {
        for student in students ->
            let preferredProjects = set studentPreferences[student].Projects
            sum (studentHasRoleInProject[student, All, In preferredProjects]) >== 1.
    }

    yield! ConstraintBuilder "RoleInPreferences" {
        for student in students ->
            let preferredRoles = set studentPreferences[student].Roles
            sum (studentHasRoleInProject[student, In preferredRoles, All]) >== 1.
    }
}

let model =
    Model.create maximizeTotalHappiness
    |> Model.addObjective minimizeUnhappyStudents
    |> Model.addConstraints constraints

let settings = {
    SolverType = SolverType.CBC
    MaxDuration = 10_000L
    WriteLPFile = None
    WriteMPSFile = None
}

let result = Solver.solve settings model

match result with
| Optimal solution ->
    printfn "Total happiness: %f" (Objective.evaluate solution maximizeTotalHappiness)
    printfn "Unhappy students: %f" (Objective.evaluate solution minimizeUnhappyStudents)

    let projectRoleStudent =
        seq {
            for student in students do
                for project in shortlist do
                    for role in Role.Values do
                        if
                            studentHasRoleInProject[student, role, project]
                            |> LinearExpression.OfDecision
                            |> Solution.evaluate solution
                            = 1.0
                        then
                            yield (project, role, student)
        }

    for (project, role, student) in projectRoleStudent |> Seq.sort do
        printfn $"{project} {role}: {student}"
    
| _ -> printfn $"Unable to solve. Error: %A{result}"
