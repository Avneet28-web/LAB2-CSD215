// For more information see https://aka.ms/fsharp-console-apps
type Coach = {Name: string;FormerPlayer: bool}

type Stats = {Wins: int;Losses: int}

type Team = {Name: string;Coach: Coach;Stats: Stats}

let coach1 = { Name = "Steve Kerr"; FormerPlayer = true }
let coach2 = { Name = "Mike Budenholzer"; FormerPlayer = false }
let coach3 = { Name = "Eric Spoelstra"; FormerPlayer = false }
let coach4 = { Name = "Doc Rivers"; FormerPlayer = true }
let coach5 = { Name = "Gregg Popovich"; FormerPlayer = true }

let stats1 = { Wins = 55; Losses = 27 }
let stats2 = { Wins = 51; Losses = 31 }
let stats3 = { Wins = 53; Losses = 29 }
let stats4 = { Wins = 47; Losses = 35 }
let stats5 = { Wins = 42; Losses = 40 }

let team1 = { Name = "Golden State Warriors"; Coach = coach1; Stats = stats1 }
let team2 = { Name = "Milwaukee Bucks"; Coach = coach2; Stats = stats2 }
let team3 = { Name = "Miami Heat"; Coach = coach3; Stats = stats3 }
let team4 = { Name = "Philadelphia 76ers"; Coach = coach4; Stats = stats4 }
let team5 = { Name = "San Antonio Spurs"; Coach = coach5; Stats = stats5 }

let teams = [ team1; team2; team3; team4; team5 ]

let successfulTeams = teams|> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)

let successPercentage team =
    let totalGames = float (team.Stats.Wins + team.Stats.Losses)
    let percentage = (float team.Stats.Wins / totalGames) * 100.0
    percentage

let successPercentages = teams|> List.map (fun team -> (team.Name, successPercentage team))

printfn "Successful Teams:"
successfulTeams |> List.iter (fun team -> 
    printfn "%s: Wins = %d, Losses = %d" team.Name team.Stats.Wins team.Stats.Losses)

printfn "\nSuccess Percentages:"
successPercentages |> List.iter (fun (name, percentage) -> printfn "%s: %.2f%%" name percentage)



type Cuisine =
    | Korean
    | Turkish

type MovieType =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

type Activity =
    | BoardGame
    | Chill
    | Movie of MovieType
    | Restaurant of Cuisine
    | LongDrive of int * float

let calculateBudget (activity: Activity) : float =
    match activity with
    | BoardGame -> 0.0
    | Chill -> 0.0
    | Movie movieType ->
        match movieType with
        | Regular -> 12.0
        | IMAX -> 17.0
        | DBOX -> 20.0
        | RegularWithSnacks -> 17.0
        | IMAXWithSnacks -> 22.0
        | DBOXWithSnacks -> 25.0
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> 70.0
        | Turkish -> 65.0
    | LongDrive (kilometers, fuelCostPerKm) -> float kilometers * fuelCostPerKm

let budget1 = calculateBudget (Movie DBOXWithSnacks) // 25.0
let budget2 = calculateBudget (Restaurant Turkish) // 65.0
let budget3 = calculateBudget (LongDrive (100, 0.5)) // 50.0

printfn "\nBudget Examples:"
printfn "Movie DBOXWithSnacks: %.2f CAD" budget1
printfn "Restaurant Turkish: %.2f CAD" budget2
printfn "Long Drive (100 km, 0.5 CAD/km): %.2f CAD" budget3

