// For more information see https://aka.ms/fsharp-console-apps

//Question 1
printfn"PART A"
// PART A : DEFINE THE MODEL 

type Coach = {
    Name: string
    FormerPlayer: bool
}
type Stats = {
    Wins: int
    Losses: int
}
type Team = {
    Name: string
    Coach: Coach
    Stats: Stats
}

let createCoach name formerPlayer = 
    { Name = name; FormerPlayer = formerPlayer }
let createStats wins losses = 
    { Wins = wins; Losses = losses }
let createTeam name coach stats = 
    { Name = name; Coach = coach; Stats = stats }

let coach = createCoach "Larry Bird" true
let stats = createStats 58 24
let team = createTeam "Indiana Pacers" coach stats
printfn "%A" team

printfn""
printfn"PART B"

// PART 2 : CREATE A LIST OF 5 TEAMS

let coach1 = createCoach "Steve Kerr" true
let stats1 = createStats 58 24
let team1 = createTeam "Golden State Warriors" coach1 stats1

let coach2 = createCoach "Mike Budenholzer" true
let stats2 = createStats 56 26
let team2 = createTeam "Milwaukee Bucks" coach2 stats2

let coach3 = createCoach "Nick Nurse" true
let stats3 = createStats 49 33
let team3 = createTeam "Toronto Raptors" coach3 stats3

let coach4 = createCoach "Frank Vogel" true
let stats4 = createStats 42 40
let team4 = createTeam "Los Angeles Lakers" coach4 stats4

let coach5 = createCoach "Doc Rivers" true
let stats5 = createStats 48 34
let team5 = createTeam "Philadelphia 76ers" coach5 stats5


let teams = [team1; team2; team3; team4; team5]
let printTeam team =
    printfn "Team: %s" team.Name
    printfn "Coach: %s (Former Player: %b)" team.Coach.Name team.Coach.FormerPlayer
    printfn "Wins: %d, Losses: %d\n" team.Stats.Wins team.Stats.Losses

teams |> List.iter printTeam


printfn""
printfn""
printfn"PART C"

// PART C : Filtering the List

let isSuccessful team = team.Stats.Wins > team.Stats.Losses
let successfulTeams = teams |> List.filter isSuccessful

successfulTeams |> List.iter printTeam

printfn""
printfn""
printfn"PART D"

// PART D : MAPPING THE LIST 
let calculateSuccessPercentage team =
    let totalGames = float (team.Stats.Wins + team.Stats.Losses)
    let successPercentage = (float team.Stats.Wins) / totalGames * 100.0
    (team.Name, successPercentage)
let successPercentages = teams |> List.map calculateSuccessPercentage

successPercentages |> List.iter (fun (teamName, percentage) ->
    printfn "Team: %s, Success Percentage: %.2f%%" teamName percentage)

printfn""
printfn""
printfn""
printfn""
printfn""
printfn"QUESTION 2"
// QUESTION 2
printfn"PART 1"
//PART 1 : Define the Cuisine

type Cuisine =
  | Korean
  | Turkish
let describeCuisine x =
  match x with
  | Korean -> printfn "The selected cuisine is Korean."
  | Turkish -> printfn "The selected cuisine is Turkish."

let myCuisine1 = Korean
describeCuisine myCuisine1
let myCuisine2 = Turkish
describeCuisine myCuisine2

printfn""
printfn""
printfn"PART 2"

//PART 2 : Define the Movie Genres
type MovieType =
  | Regular
  | IMAX
  | DBOX
  | RegularWithSnacks
  | IMAXWithSnacks
  | DBOXWithSnacks

let describeMovieExperience movie =
  match movie with
  | Regular -> printfn "This is a regular movie experience."
  | IMAX -> printfn "This is an IMAX movie experience."
  | DBOX -> printfn "This is a DBOX movie experience."
  | RegularWithSnacks -> printfn "This is a regular movie experience with snacks."
  | IMAXWithSnacks -> printfn "This is an IMAX movie experience with snacks."
  | DBOXWithSnacks -> printfn "This is a DBOX movie experience with snacks."

let movie1 = Regular
let movie2 = IMAXWithSnacks

describeMovieExperience movie1
describeMovieExperience movie2


printfn""
printfn""
printfn"PART 3"

//PART 3 : Define the Activity
type genre =
    | Action
    | Comedy
    | Drama
    | Horror
    | Romance

type Activity =
    | BoardGame
    | Chill
    | Movie of genre
    | Restaurant of Cuisine
    | LongDrive of int * float 

let describeActivity = function
    | BoardGame -> printfn "Let's play a board game."
    | Chill -> printfn "Let's chill out."
    | Movie genre -> printfn "Let's watch a %A movie." genre
    | Restaurant cuisine -> printfn "Let's go to a %A restaurant." cuisine
    | LongDrive (km, fuelCost) -> printfn "Let's take a long drive for %i km. Fuel charge is %.2f per km." km fuelCost

describeActivity (Movie Horror)
describeActivity (Restaurant Korean)
describeActivity (LongDrive (150, 1.2))


printfn""
printfn""
printfn"PART 4"

//PART 4 : Calculate the Budget

let calculateBudget activity =
    match activity with
    | BoardGame | Chill -> 0 
    | Movie genre ->
        let baseCost =
            match genre with
            | Action | Comedy | Drama -> 12
            | Horror | Romance -> 12
        baseCost
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> 70
        | Turkish -> 65
    | LongDrive (km, fuelCost) -> int (float km * fuelCost) 

let budget1 = calculateBudget (BoardGame)
let budget2 = calculateBudget (Movie Comedy)
let budget3 = calculateBudget (Restaurant Korean)
let budget4 = calculateBudget (LongDrive (150, 1.2))

printfn "Budget for BoardGame: %i CAD" budget1
printfn "Budget for Movie: %i CAD" budget2
printfn "Budget for Restaurant: %i CAD" budget3
printfn "Budget for LongDrive: %i CAD" budget4



