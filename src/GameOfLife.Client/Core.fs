module GameOfLife.Client.Core

// The world is a 10x10 grid
let width = 10
let height = 10

// A position is a point on the grid
type Position = { X: int; Y: int }
// A world is a list of all the alive cells
type World = Position list


let isAlive (world: World) (position: Position) =
    List.exists (fun cell -> cell = position) world

let isEmpty (world: World) (position: Position) =
    not (isAlive world position)

/// Euclidean remainder, the proper modulo operation
let inline (%!) a b = (a % b + b) % b

let neighbors (position: Position) =
    // Wrap around the edges of the grid.
    let wrap (position: Position) =
        { X = ((position.X - 1) %! width) + 1; Y = ((position.Y - 1) %! height) + 1 }
    
    [
        { X = position.X - 1; Y = position.Y - 1 };
        { X = position.X; Y = position.Y - 1 };
        { X = position.X + 1; Y = position.Y - 1 };
        { X = position.X - 1; Y = position.Y };
        { X = position.X + 1; Y = position.Y };
        { X = position.X - 1; Y = position.Y + 1 };
        { X = position.X; Y = position.Y + 1 };
        { X = position.X + 1; Y = position.Y + 1 };
    ] |> List.map wrap

let liveNeighbors (world: World) (position: Position) =
    neighbors position
    |> List.filter (isAlive world)
    |> List.length

// Any live cell with two or three live neighbors survives
let survivors (world: World) =
    world
    |> List.filter (fun position ->
        let liveNeighbors = liveNeighbors world position
        liveNeighbors = 2 || liveNeighbors = 3
    )

// Any dead cell with three live neighbors becomes a live cell
let births (world: World) =
    world
    |> List.collect neighbors
    |> List.filter (isEmpty world)
    |> List.filter (fun position ->
        liveNeighbors world position = 3
    )

// All other live cells die in the next generation. Similarly, all other dead cells stay dead
let nextGeneration (world: World) =
    world
    |> survivors
    |> List.append (births world)
    |> List.distinct


// Initial game seed
let glider =
    [ { X = 4; Y = 2 }
      { X = 2; Y = 3 }
      { X = 4; Y = 3 }
      { X = 3; Y = 4 }
      { X = 4; Y = 4 } ]