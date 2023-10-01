module GameOfLife.Client.Main

open Elmish
open Bolero
open Bolero.Html
open GameOfLife.Client.Core


/// The Elmish application's model.
type Model =
    { generation: int
      world: World }

let initModel =
    { generation = 0
      world = glider }

/// The Elmish application's update messages.
type Message =
    | NextGeneration

let update message model =
    match message with
    | NextGeneration ->
        { model with
            generation = model.generation + 1
            world = nextGeneration model.world }

let view model dispatch =
    concat {
        h1 { $"Generation {model.generation}" }
        button { 
            on.click (fun _ -> dispatch NextGeneration) 
            "Next generation"
        }
        table {
            for row in 0..height do
                tr {
                    for column in 0..width do
                        let position = { X = column; Y = row }
                        let isAlive = isAlive model.world position
                        td { 
                            attr.``class`` (if isAlive then "alive" else "dead")
                            "o"
                        }
                }
        }
    }
    

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
