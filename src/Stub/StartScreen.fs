namespace SamyTheSalmon

open System.IO
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.Rendering.GL

open OpenTK.Input

module StartScreen =

    let mutable private name = ""

    let mutable private listeners = []

    // removes the last character from the given string
    let private removeLast (str : string) =
        if str.IsEmpty () then "" else str.Length - 1 |> str.Remove

    // initializes the start screen
    let init (r : Runtime) (win : GameWindow) (s : State) =

        // setup callbacks for playername input
        name <- s.playername |> Mod.force

        listeners <- [
            win.KeyPress.Subscribe (fun k -> name <- name + string k.KeyChar)
            win.KeyDown.Subscribe (fun k -> if k.Key = Key.BackSpace then name <- name |> removeLast)
        ]

        // set render task & change status
        RenderTasks.startScreen r win s
        { s with status = ShowingInstructions }


    // updates the start screen
    let update (input : Input.State) (s : State) = 

        // update player name
        if not input.enter then
            s.playername <== name + "_"
            s
        else
            // remove callbacks
            listeners |> List.iter (fun x -> x.Dispose ())

            // remove cursor from name and save to file
            s.playername <== (s.playername |> Mod.force |> removeLast)
            File.WriteAllText(@"..\resources\playername.txt", s.playername |> Mod.force)

            // change status
            { s with status = InitializingGame }