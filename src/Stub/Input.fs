namespace SamyTheSalmon

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.Application.WinForms

open OpenTK.Input

module Input =

    // state of input controls
    type State = 
        { 
            w : bool
            a : bool
            s : bool 
            d : bool
            space : bool
            enter : bool
            esc   : bool  
            mouse : V2i         // raw mouse position
            mouseDelta : V2i    // raw mouse delta
         }  

    // key bindings
    let grab (win : GameWindow) (s : Option<State>) =

        let k = win.Keyboard
        let mousePos = Mouse.GetState() |> (fun m -> m.X, m.Y) |> V2i
        
        { 
            w = k.IsDown(Keys.W) |> Mod.force
            a = k.IsDown(Keys.A) |> Mod.force
            s = k.IsDown(Keys.S) |> Mod.force
            d = k.IsDown(Keys.D) |> Mod.force
            space = k.IsDown(Keys.Space) |> Mod.force
            enter = k.IsDown(Keys.Enter) |> Mod.force
            esc   = k.IsDown(Keys.Escape)|> Mod.force 
            mouse = mousePos
            mouseDelta = 
                match win.Focused, s with
                    | true, Some s -> mousePos - s.mouse
                    | _ -> V2i.OO
        }