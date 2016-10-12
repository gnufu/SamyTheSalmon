module AardvarkTips

open Aardvark.Application
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Base.Rendering

(* 
discussion:
using adaptive functions to model game logics is a nice idea but it turned out that the primities, such as
integrate and stepTime etc have some fundamental flaws.
Especially for complex user interactions such in games i'd recommend to use vanilla game loop style instead
of highly composable abstract functional programming vodoo.
for turn based games i tried a vanilla F# implementation here: https://github.com/vrvis/aardvark.rendering/blob/master/src/Demo/Examples/Tic-Tac-Toe.fs
for real time games i'd suggest a similar approach, i.e. instead of having a scene graph describing the scene, i'd 
rather go for a simple domain model, which in turn gets translated into a scene graph (or render objects directly).
e.g.
*)

module SamyLogics =

    type Heightfield = int
    type OtherProperties = Things of unit
    type GameEntity = Obstacle of V3d * OtherProperties 
                    | Riverbed of Heightfield
                    | Fischfutter of IModRef<V3d>

    type Sammy = { position : IModRef<V3d>; health : IModRef<double> }
    type GameStatus = ShowingInstructions | Running | EndOfGame
    type GameState = { entities : list<GameEntity>; sammy : Sammy; gameStatus : GameStatus }
    type GameInput = { startGame : bool; forwardPressed : bool } 

    let mutable currentGameState = 
        // read level data
        { entities = []; sammy = { position = Mod.init V3d.OOO; health = Mod.init 1.0 }; gameStatus = ShowingInstructions }

    let (<==) m v = transact (fun () -> Mod.change m v)
    let interactGame (g : GameState) (gameInputs : GameInput ) =
        match g.gameStatus with
            | ShowingInstructions  -> 
                if gameInputs.startGame then { g with gameStatus = Running } else g
            | Running ->
                // complex game logics here
                if gameInputs.forwardPressed then 
                    g.sammy.position <== g.sammy.position.Value + V3d.OOI
                    g
                // if normal then do collision detection, if fly god mode then just ignore collisions etc...
                else failwith ""
            | EndOfGame -> 
                g

    let gameLoop () =
        while true do // conceptually
            // grab keys
            let input : GameInput = failwith ""
            currentGameState <- interactGame currentGameState input
            // maybe sleep, simluation time etc...

module Questions =
(*
Q: 
ich hab das Spawnen von den Obstacles soweit fertig. Probleme macht hier nur das Blending bezgl. T
ransparenz. Aus irgendeinem Grund werden die Obstacles stets nach dem Wasser gerendert, 
was natürlich dann nicht richtig funktioniert. Ich vermute es liegt daran, dass die Obstacles 
mit Sg.dynamic im Scenegraph eingefügt werden, das Wasser hingegen "statisch" ist. Kann man da irgendwas machen?
Btw, gibt es irgendeine Möglichkeit Exceptions in einem adaptive-Block abzufangen oder 
zumindest herauszufinden welche geworfen wurde? 

A: weil objekte sortiert werden um effizient gerrendert zu werden, siehe: http://www.vrvis.at/publications/pdfs/PB-VRVis-2015-015.pdf
Lösung dazu: passes - mit passes kann man explizite reihenfolge festlegen
*)
    let p = Rendering.RenderPass.main
    let test = Sg.pass p (failwith "") // assigns a int64 to a scene graph part, dictating a render order (increasing pass)


// 2) try finally in adaptive
//    let a = 
//        adaptive {
//            try
//                failwith ""
//            with e -> printfn "SDF"
//        }  // ERROR: adaptive blocks do not support try finally. if you need try finally there's a trick

    let testA() =
        try
            failwith ""
        with e -> printfn "failed"

    let blu = 
        adaptive {
            let a = testA()
            return 1
        }  // having a let binding (function not living in adaptive world) lets you use try finally within pure (non adaptive)
           // parts of an adaptive computation.

(*
3) Compile von shader
Übrigens, gibt es noch ein Problem bei den Obstacles. 
Da diese erst nach und nach zum World-Sg hinzugefügt werden, 
verzögert sich auch das Compilen der Shader. Wenn wir nun verschiedene Objekte haben, 
werden deren Shader erst compiled wenn sie zum ersten Mal spawnen, was irgendwann im Spielverlauf passieren kann. 
Das führt wiederum zu deutlichen Freezes. Gibt es vll. 
irgendeine Möglichkeit das Laden / Initialisieren eines Sg irgendwie zu forcieren?
A:
*)
    let test2 ()=
        let runtime : IRuntime = failwith ""
        let win : IRenderControl = failwith ""
        let compiledShader = 
            // prepares effect for specific framebuffer signature (requires runtime to have opengl context etc)
            runtime.PrepareEffect(win.FramebufferSignature, [ DefaultSurfaces.diffuseTexture |> toEffect ]) :> ISurface // upcast necessary since upcasts not implicit in f#
        let appliedCompiledShader = Sg.surface (Mod.constant compiledShader) // apply using Sg.surface
        ()

        (* but how to convert current GameContent to have runtime? one simple hack would be: 
        *)
    module MyNewContent =
        let mutable obstacle : ISg = Unchecked.defaultof<_>

        let initContent (r : IRuntime) (win : IRenderControl) =
            let compiledShader = failwith ""
            obstacle <- Sg.surface compiledShader (failwith "isg")

    (* actually nicer (more functional) would be to create the window and simply create game content afterwards instead of 
    globally in a module *)

