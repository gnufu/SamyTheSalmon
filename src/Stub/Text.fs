namespace SamyTheSalmon

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.Text

module Text =
    open Aardvark.SceneGraph
    open System

    // sg for text rendering
    let private textSg (s : float) (t : V2d) (c : IMod<string>) =
        let f = Font("Consolas", FontStyle.Regular)
        Sg.text f C4b.White c |> Sg.scale s |> Sg.translate t.X t.Y 0.0

    // GUI for samys health
    let health (state : State) =
        
        let health = 
            adaptive {
                let! m = state.samy.health |> Mod.map (floor >> string)
                let txt = "health: " + m
                return txt
            }
        textSg 0.05 (V2d(-0.97, 0.9)) health

    // GUI for player score
    let score (state : State) =
        let score =
            adaptive {
                let! s = state.score
                return "Score: " + s.ToString()
            }
        textSg 0.05 (V2d(0.7, 0.9)) score
       
    // player highscore for finish screen
    let highscore (hs : IMod<string>) (s : IMod<V2i>) =
        let trafo =
            adaptive {
                let! s = s
                return M33d.Translation(V2d(((double)s.X / 2.5), ((double)s.Y / 2.0)))
            }
        textSg 0.08 (V2d(-0.2, 0.0)) hs

    // playername input at starting screen
    let playername (pn : IModRef<string>) (s : IMod<V2i>) =
        let trafo =
            adaptive {
                let! s = s
                return M33d.Translation(V2d((s.X / 2), (s.Y / 2)))
            }
        textSg 0.08 (V2d(0.0, -0.05)) pn
        