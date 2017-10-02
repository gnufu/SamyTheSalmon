namespace SamyTheSalmon

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Rendering.Text

module Text =
    open Aardvark.SceneGraph

    // nvg for text rendering
    let private textNvg (c : IMod<string>) =
        let f = Font("Consolas", FontStyle.Regular)
        Sg.text f C4b.White c |> Sg.scale 0.08
        
        //c
        //|> Nvg.text
        //|> Nvg.systemFont "Consolas" FontStyle.Bold
        //|> Nvg.fillColor ~~C4f.White
        //|> Nvg.fontSize ~~30.0

    // feedback msg for debug keys
    let feedbackMsg (runtime : IRuntime) (size : IMod<V2i>) =
        let msg = 
            adaptive {
                let! m = Config.Application.feedbackMsg
                return m
            }
        
        let msgNvg = textNvg msg
        msgNvg
        //let text = Nvg.ContextApplicator(runtime.GetNanoVgContext(), Mod.constant msgNvg)
        
        //// gray box around msg
        //let overall = ref Box2d.Invalid
        //let rect =
        //    text.LocalBoundingBox() 
        //       |> Mod.map (fun bb -> 
        //            RoundedRectangle(Box2d.FromMinAndSize(bb.Min, bb.Max.X - bb.Min.X, bb.Max.Y - bb.Min.Y),0.00))
        //       |> Nvg.fill
        //       |> Nvg.fillColor ~~(C4f(0.0, 0.0, 0.0, 0.5))

        //let sg = 
        //    let size = size |> Mod.force
        //    Nvg.ofList [rect; text]
        //        |> Nvg.trafo ~~(M33d.Translation(V2d((double) size.X / 2.0, 10.0)))

        //runtime.CompileRender sg

    // GUI for samys health
    let health (runtime : IRuntime) (state : State) =
  
        // samys health
        let health = state.samy.health |> Mod.map (floor >> string)

        // state bar color (switches if health is low)
        let color = 
            adaptive {
                let! h = state.samy.health
                let c =
                    match h > 20.0 with
                    | true -> C4f(C4f.Green.R, C4f.Green.G, C4f.Green.B, 0.75F)
                    | false -> C4f(C4f.Red.R, C4f.Red.G, C4f.Red.B, 0.75F)
                return c
            }

        // size of state bar changes according to samys health
        let trafo =
            adaptive {
                let! h = state.samy.health
                return M33d.Scale(V2d(h / 100.0, 1.0))
            }

        let healthInfo = textNvg health  
        healthInfo
        //let text = Nvg.ContextApplicator(runtime.GetNanoVgContext(), Mod.constant healthInfo)

        //// green or red box
        //let overall = ref Box2d.Invalid
        //let rect =
        //    text.LocalBoundingBox() 
        //       |> Mod.map (fun bb -> 
        //            RoundedRectangle(Box2d.FromMinAndSize(bb.Min, 400.0, bb.Max.Y - bb.Min.Y),0.00))
        //       |> Nvg.fill
        //       |> Nvg.fillColor color
        //       |> Nvg.trafo trafo
 
        //let sg = 
        //    Nvg.ofList [rect; text]
        //        |> Nvg.trafo ~~(M33d.Translation(V2d(15.0, 10.0)))

        //runtime.CompileRender sg

    // GUI for player score
    let score (runtime : IRuntime) (state : State) =

        // player score
        let score =
            adaptive {
                let! s = state.score
                return "Score: " + s.ToString()
            }

        let scoreInfo = textNvg score
        scoreInfo
        //let text = Nvg.ContextApplicator(runtime.GetNanoVgContext(), Mod.constant scoreInfo)

        //// gray score box
        //let overall = ref Box2d.Invalid
        //let rect =
        //    text.LocalBoundingBox() 
        //       |> Mod.map (fun bb -> 
        //            let mutable b = bb.EnlargedBy(V2d(0.0, 3.0))
        //            overall := b.Union(!overall)
        //            !overall
        //          ) 
        //       |> Mod.map (fun b -> RoundedRectangle(b, 5.0))
        //       |> Nvg.fill
        //       |> Nvg.fillColor ~~(C4f(C4f.Gray80.R, C4f.Gray80.G, C4f.Gray80.B, 0.75F))
 
        //let sg = 
        //    Nvg.ofList [rect; text]
        //        |> Nvg.trafo ~~(M33d.Translation(V2d(15.0, 55.0)))

        //runtime.CompileRender sg

    // player highscore for finish screen
    let highscore (runtime : IRuntime) (hs : IMod<string>) (s : IMod<V2i>) =
        // window size
        let trafo =
            adaptive {
                let! s = s
                return M33d.Translation(V2d(((double)s.X / 2.5), ((double)s.Y / 2.0)))
            }

        textNvg hs
        //let hs = textNvg hs |> Nvg.trafo trafo
        //runtime.CompileRender hs

    // playername input at starting screen
    let playername (runtime : IRuntime) (pn : IModRef<string>) (s : IMod<V2i>) =
        // window size
        let trafo =
            adaptive {
                let! s = s
                return M33d.Translation(V2d((s.X / 2), (s.Y / 2)))
            }
        
        textNvg pn

        //let pn = textNvg pn |> Nvg.trafo trafo
        //runtime.CompileRender pn