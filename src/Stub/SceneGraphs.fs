namespace SamyTheSalmon

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Base.Rendering
open Aardvark.SceneGraph.Semantics

open SamyTheSalmon.Lighting

module SceneGraphs =
    
    // finish- and starting-screen sg 
    // sg only consists of a quad which goes over the whole screen 
    // with a texture on it
    let screenSg texPath =
        let quad = SomeHelpers.quad C4b.White
        let texture = FileTexture(texPath, true) :> ITexture |> Mod.constant

        quad 
        |> Sg.texture DefaultSemantic.DiffuseColorTexture texture
        |> Sg.effect [ DefaultSurfaces.diffuseTexture |> toEffect ]

    // build the start-screen sg
    let startSg = screenSg @"..\resources\screens\startScreen.png"
            
    // build the finish-screen sg
    let finishSg = screenSg @"..\resources\screens\finishScreen.png"

    // build the player-lost sg
    let playerLostSg = screenSg @"..\resources\screens\youLostScreen.png"

    let changeToFinishSg (pos : V3d) currSg = 
        if pos.Y <= -9.6 then transact( fun _ -> Mod.change currSg finishSg )

    // build the gaming sg
    let gameSg worldSg view proj lightSources =
        worldSg
            // viewTrafo () creates camera controls and returns IMod<ICameraView> which we project to its view trafo component by using CameraView.viewTrafo
            |> Sg.viewTrafo (view |> Mod.map CameraView.viewTrafo) 
            // perspective. () connects a proj trafo to the current main window (in order to take account for aspect ratio when creating the matrices.
            // Again, perspective() returns IMod<Frustum> which we project to its matrix by mapping ofer Frustum.projTrafo.
            |> Sg.projTrafo (proj |> Mod.map Frustum.projTrafo)
            |> Sg.lights lightSources
            |> Sg.blendMode (Mod.constant Rendering.BlendMode.Blend)
