namespace SamyTheSalmon

open Aardvark.Base
open Aardvark.Base.Ag
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Base.Rendering.Effects
open Aardvark.SceneGraph

[<AutoOpen>]
module RenderingExtensions =

    open FShade

    module DefaultSemantic =
        let Color0 = Sym.ofString "Color0"
        let Color1 = Sym.ofString "Color1"

    type Color0Attribute() = inherit SemanticAttribute(DefaultSemantic.Color0.ToString())
    type Color1Attribute() = inherit SemanticAttribute(DefaultSemantic.Color1.ToString())

    module DefaultSurfaces =

        type Fragment = {
            [<Color0>]  c0 : V4d
            [<Color1>]  c1 : V4d
        }

        let duplicateOutput (v : Vertex) =
            fragment {
                return { c0 = v.c; c1 = v.c }
            }

    module RenderTask =

        open Aardvark.Base.Rendering
        open Aardvark.Base.Incremental.Operators

        // renders to the given FBO, clearing it first
        let renderTo' (fbo : IOutputMod<IFramebuffer>) (task : IRenderTask) =

            let runtime = task.Runtime.Value
            let signature = task.FramebufferSignature.Value

            let clearColors =
                signature.ColorAttachments
                    |> Map.toList
                    |> List.map (fun (_, (s, _)) -> s, C4f.Black)

            let clear = runtime.CompileClear(signature, ~~clearColors, ~~1.0)

            RenderTask.ofList [clear; task] |> RenderTask.renderTo fbo

    module Sg =

        let writeBuffers (buffers : Set<Symbol>) (sg : ISg) =
            Sg.WriteBuffersApplicator (Some buffers, Mod.constant sg) :> ISg

        let writeBuffer (buffer : Symbol) (sg : ISg) =
            sg |> writeBuffers (buffer |> Set.singleton)
