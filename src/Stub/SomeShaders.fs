namespace SamyTheSalmon

open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Base.Rendering.Effects

open FShade

module Shaders =

    // simple alpha testing shader
    let alphaTest (vtx : Vertex) =
        let a = Config.Application.transparency  :> IMod<_>
        fragment {
            let t = a.GetValue()
            if t && vtx.c.W < 0.1 then discard ()
            return vtx.c
        }