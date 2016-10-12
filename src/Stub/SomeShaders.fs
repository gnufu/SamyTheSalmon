namespace SamyTheSalmon

open Aardvark.Base.Incremental
open Aardvark.Base.Rendering

open FShade

module Shaders =

    // simple alpha testing shader
    let alphaTest (vtx : DefaultSurfaces.Vertex) =
        let a = Config.Application.transparency  :> IMod<_>
        fragment {
            let t = !!a
            if t && vtx.c.W < 0.1 then discard ()
            return vtx.c
        }