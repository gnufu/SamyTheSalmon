namespace SamyTheSalmon

open System
open Aardvark.Application
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics

[<AutoOpen>]
module WaterExtensions =

    open FShade

    module DefaultSemantic =
        let ReflectionTexture = Sym.ofString "ReflectionTexture"
        let RefractionTexture = Sym.ofString "RefractionTexture"
        let dUdVTexture       = Sym.ofString "dUdVTexture"

    module Sg =
        let reflectionTexture (tex : IMod<ITexture>) (sg : ISg) =
            sg |> Sg.texture DefaultSemantic.ReflectionTexture tex

        let refractionTexture (tex : IMod<ITexture>) (sg : ISg) =
            sg |> Sg.texture DefaultSemantic.RefractionTexture tex

        let dUdVTexture (tex : IMod<ITexture>) (sg : ISg) =
            sg |> Sg.texture DefaultSemantic.dUdVTexture tex
        
module Water =

    module Effect =
        open FShade

        type FShade.Parameters.Uniforms.UniformScope with
            member x.ClipFactor : float = x?ClipFactor

        // makes the water flow by continously incrementing the x component of the UV coordinates of each vertex
        let flow (offset : IMod<double>) (v : DefaultSurfaces.Vertex) =

            vertex {
                let t = V2d(v.tc.X + !!offset, v.tc.Y)
                return {v with tc = t }
            }

        let private reflectionSampler =
            sampler2d {
                texture uniform?ReflectionTexture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Clamp
                addressV WrapMode.Clamp
            }

        let private refractionSampler =
            sampler2d {
                texture uniform?RefractionTexture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Clamp
                addressV WrapMode.Clamp
            }

        let private dUdVSampler =
            sampler2d {
                texture uniform?dUdVTexture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let fresnel (v : DefaultSurfaces.Vertex) =
            fragment {

                let eye = uniform.CameraLocation.XYZ

                // sample dUdV map for distortion
                let dist = dUdVSampler.Sample(v.tc).XY * 2.0 - 1.0
                let tc = (v.pos.XY / v.pos.W) * 0.5 + 0.5 + (dist * 0.025)

                // sample reflection texture
                let refl = reflectionSampler.Sample(tc).XYZ

                // sample refraction texture 
                let refr = refractionSampler.Sample(tc).XYZ

                // compute fresnel term
                let f = 

                    // schlick approximation for air - water
                    let schlick theta =
                        let r0 = 0.02
                        r0 + (1.0 - r0) * (pow (1.0 - theta) 5.0)

                    // rectify normal a bit
                    let N = 
                        let sgn = if eye.Z > 0.0 then 1.0 else -1.0
                        V3d (v.n.XY, v.n.Z + 10.0) * sgn |> Vec.normalize

                    let V = (eye - v.wp.XYZ) |> Vec.normalize
                        
                    (Vec.dot N V) |> max 0.0 |> schlick
     
                // mix reflection and refraction
                let color = Vec.lerp refr refl f

                // incorporate constant color of the water surface
                return V4d(Vec.lerp color v.c.XYZ v.c.W, 1.0)
            }

        // color of the fog
        let private fogColor =
            let v = Config.Water.color.ToV4d ()
            v.XYZI

        // applies exponential fog for under water scenery
        let fog (v : DefaultSurfaces.Vertex) =
            fragment {

                // get world position
                let p = v.wp.XYZ

                // only apply if below water surface
                if p.Z <= 0.0 then

                    // Find the depth of the fragment
                    let d =
                        // if camera is below water, simply take distance to camera
                        // otherwise we have to compute the distance to intersection of the view ray with the water plane
                        if uniform.CameraLocation.Z <= 0.0 then
                            V3d.Distance (p, uniform.CameraLocation)
                        else
                            // find intersection of view ray with water plane
                            let g = uniform.CameraLocation - p
                            let t = -p.Z / g.Z

                            // ignore distance above water
                            Vec.length (t * g)

                    // apply
                    let f = exp (-d * Config.Water.density) |> clamp 0.0 1.0
                    return (1.0 - f) * fogColor + f * v.c;
                else
                    return v.c
            }

        // applies exponential fog for the water surface itself
        let fogSurface (v : DefaultSurfaces.Vertex) =
            fragment {

                // only apply if camera is below water surface
                if uniform.CameraLocation.Z <= 0.0 then

                    // simply take distance to camera
                    let d = V3d.Distance (v.wp.XYZ, uniform.CameraLocation)
                    
                    // apply
                    let f = exp (-d * Config.Water.density) |> clamp 0.0 1.0
                    return (1.0 - f) * fogColor + f * v.c;
                else
                    return v.c
            }

        // clips geometry above / below water
        let clip (v : DefaultSurfaces.Vertex) =
            fragment {
                if v.wp.Z * uniform.ClipFactor > 0.0 then
                    discard ()

                return v.c
            }

    // updates the water state, called per frame
    let update (dt : double) (s : State) =

        s.water.offset <=+ dt * Config.Water.speed
        s

[<AutoOpen>]
module SgWaterExtensions =

    type Clip =
        | None = 0
        | Above = 1
        | Below = -1

    module Sg =

        let clip (mode : IMod<Clip>) (sg : ISg) =

            let factor = mode |> Mod.map (fun m -> float m)
            sg |> Sg.uniform "ClipFactor" factor