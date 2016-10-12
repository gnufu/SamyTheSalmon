namespace SamyTheSalmon

open System
open Aardvark.Application
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics

module Lighting = 

    type Source = {
            typ         : int   // 0 = directional, 1 = point light, 2 = spot light
            direction   : V3d   // light direction (doesn't have to be normalized)
            position    : V3d   // position of the point light
            coneAngle   : float // cone angle of spot light (in degree)
            attenuation : V3d   // attenuation of the point light (constant, linear, quadratic)
            ambient     : C4d   // ambient color of the light
            color       : C4d   // diffuse + specular color of the light
        }

    let directional direction ambient color =
        { typ = 0
          direction = direction
          position = V3d.Zero
          coneAngle = 0.0 
          attenuation = V3d.Zero 
          ambient = ambient 
          color = color }

    let point position attenuation ambient color =
        { typ = 1 
          direction = V3d.Zero 
          position = position
          coneAngle = 0.0 
          attenuation = attenuation 
          ambient = ambient 
          color = color }

    let point' position radius ambient color =
        point position (V3d (1.0, 2.0 / radius, 1.0 / (radius * radius))) ambient color

    let zero =
        { typ = 0
          direction = V3d.Zero
          position = V3d.Zero 
          coneAngle = 0.0
          attenuation = V3d.Zero
          ambient = C4d.Black
          color = C4d.Black }

    // Effects for the lighting
    module Effect =
        open FShade

        type FShade.Parameters.Uniforms.UniformScope with
            member x.MaterialAmbient : V4d                                      = x?MaterialAmbient
            member x.MaterialDiffuse : V4d                                      = x?MaterialDiffuse
            member x.MaterialSpecular : V4d                                     = x?MaterialSpecular
            member x.MaterialShininess : float                                  = x?MaterialShininess
            member x.LightType : Arr<N<Config.Lighting.maxLights>, int>         = x?LightType
            member x.LightDirection : Arr<N<Config.Lighting.maxLights>, V3d>    = x?LightDirection
            member x.LightPosition : Arr<N<Config.Lighting.maxLights>, V3d>     = x?LightPosition
            member x.LightConeAngle : Arr<N<Config.Lighting.maxLights>, float>    = x?LightConeAngle
            member x.LightAttenuation : Arr<N<Config.Lighting.maxLights>, V3d>  = x?LightAttenuation
            member x.LightAmbient : Arr<N<Config.Lighting.maxLights>, V4d>      = x?LightAmbient
            member x.LightColor : Arr<N<Config.Lighting.maxLights>, V4d>        = x?LightColor

        let private normalSampler =
            sampler2d {
                texture uniform?NormalMapTexture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        // Fragment shader for pertubing the normal using a normal map
        let normalMapping (v : DefaultSurfaces.Vertex) =
            fragment {
                let N = 
                    let TBN = M33d.FromCols(v.t.Normalized, v.b.Normalized, v.n.Normalized) // tangent space -> world space
                    let n = normalSampler.Sample(v.tc).XYZ * 2.0 - 1.0

                    (TBN * n) |> Vec.normalize

                return {v with n = N}
            }

        // Phong lighting
        let phong (v : DefaultSurfaces.Vertex) =
            fragment {

                // Normal and view vector
                let N = v.n.Normalized
                let V = (uniform.CameraLocation - v.wp.XYZ) |> Vec.normalize
            
                // Iterate over all lights
                let mutable color = V4d.Zero

                for i in 0 .. Config.Lighting.maxLights - 1 do

                    // Light vector
                    let L =
                        match uniform.LightType.[i] with
                            | 0 -> uniform.LightDirection.[i] |> Vec.normalize
                            | _ -> (uniform.LightPosition.[i] - v.wp.XYZ) |> Vec.normalize

                    // Reflection vector
                    let R = -L - 2.0 * N * (Vec.dot -L N) 

                    // Compute angles
                    let NdotL = Vec.dot N L |> max 0.0
                    let VdotR = Vec.dot V R |> max 0.0

                    // Attenuation
                    let att =
                        match uniform.LightType.[i] with
                            | 0 -> 1.0
                            (*| 2 -> 
                                let d = V3d.Distance(v.wp.XYZ, uniform.LightPosition.[i])
                                let dir = v.wp.XYZ - uniform.LightPosition.[i] |> Vec.normalize
                                let cosine = Vec.dot uniform.LightDirection.[i] dir
                                let coneCosine = cos uniform.LightConeAngle.[i]
                                let factor = clamp 0.0 1.0 ((cosine - coneCosine) / (1.0 - coneCosine))
                                if cosine > coneCosine then
                                    factor / (d * uniform.LightAttenuation.[i].Y)
                                else 0.0*)
                            | _ -> 
                                let d = V3d.Distance(uniform.LightPosition.[i], v.wp.XYZ)
                                1.0 / Vec.dot uniform.LightAttenuation.[i] (V3d(1.0, d, d * d))

                    color <- color +
                             v.c * uniform.MaterialAmbient * uniform.LightAmbient.[i] +
                             uniform.LightColor.[i] * att * (v.c * uniform.MaterialDiffuse * NdotL + uniform.MaterialSpecular * Math.Pow(VdotR, uniform.MaterialShininess))

                // Return final color
                return V4d(color.XYZ, v.c.W)
            }

        // Phong (specular only)
        let phongSpecular (v : DefaultSurfaces.Vertex) =
            fragment {

                // Normal and view vector
                let N = v.n.Normalized
                let V = (uniform.CameraLocation - v.wp.XYZ) |> Vec.normalize
            
                // Iterate over all lights
                let mutable color = v.c

                for i in 0 .. Config.Lighting.maxLights - 1 do

                    // Light vector
                    let L =
                        match uniform.LightType.[i] with
                            | 0 -> uniform.LightDirection.[i]
                            | _ -> (uniform.LightPosition.[i] - v.wp.XYZ) |> Vec.normalize

                    // Reflection vector
                    let R = -L - 2.0 * N * (Vec.dot -L N) 

                    // Compute angles
                    let NdotL = Vec.dot N L |> max 0.0
                    let VdotR = Vec.dot V R |> max 0.0

                    // Attenuation
                    let att =
                        match uniform.LightType.[i] with
                            | 0 -> 1.0
                            | _ -> let d = V3d.Distance(uniform.LightPosition.[i], v.wp.XYZ)
                                   1.0 / Vec.dot uniform.LightAttenuation.[i] (V3d(1.0, d, d * d))

                    color <- color +
                             uniform.LightColor.[i] * att * (uniform.MaterialSpecular * Math.Pow(VdotR, uniform.MaterialShininess))

                // Return final color
                return V4d(color.XYZ, v.c.W)
            }


[<AutoOpen>]
module SgLightingExtensions =

    module Sg =

        // Defines the light sources
        let lights (l : IMod<Lighting.Source []>) sg =

            let unpack f = Mod.map (fun x -> x |> Seq.map f |> Seq.toArray) l

            sg
                |> Sg.uniform "LightType" ( unpack (fun x -> x.typ) )
                |> Sg.uniform "LightDirection" ( unpack (fun x -> x.direction.Normalized) )
                |> Sg.uniform "LightPosition" ( unpack (fun x -> x.position) )
                |> Sg.uniform "LightConeAngle" ( unpack (fun x -> x.coneAngle) )
                |> Sg.uniform "LightAttenuation" ( unpack (fun x -> x.attenuation) )
                |> Sg.uniform "LightAmbient" ( unpack (fun x -> x.ambient.ToV4d()) )
                |> Sg.uniform "LightColor" ( unpack (fun x -> x.color.ToV4d()) )

        // Defines the material
        let material (ambient : IMod<C4d>) (diffuse : IMod<C4d>) (specular : IMod<C4d>) (shininess : IMod<float>) sg =
            sg
                |> Sg.uniform "MaterialAmbient" ( ambient |> Mod.map ( fun c -> c.ToV4d() ) )
                |> Sg.uniform "MaterialDiffuse" ( diffuse |> Mod.map ( fun c -> c.ToV4d() ) )
                |> Sg.uniform "MaterialSpecular" ( specular |> Mod.map ( fun c -> c.ToV4d() ) )
                |> Sg.uniform "MaterialShininess" shininess 
        