namespace SamyTheSalmon

open Aardvark.Application
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Base.Rendering.Effects
open Aardvark.Rendering.GL
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics

// source: http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-16-shadow-mapping/
module Shadows = 

    // look at scene from view of lights
    let private shadowCam (s : State) =  
        let light = (s.lights |> Mod.force).[0]
        CameraView.Look(V3d(0,0,10), -light.direction, V3d.OIO)

    let private shadowProj = 
        Trafo3d.OrthoProjectionRH(-20.0, 20.0, -20.0, 20.0, 0.1, 1000.0) 

    // compute light space view proj trafo
    let lightSpaceViewProjTrafo (s : State) = 
        (shadowCam s |> CameraView.viewTrafo) * (shadowProj) |> Mod.constant

    let private shadowMapSize =  V2i(4096, 4096) |> Mod.init

    // creates shadow map
    let createTexture (r : IRuntime) (s : State) (sg : ISg) = 
        let signature =
            r.CreateFramebufferSignature [
                DefaultSemantic.Depth, { format = RenderbufferFormat.DepthComponent32; samples = 1 }
            ]

        let fbo = r.CreateFramebuffer (signature, shadowMapSize)

        sg
        |> Sg.viewTrafo (shadowCam s |> CameraView.viewTrafo |> Mod.constant)
        |> Sg.projTrafo (shadowProj |> Mod.constant)
        |> Sg.effect [
            DefaultSurfaces.trafo |> toEffect
        ]
        |> Sg.compile r signature   
        |> RenderTask.renderTo' fbo
        |> RenderTask.getResult DefaultSemantic.Depth

    // shadow map shader
    module ShadowShader = 
        open FShade

        type UniformScope with
            member x.LightViewMatrix : M44d = uniform?LightViewMatrix
        
        let private PoissonDisk = 
            [|
                V2d( -0.94201624,  -0.39906216 )
                V2d(  0.94558609,  -0.76890725 ) 
                V2d( -0.094184101, -0.92938870 )
                V2d(  0.34495938,   0.29387760 ) 
                V2d( -0.91588581,   0.45771432 ) 
                V2d( -0.81544232,  -0.87912464 ) 
                V2d( -0.38277543,   0.27676845 ) 
                V2d(  0.97484398,   0.75648379 ) 
                V2d(  0.44323325,  -0.97511554 ) 
                V2d(  0.53742981,  -0.47373420 ) 
                V2d( -0.26496911,  -0.41893023 ) 
                V2d(  0.79197514,   0.19090188 ) 
                V2d( -0.24188840,   0.99706507 ) 
                V2d( -0.81409955,   0.91437590 ) 
                V2d(  0.19984126,   0.78641367 ) 
                V2d(  0.14383161,  -0.14100790 ) 
            |]

        let private shadowSampler =
            sampler2dShadow {
                texture uniform?ShadowTexture
                filter Filter.MinMagLinear
                addressU WrapMode.Border
                addressV WrapMode.Border
                borderColor C4f.White
                comparison ComparisonFunction.LessOrEqual
            }

        let shader (vtx : Vertex) =
            fragment {
                let lightSpace = uniform.LightViewMatrix * vtx.wp
                let div = lightSpace.XYZ / lightSpace.W
                let v = V3d(0.5, 0.5,0.5) + V3d(0.5, 0.5, 0.5) * div.XYZ

                // PCF using offset disk from 
                // http://developer.download.nvidia.com/whitepapers/2008/PCSS_Integration.pdf
                let mutable sum = 0.0
                for i = 0 to 15 do
                    let offset = PoissonDisk.[i] * (1.0 / 4096.0)
                    sum <- sum + shadowSampler.Sample(v.XY + offset, v.Z)
                sum <- (sum / 16.0) + 0.5

                return V4d(vtx.c.XYZ * sum, vtx.c.W)
            }