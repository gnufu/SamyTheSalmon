namespace SamyTheSalmon

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.SceneGraph

module World =

    // passes
    let private pass0 = Rendering.RenderPass.main
    let private pass1 = Rendering.RenderPass.after "transparency" RenderPassOrder.Arbitrary pass0
   
    // cull modes
    let private cullBack = Mod.constant CullMode.Clockwise
    let private cullFront = Mod.constant CullMode.CounterClockwise

    // utility function to create FBO
    let private createFBO (r : IRuntime) (size : IMod<V2i>) (attachments : List<Symbol * AttachmentSignature>)  =

        let signature = attachments |> r.CreateFramebufferSignature
        let fbo = r.CreateFramebuffer (signature, size)
      
        (fbo, signature)

    // create FBO for refraction effect
    let private createRefractionFBO (win : IRenderControl) =

        let attachments = [
            DefaultSemantic.Color0, { format = RenderbufferFormat.Rgba8; samples = 1 }
            DefaultSemantic.Color1, { format = RenderbufferFormat.Rgba8; samples = 1 }
            DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
        ]

        attachments |> createFBO win.Runtime win.Sizes

    // create FBO for reflection effect
    let private createReflectionFBO (win : IRenderControl) =

        let attachments = [
            DefaultSemantic.Color0, { format = RenderbufferFormat.Rgba8; samples = 1 }
            DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
        ]

        attachments |> createFBO win.Runtime Config.Water.reflectionTextureSize

    // compile shader for objects
    let private compileObjectsShader (r : IRuntime) (signature : IFramebufferSignature) =

        let effect = [
            DefaultSurfaces.trafo |> toEffect 
            Water.Effect.clip |> toEffect
            DefaultSurfaces.diffuseTexture |> toEffect
            SamyTheSalmon.Lighting.Effect.phong |> toEffect 
            Water.Effect.fog |> toEffect
            DefaultSurfaces.duplicateOutput |> toEffect
        ]

        (r.PrepareEffect (signature, effect) :> ISurface)

    // compile shader for plants
    let private compilePlantsShader (r : IRuntime) (signature : IFramebufferSignature) =

        let effect = [
            DefaultSurfaces.trafo |> toEffect 
            Water.Effect.clip |> toEffect
            DefaultSurfaces.diffuseTexture |> toEffect
            DefaultSurfaces.simpleLighting |> toEffect
            //SamyTheSalmon.Lighting.Effect.phong |> toEffect
            Shaders.alphaTest |> toEffect
            Water.Effect.fog |> toEffect
            DefaultSurfaces.duplicateOutput |> toEffect
        ]
                
        (r.PrepareEffect (signature, effect) :> ISurface)

    // compile shader for fireflies
    let private compileFireflyShader (r : IRuntime) (signature : IFramebufferSignature) =

        let effect = [
            DefaultSurfaces.trafo |> toEffect
            DefaultSurfaces.constantColor C4f.Green |> toEffect
            DefaultSurfaces.simpleLighting |> toEffect
            DefaultSurfaces.duplicateOutput |> toEffect
        ]

        (r.PrepareEffect (signature, effect) :> ISurface)

    // switch for transparency on / off
    let transparency =
        adaptive {
            let! t = Config.Application.transparency 
            return match t with
                    | true -> BlendMode.Blend
                    | false -> BlendMode.None
        }

    // renders the shadows to a texture
    let private renderShadows (win : IRenderControl) (objects : aset<ISg>) (s : State) =
        
        let trafo = Shadows.lightSpaceViewProjTrafo s
        let plants = GameContent.Content.plants

        let texture =
            objects
                //|> ASet.unionTwo plants
                |> Sg.cullSet trafo
                |> Sg.andAlso (plants |> Sg.group')
                |> Sg.cullMode cullBack
                |> Shadows.createTexture win.Runtime s 

        (texture, trafo)

    // builds the sg for the solid scene (i.e. everything but the water surface)
    let private solidScene (objects : ISg) (plants : ISg) (shadowTexture : IMod<ITexture>, shadowTrafo : IMod<Trafo3d>) (s : State) =

        // building scenegraphs
        let terrainSg = GameContent.Content.terrain
        let skyboxSg = GameContent.Content.skybox

        Sg.group' [
            objects
                |> Sg.pass pass0

            terrainSg
                |> Sg.material (Mod.constant C4d.White) (Mod.constant C4d.White) (Mod.constant C4d.Black) (Mod.constant 32.0)
                |> Sg.uniform "LightViewMatrix" shadowTrafo
                |> Sg.texture (Sym.ofString "ShadowTexture") shadowTexture
                |> Sg.effect [
                    DefaultSurfaces.trafo |> toEffect 
                    Water.Effect.clip |> toEffect
                    DefaultSurfaces.diffuseTexture |> toEffect
                    SamyTheSalmon.Lighting.Effect.normalMapping |> toEffect
                    SamyTheSalmon.Lighting.Effect.phong |> toEffect 
                    Shadows.ShadowShader.shader |> toEffect
                    Water.Effect.fog |> toEffect
                    DefaultSurfaces.duplicateOutput |> toEffect
                 ]
                 |> Sg.pass pass0

            plants
                |> Sg.pass pass1

            skyboxSg
                |> Sg.material (Mod.constant C4d.White) (Mod.constant C4d.White) (Mod.constant C4d.White) (Mod.constant 32.0)
                |> Sg.effect [
                    DefaultSurfaces.trafo |> toEffect 
                    Water.Effect.clip |> toEffect
                    DefaultSurfaces.diffuseTexture |> toEffect 
                    DefaultSurfaces.duplicateOutput |> toEffect
                ]
                |> Sg.cullMode (Mod.constant CullMode.None)
                |> Sg.pass pass0
        ]

    // builds the sg for the water surface
    let private waterSurface (s : State) =

        let waterSg = GameContent.Content.water

        waterSg
            |> Sg.material (Mod.constant C4d.White) (Mod.constant C4d.White) (Mod.constant C4d.White) (Mod.constant 128.0)
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                Water.Effect.flow |> toEffect
                SamyTheSalmon.Lighting.Effect.normalMapping |> toEffect
                DefaultSurfaces.constantColor Config.Water.color |> toEffect
                Water.Effect.fresnel |> toEffect
                Water.Effect.fogSurface |> toEffect
                SamyTheSalmon.Lighting.Effect.phongSpecular |> toEffect 
                DefaultSurfaces.duplicateOutput |> toEffect
            ]
            |> Sg.uniform ("offset") s.water.offset

    // helper function to apply trafos, lights, blending, etc.
    let private addToScene view proj lights sg =

        // switch for wire frame on / off
        let fillMode =
            adaptive {
                let! wf = Config.Application.wireFrame 
                return match wf with
                       | true -> FillMode.Line
                       | false -> FillMode.Fill
            }

        // switch for changing texture and mip mapping quality
        let modifysampler = 
            adaptive {
                let! texSampling = Config.Application.textureSamlingQuality
                let! mipMapping = Config.Application.mipMappingQuality

                // for details see: https://github.com/vrvis/aardvark/blob/ca4201b5d0a14d86d055d02b487b4cf5855955fa/src/Aardvark.Base/Rendering/SamplerState.cs
                let filterSwitch =
                    match (texSampling, mipMapping) with
                    | (TextureSampling.NearestNeigbor, MipMapping.Off) -> TextureFilter.MinMagPoint
                    | (TextureSampling.NearestNeigbor, MipMapping.NearestNeighbor) -> TextureFilter.MinMagMipPoint
                    | (TextureSampling.NearestNeigbor, MipMapping.Linear) -> TextureFilter.MinMagPointMipLinear
                    | (TextureSampling.Bilinear, MipMapping.Off) -> TextureFilter.MinMagLinear
                    | (TextureSampling.Bilinear, MipMapping.NearestNeighbor) -> TextureFilter.MinMagLinearMipPoint
                    | (TextureSampling.Bilinear, MipMapping.Linear) -> TextureFilter.MinMagMipLinear

                let ssd = SamplerStateDescription()
                ssd.Filter <- filterSwitch 
                ssd.AddressU <- WrapMode.Wrap
                ssd.AddressV <- WrapMode.Wrap
                return fun (_ : SamplerStateDescription) -> ssd
            }

        sg |> Sg.viewTrafo view
           |> Sg.projTrafo proj
           |> Sg.lights lights
           |> Sg.blendMode transparency
           |> Sg.fillMode fillMode
           |> Sg.modifySamplerState DefaultSemantic.DiffuseColorTexture modifysampler

    // build the overall world-sg
    let sg (win : IRenderControl) (s : State) =

        let r = win.Runtime

        // create FBOs for refraction & reflection
        let fboRefraction = createRefractionFBO win
        let fboReflection = createReflectionFBO win

        // build view and projection trafos
        let viewTrafo = s.camera.view |> Mod.map CameraView.viewTrafo
        let projTrafo = s.camera.frustum |> Mod.map Frustum.projTrafo
        let mirroredViewTrafo = viewTrafo |> Mod.map (fun t -> Trafo3d.Scale (1.0, 1.0, -1.0) * t)

        // build view frusta
        let frustum = Mod.map2 (*) viewTrafo projTrafo 
        let mirroredFrustum = Mod.map2 (*) mirroredViewTrafo projTrafo

        // prepare sg for goodies and obstacles
        let objects = 
            let goodies = Sg.goodies s.goodies.instances
            let obstacles = Sg.obstacles s.obstacles.instances
            ASet.union goodies obstacles
            //ASet.empty

        // render shadows
        let shadows = renderShadows win objects s

        // render solid scene into both color attachments
        let refractionTexture =

            let fireflies = 
                let shader = compileFireflyShader r (snd fboRefraction) 
                Sg.fireflies s.fireflies.instances
                |> Sg.cullSet frustum
                |> Sg.surface shader

            let obj =
                let shader = compileObjectsShader r (snd fboRefraction) 
                objects
                    |> Sg.cullSet frustum
                    |> Sg.surface shader
                    |> Sg.andAlso fireflies

            let plants =
                let shader = compilePlantsShader r (snd fboRefraction) 
                GameContent.Content.plants
                    //|> ASet.ofList
                    //|> Sg.cullSet frustum
                    |> Sg.group'
                    |> Sg.surface shader

            solidScene obj plants shadows s
                |> Sg.cullMode cullBack
                |> Sg.clip (Mod.constant Clip.None)
                |> addToScene viewTrafo projTrafo s.lights
                |> Sg.compile r (snd fboRefraction)
                |> RenderTask.renderTo' (fst fboRefraction)
                |> RenderTask.getResult DefaultSemantic.Color1

        // render reflection into own FBO
        let reflectionTexture =

            let fireflies = 
                let shader = compileFireflyShader r (snd fboRefraction) 
                Sg.fireflies s.fireflies.instances
                |> Sg.cullSet mirroredFrustum
                |> Sg.surface shader

            let obj =
                let shader = compileObjectsShader r (snd fboReflection)
                objects
                    |> Sg.cullSet mirroredFrustum
                    |> Sg.surface shader
                    |> Sg.andAlso fireflies

            let plants =
                let shader = compilePlantsShader r (snd fboReflection) 
                GameContent.Content.plants
                    |> Sg.group'
                    |> Sg.surface shader

            let clipMode = s.camera.view |> Mod.map (fun v -> if v.Location.Z > 0.0 then
                                                                  Clip.Below
                                                              else
                                                                  Clip.Above)

            solidScene obj plants shadows s
                |> Sg.cullMode cullFront
                |> Sg.clip clipMode
                |> addToScene mirroredViewTrafo projTrafo s.lights
                |> Sg.compile r (snd fboReflection)
                |> RenderTask.renderTo' (fst fboReflection)
                |> RenderTask.getResult DefaultSemantic.Color0
            
        let map = Mod.constant GameContent.Content.dUdVMap

        // use second attachment as refraction texture and render into first to
        // obtain the final scene       
        let finalScene =
            
            waterSurface s
                |> addToScene viewTrafo projTrafo s.lights
                |> WaterSg.reflectionTexture reflectionTexture
                |> WaterSg.refractionTexture refractionTexture
                |> WaterSg.dUdVTexture map
                |> Sg.writeBuffer DefaultSemantic.Color0
                |> Sg.compile r (snd fboRefraction)
                |> RenderTask.renderTo' (fst fboRefraction)
                |> RenderTask.getResult DefaultSemantic.Color0

        // render full screen quad and sample final result
        SomeHelpers.quad C4b.White
            |> Sg.diffuseTexture finalScene
            |> Sg.effect [ DefaultSurfaces.diffuseTexture |> toEffect ]