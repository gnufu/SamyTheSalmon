namespace SamyTheSalmon

open System
open Aardvark.Base
open Aardvark.Base.Incremental

type TextureSampling = NearestNeigbor | Bilinear 
type MipMapping = Off | NearestNeighbor | Linear 

module Config =

    let app = new Aardvark.Application.WinForms.OpenGlApplication()

    module Lighting = 

        [<Literal>]
        let maxLights = 8

    module Water =

        // the flowing speed of the water
        let speed = 1.0

        // the color of the water surface
        let color = C4f(0.15, 0.3, 0.4, 0.2)

        // the fog density
        let density = 0.5

        // size of texture of reflection
        let reflectionTextureSize = V2i (1024, 1024) |> Mod.init

    module Application =

        // stopwatch for time meassuring for player's score
        let stopwatch = System.Diagnostics.Stopwatch()

        // fullscreen & cursor switch
        let fullscreen = true
        let cursorVisible = false        
    
        // cap for delta time 
        let deltaCap = 0.1

        // play sound
        let playSound = true
        
        // switch for wire frame
        let wireFrame = false |> Mod.init

        // switch for texture samling quality
        let textureSamlingQuality = TextureSampling.Bilinear |> Mod.init

        // switch for mip mapping quality
        let mipMappingQuality = MipMapping.Linear |> Mod.init

        // switch for frustum culling
        let frustumCulling = true |> Mod.init

        // switch for transparency
        let transparency = true |> Mod.init

     module Obstacles =
       
        // the initial interval of the obstacles spawning
        let intervalStart = 2.0

        // the initial speed of the obstacles
        let flowSpeedStart = 0.025

        // the maximum spinning speed around an axis (radians / second)
        let maxSpin = Math.PI / 4.0

        // determines how much ahead of the player the obstacles should spawn (path position)
        let relativeOffsetSpawn = 0.3

        // determines how much behind the player the obstacles are removed (path position)
        let relativeOffsetLimit = 0.15

        // rotation trafos for boots obstacles
        let bootsRotation_1 = Trafo3d.Rotation(V3d.One) |> Mod.init
        let bootsRotation_2 = Trafo3d.Rotation(V3d.One) |> Mod.init

    module World =

        // global scale of the terrain and water
        let scale = 0.25

        // color of the sun light
        let sun = C4d (0.88, 0.72, 0.53) * 0.75

    module Samy =

        // the maximum speed that can be achieved by pressing W, A, D
        let maxSpeed = V2d(0.35, 0.75)

        // the speed of a dash movement
        let dashSpeed = 2.5

        // cooldown time for the dash movement (seconds)
        let dashCooldown = 5.0

        // gravity that is applied if Z >= 0
        let gravity = 2.0

        // drag while under water
        let dragWater = 1.0

        // drag while aboive water
        let dragAir = 0.5

        // health decreasing factor
        let decHealthFactor = 14.0