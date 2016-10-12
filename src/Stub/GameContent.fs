namespace SamyTheSalmon

open System

open Aardvark.Base
open Aardvark.Base.Ag
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics
open Aardvark.Application

module GameContent = 

    module Media =
        // path to background music
        let sound = @"..\resources\audio\happy-background-music.wav"

        // load sound & returns player
        let player = sound |> Audio.loadPlayer 

    module Content =
        // load heightmap
        let heightmap = PixImage<byte> @"..\resources\terrain\hm_3.png"

        // load normalmap
        let normalmap = PixImage<byte> @"..\resources\terrain\nm.png"

        // load dUdV map for water refraction / reflection
        let dUdVMap = FileTexture (@"..\resources\terrain\water_dudv.png", true) :> ITexture

        // loads file and applies global trafo
        let private load f (s : float) name =
            let trafo = Trafo3d.RotationX(Math.PI / 2.0) * Trafo3d.Scale(s)
            Path.combine [@"..\resources"; name]
                |> f
                |> Sg.trafo (Mod.constant trafo)
        
        // load the models for the terrain and the water mesh
        // these are stored separately since different shaders and materials must be applied and
        // separating them at this point is cumbersome due to the file format, blender and assimp
        let terrain = @"terrain\terrain.3ds" |> load Assimp.load Config.World.scale 
        let water = @"terrain\water.3ds" |> load Assimp.load Config.World.scale 
        let skybox = @"terrain\skybox.3ds" |> load Assimp.load 2.5 |> Sg.trafo (Mod.constant (Trafo3d.RotationZInDegrees(270.0)))

        // load different obstacles
        let bear     = @"obstacles\bear\bear.obj"               |> load Assimp.load 0.01
        let runway   = @"obstacles\Runway\Runway-warnings.3DS"  |> load Assimp.load 0.05
        let jug      = @"obstacles\Jug\Jug1.obj"                |> load Assimp.load 0.5
        let pepsi    = @"obstacles\Pepsi\Pepsi_Can.obj"         |> load Assimp.load 0.004
        let treasure = @"obstacles\treasure\treasure_chest.obj" |> load Assimp.load 0.15
        let bb8      = @"obstacles\bb8\bb8.3DS"                 |> load Assimp.load 0.006
        let egg      = @"obstacles\egg\egg.3DS"                 |> load Assimp.load 0.0007
        let coin     = @"obstacles\coin\TyveKrone.obj"          |> load Assimp.load 0.03
        let barrel   = @"obstacles\light\Radioactive barrel.obj"|> load Assimp.load 0.02
        let boots_1    = @"obstacles\Army_boots\boots_1.obj"   |> load Assimp.load 0.03
        let boots_2    = @"obstacles\Army_boots\boots_2.obj"   |> load Assimp.load 0.03 

        // load the paths for the obstacles, applying the global trafo
        let paths = 
            let files = seq { 1 .. 6 } |> Seq.map (sprintf @"terrain\path%d.obj")
            files |> Seq.map (fun f -> f |> load Path.load Config.World.scale
                                         |> Path.points
                                         |> Array.sortBy (fun v -> v.Y)) |> Seq.toArray
        
        // calculates bounding box of riverbed
        let riverbedBB = terrain.LocalBoundingBox() |> Mod.force

        // loads goodies positions
        let goodies =
            // TODO: maybe move Path.load & Path.points somwhere else
            Path.combine [@"..\resources\terrain"; "goodies.obj"] |> Path.load |> Path.points

        // loads positions of fireflies
        let fireflies =
            // TODO: maybe move Path.load & Path.points somwhere else
            Path.combine [@"..\resources\terrain"; "fireflies.obj"] |> Path.load |> Path.points

        // goodie type
        let goodieType =
            [| coin |]
                |> Array.map (Sg.material (Mod.constant C4d.White) (Mod.constant C4d.White) (Mod.constant C4d.White) (Mod.constant 152.0))

        // obstacle types 
        let obstacleTypes  =
            let cullNone = Mod.constant CullMode.None
            let egg = egg |> Sg.cullMode cullNone
            
            // boots are loaded seperatly and rotate around each other each with
            // it's own trafo, finally they are put into one single sg node again
            let boots_2 = boots_2 |> Sg.trafo Config.Obstacles.bootsRotation_2
            let boots = 
                boots_1
                |> Sg.trafo Config.Obstacles.bootsRotation_1
                |> Sg.andAlso boots_2 
                |> Sg.cullMode cullNone

            [| bear; runway; jug; pepsi; treasure; boots; egg; bb8; barrel |]
                |> Array.map (Sg.material (Mod.constant C4d.White) (Mod.constant C4d.White) (Mod.constant C4d.White) (Mod.constant 152.0))
                //|> Array.map (Sg.loadAsync win.FramebufferSignature)

        let private plant = @"plants\AG01_Avena_sativa_Oats_3ds\AG01_9.3ds" |> load Assimp.load 0.8

        let private plantTrafos = 
            [| for x in riverbedBB.Min.X .. riverbedBB.Max.X do
                for y in riverbedBB.Min.Y .. riverbedBB.Max.Y do

                    let xNormalized = (float x - riverbedBB.Min.X) / riverbedBB.Size.X
                    let yNormalized = (float y - riverbedBB.Min.Y) / riverbedBB.Size.Y
                    let xPixPos = (xNormalized * float heightmap.Size.X) + 0.5
                    let yPixPos = (yNormalized * float heightmap.Size.Y) + 0.5
                    let clampedX = clamp 0.0 (float heightmap.Size.X - 1.0) (float xPixPos) |> int64
                    let clampedY = clamp 0.0 (float heightmap.Size.Y - 1.0) (float yPixPos) |> int64

                    let z : C3b = heightmap.GetMatrix<C3b>().[clampedX, clampedY]
                    let color = float z.B / 255.0
                    let z = riverbedBB.Min.Z + riverbedBB.SizeZ * (color * color)

                    if z <= 0.0 then
                        yield Trafo3d.Translation (float x, float y, z - 0.11)
            |]

        let plants = 
            [ for trafo in plantTrafos do
                yield Sg.trafo (Mod.constant trafo) plant ]
