namespace SamyTheSalmon

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph

[<AutoOpen>]
module GameTypes =

    // the status of the game
    type Status = Initializing | ShowingInstructions | InitializingGame | Running | EndOfGame

    // state of the player
    type Samy = 
        { 
            position    : IModRef<V3d>
            speed       : IModRef<V2d>      // speed relative to orientation (x = left / right, y = forward / back)
            velocity    : IModRef<V3d>      // affected by gravity, bouncing, ... 
            dash        : IModRef<double>   // timer for dashing
            health      : IModRef<double> 
        }

    // state of the camera view
    type Camera = 
        { 
            view : IModRef<CameraView> 
            frustum : IMod<Frustum>
        }
        
    // state of the water surface
    type Water = { offset : IModRef<double> }

    // state of a single obstacle
    type Obstacle =
        {
            sg : ISg
            position : IModRef<double>      // 0.0 - 1.0
            rotation : IModRef<Trafo3d>
            boundingbox : IMod<Box3d>
            spin : V3d                      // yaw, pitch, roll (radians / second)
            path : V3d []
        }

    // state of the obstacles
    type Obstacles = 
        {
            timer : double
            instances : IModRef<Obstacle list>
        }

    // state of single goodie
    type Goodie =
        {
            sg : ISg
            position : IModRef<V3d>
            rotation : IModRef<Trafo3d>
            spin : V3d
            boundingbox : IMod<Box3d>
        }

    // state of all goodies
    type Goodies =
        {
            instances : IModRef<Goodie list>
        }

    // state of a firefly
    type Firefly =
        {
            sceneGraph : ISg
            position   : ModRef<V3d>
            t          : ModRef<double> 
            light      : ModRef<Lighting.Source>
        }

    // state of all fireflies
    type Fireflies =
        {
            instances : IModRef<Firefly list>
        }
    
    // overall game state
    type State = 
        { 
            status     : Status;
            score      : IModRef<int>
            samy       : Samy
            camera     : Camera
            water      : Water
            lights     : IModRef<Lighting.Source []>
            obstacles  : Obstacles
            goodies    : Goodies
            fireflies  : Fireflies
            playername : IModRef<string>
        }


