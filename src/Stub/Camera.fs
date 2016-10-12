namespace SamyTheSalmon

open Aardvark.Base
open Aardvark.Base.Incremental


module Camera =

    // updates camera location
    let updateLocation (pos : V3d) (s : State) =
        let c = s.camera.view |> Mod.force
        if pos <> c.Location then
            s.camera.view <== c.WithLocation(pos)
        s

    // updates camera direction
    let updateDirection (newForward : V3d) (s : State) = 
        let cam = s.camera.view |> Mod.force
        if(cam.Forward <> newForward) then
            s.camera.view <== cam.WithForward(newForward)
        s