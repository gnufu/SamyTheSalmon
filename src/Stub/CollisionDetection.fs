namespace SamyTheSalmon

open System
open Aardvark.Base
open Aardvark.Base.Incremental

module CollisionDetection =

    let private rand = Random ()

    // Compute barycentric coordinates (u, v, w) for 
    // point p with respect to triangle (a, b, c)
    // Realtime Collision Detection, p. 47
    let private barycentric (a : V2d) (b : V2d) (c : V2d) (p : V2d) =

        let v0 = b - a
        let v1 = c - a
        let v2 = p - a

        let d00 = V2d.Dot(v0, v0)
        let d01 = V2d.Dot(v0, v1)
        let d11 = V2d.Dot(v1, v1)
        let d20 = V2d.Dot(v2, v0)
        let d21 = V2d.Dot(v2, v1)

        let denom = d00 * d11 - d01 * d01
        let v = (d11 * d20 - d01 * d21) / denom
        let w = (d00 * d21 - d01 * d20) / denom
        let u = 1.0 - v - w;
        (u, v, w)

    // samples the given texture using bilinear interpolation
    let private sample (img : PixImage<byte>) (p : V2d) =

        let m = img.GetMatrix<C3b>()

        let clampedX = clamp 0.0 (float img.Size.X - 1.0) p.X
        let clampedY = clamp 0.0 (float img.Size.Y - 1.0) p.Y

        let value =
            // we have to take care in which triangle we are in an heigthmap quad
            match p.X <= p.Y with
            | true ->
                
                // get proper values for the triangle corners around samys position
                let a = V2d(floor clampedX, floor clampedY)
                let b = V2d(ceil clampedX, floor clampedY)
                let c = V2d(floor clampedX, ceil clampedY)
                let (u, v, w) = barycentric a b c p
                
                // get the colors of our triangle corners
                let colorA = m.[int64 a.X, int64 a.Y].ToC3d ()
                let colorB = m.[int64 b.X, int64 b.Y].ToC3d ()
                let colorC = m.[int64 c.X, int64 c.Y].ToC3d ()

                // compute interpolated color and z-value
                colorA * u + colorB * v + colorC * w

            | false ->
             
                // get proper values for the triangle corners around samys position
                let a = V2d(ceil clampedX, floor clampedY)
                let b = V2d(floor clampedX, ceil clampedY)
                let c = V2d(ceil clampedX, ceil clampedY)
                let (u, v, w) = barycentric a b c p
                
                // get the colors of our triangle corners
                let colorA = m.[int64 a.X, int64 a.Y].ToC3d ()
                let colorB = m.[int64 b.X, int64 b.Y].ToC3d ()
                let colorC = m.[int64 c.X, int64 c.Y].ToC3d ()

                // compute interpolated color and z-value
                colorA * u + colorB * v + colorC * w
        value

    // transforms world space coordinates to texture space
    let private toTextureSpace (tex : PixImage<'a>) (pos : V3d) =

        let bb = GameContent.Content.riverbedBB 

        let xNormalized = (pos.X - bb.Min.X) / bb.Size.X
        let yNormalized = (pos.Y - bb.Min.Y) / bb.Size.Y

        let xPixPos = (xNormalized * float tex.Size.X) + 0.5
        let yPixPos = (yNormalized * float tex.Size.Y) + 0.5

        V2d(xPixPos, yPixPos)

    // computes possible samy riverbed intersection point
    let private getTerrainHeight (pos : V3d) =

        let hMap = GameContent.Content.heightmap
        let bb = GameContent.Content.riverbedBB 

        // interpolate the colors around samys position to get smooth valid z-values
        let color = (toTextureSpace hMap pos) |> sample hMap
        bb.Min.Z + ((color.B * color.B) + 0.01) * bb.Size.Z

    // computes terrain normal
    let private getTerrainNormal (pos : V3d) =

        let nMap = GameContent.Content.normalmap

        // sample normal
        let normal = (toTextureSpace nMap pos) |> sample nMap
        (normal.ToV3d () * 2.0 - 1.0) |> Vec.normalize

    // returns a random vector for bouncing off the terrain
    let private randomBounce (n : V3d) =
     
        // random float (-1.0; 1.0)
        let rand () = rand.NextDouble() * 2.0 - 1.0

        // finds a basis for which the given vector will be the x axis
        let findBasis (baseX : V3d) =

            let baseY = baseX.AxisAlignedNormal()
            let baseZ = V3d.Cross(baseX, baseY)

            M33d.FromCols(baseX.Normalized, baseY.Normalized, baseZ.Normalized)
        
        // find a random vector pointing towards positive x
        let v = V3d(1.0, rand(), rand()) |> Vec.normalize

        // transform to normal space
        let basis = findBasis n
        basis * v

    // tests samy riverbed collision & changes samys movement & health properly
    let riverbed (newPos : V3d) (dt : float) (s : State) =

        // loose additional health if samy is outside of the water 
        if newPos.Z > 0.0 then
            s.samy.health <=- dt * (Config.Samy.decHealthFactor * 0.5)
        
        // returns if position and velocity is valid (i.e no collision occurs at p + v)
        let isValid (p : V3d) (v : V3d) =
            p.Z >= ((p + v) |> getTerrainHeight) + 0.015

        // check if collision occurred
        if isValid newPos V3d.Zero then
            newPos
        else
            // compute samy's current total velocity
            let view = s.camera.view |> Mod.force
            let sp = s.samy.speed |> Mod.force
            let mutable v = s.samy.velocity |> Mod.force

            let totalVel = view.Forward * sp.Y + view.Right * sp.X + v

            // get normal and collision point on terrain
            let cp = V3d(newPos.XY, (getTerrainHeight newPos) + 0.015)
            let n = getTerrainNormal newPos

            // first try to reflect off surface
            v <- (Vec.reflect totalVel n) * 0.5

            // if this would result in another collision just 
            // bounce off in direction of the normal an hope for the best
            if not (isValid cp v) then
                v <- n * v.Length

            // if player gets stuck above water, we want to jump around a bit
            if cp.Z >= 0.0 && v.Length < 0.1 then
                v <- (v + randomBounce n) |> Vec.normalize

            // update Samy's state
            s.samy.health <=- dt * Config.Samy.decHealthFactor
            s.samy.speed <== V2d.Zero
            s.samy.velocity <== v

            cp

    // tests for samy obstacle collisons & updates samys health accordingly
    let obstacles (pos : V3d) (s : State) (o : Obstacle) (dt : float) =
        let bb = o.boundingbox |> Mod.force
        match pos |> bb.Contains with
        | true ->
            // reduces samys health
            s.samy.health <=- dt * Config.Samy.decHealthFactor
            
            // compute intersection point
            let mutable min = System.Double.MinValue
            let mutable max = System.Double.MaxValue

            let origin = pos
            let view = s.camera.view |> Mod.force

            let r = FastRay3d(origin, view.Forward)
            r.Intersects(bb, &min, &max) |> ignore
            let newPos = origin + view.Forward * min

            // bounce
            let sp = s.samy.speed |> Mod.force
            let v = s.samy.velocity |> Mod.force

            let n = (newPos - bb.Center) |> Vec.normalize
            let v = view.Forward * sp.Y + view.Right * sp.X + v

            s.samy.speed <== V2d.Zero
            s.samy.velocity <== (Vec.reflect v n) * 0.5

            // return new position
            newPos


        | false -> pos

    // test for samy goody collisions & updates score accordingly
    let goodies (pos : V3d) (s : State) (g : Goodie) =
        // sphere - sphere intersection test
        // samy is treated like a sphere with his position as 
        // center and a fix sized radius of 0.1
        let bs = g.boundingbox |> Mod.force

        // TODO: ask for proper cast
        // calculate bounding sphere b/c this cast
        // goodieSg?LocalBoundingBox() : IMod<Box3d> fails
        let bSph = Sphere3d(bs.Center, bs.Distance bs.Max)

        // first calculate squared distacne between centers
        let cDis = bSph.Center - (s.samy.position |> Mod.force)
        let cDis = V3d.Dot(cDis, cDis)

        // Spheres intersect if center distance is less than sqaured sum of radii
        let radii = Math.Pow(bSph.Radius + 0.1, 2.0)
        if cDis <= radii then
            // add points to score
            s.score <== (s.score |> Mod.force) + 100

            // remove collected goodie 
            let gds = s.goodies.instances |> Mod.force |> List.filter(fun lg -> lg <> g)
            s.goodies.instances <== gds
     