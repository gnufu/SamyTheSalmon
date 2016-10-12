﻿namespace SamyTheSalmon

open System
open System.Collections.Generic

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Application

[<AutoOpen>]
module ModExtensions =

    // shortcuts to change mod
    let (<==) m v = transact (fun () -> Mod.change m v)

    let inline (<=+) m v = m <== (m |> Mod.force) + v

    let inline (<=-) m v = m <=+ -v

    let inline (<=*) m v = m <== (m |> Mod.force) * v

    let inline (<=/) m v = m <== (m |> Mod.force) / v

    module Mod =

        // maps the value with the given function and changes the mod cell
        let mapch (f : 'a -> 'a) (m : IModRef<'a>) =
            m <== (m |> Mod.force |> f) 

// helper function for creating boxes, wired boxes, quads and lines
module SomeHelpers =
    let box (color : C4b) (box : Box3f) = 

        let randomColor = color

        let indices =
            [|
                1;2;6; 1;6;5
                2;3;7; 2;7;6
                4;5;6; 4;6;7
                3;0;4; 3;4;7
                0;1;5; 0;5;4
                0;3;2; 0;2;1
            |]


        let positions = 
            [|
                V3f(box.Min.X, box.Min.Y, box.Min.Z)
                V3f(box.Max.X, box.Min.Y, box.Min.Z)
                V3f(box.Max.X, box.Max.Y, box.Min.Z)
                V3f(box.Min.X, box.Max.Y, box.Min.Z)
                V3f(box.Min.X, box.Min.Y, box.Max.Z)
                V3f(box.Max.X, box.Min.Y, box.Max.Z)
                V3f(box.Max.X, box.Max.Y, box.Max.Z)
                V3f(box.Min.X, box.Max.Y, box.Max.Z)
            |]
            |> Array.map (fun x -> x - box.Center)

        let normals = 
            [| 
                V3f.IOO;
                V3f.OIO;
                V3f.OOI;

                -V3f.IOO;
                -V3f.OIO;
                -V3f.OOI;
            |]

        IndexedGeometry(
            Mode = IndexedGeometryMode.TriangleList,

            IndexedAttributes =
                SymDict.ofList [
                    DefaultSemantic.Positions, indices |> Array.map (fun i -> positions.[i]) :> Array
                    DefaultSemantic.Normals, indices |> Array.mapi (fun ti _ -> normals.[ti / 6]) :> Array
                    DefaultSemantic.Colors, indices |> Array.map (fun _ -> randomColor) :> Array
                ]
        )

    let wireBox (color : C4b) (box : Box3d) = 
        let indices =
            [|
                1;2; 2;6; 6;5; 5;1;
                2;3; 3;7; 7;6; 4;5; 
                7;4; 3;0; 0;4; 0;1;
            |]

        let positions = 
            [|
                V3f(box.Min.X, box.Min.Y, box.Min.Z)
                V3f(box.Max.X, box.Min.Y, box.Min.Z)
                V3f(box.Max.X, box.Max.Y, box.Min.Z)
                V3f(box.Min.X, box.Max.Y, box.Min.Z)
                V3f(box.Min.X, box.Min.Y, box.Max.Z)
                V3f(box.Max.X, box.Min.Y, box.Max.Z)
                V3f(box.Max.X, box.Max.Y, box.Max.Z)
                V3f(box.Min.X, box.Max.Y, box.Max.Z)
            |]

        let normals = 
            [| 
                V3f.IOO;
                V3f.OIO;
                V3f.OOI;

                -V3f.IOO;
                -V3f.OIO;
                -V3f.OOI;
            |]

        IndexedGeometry(
            Mode = IndexedGeometryMode.LineList,

            IndexedAttributes =
                SymDict.ofList [
                    DefaultSemantic.Positions, indices |> Array.map  (fun i -> positions.[i]) :> Array
                    DefaultSemantic.Normals,   indices |> Array.mapi (fun ti _ -> normals.[ti / 6]) :> Array
                    DefaultSemantic.Colors,    indices |> Array.map  (fun _ -> color) :> Array
                ]

        )

    let quad (color : C4b) =
        let quad =
            let index = [|0;1;2; 0;2;3|]
            let positions = [|V3f(-1,-1,0); V3f(1,-1,0); V3f(1,1,0); V3f(-1,1,0) |]
            let uvs = [| V2f.OO; V2f.IO; V2f.II; V2f.OI |]

            IndexedGeometry(IndexedGeometryMode.TriangleList, index, 
                SymDict.ofList [
                    DefaultSemantic.Positions, positions :> Array
                    DefaultSemantic.Colors,  Array.init positions.Length (constF color  ) :> Array
                    DefaultSemantic.Normals, Array.init positions.Length (constF V3f.OOI) :> Array
                    DefaultSemantic.DiffuseColorCoordinates, uvs :> System.Array
                ], SymDict.empty)

        quad |> Sg.ofIndexedGeometry

    let lineGeometry (color : C4b) (positions : array<V3f>) =
        let indices =
            [| for i in 0 .. positions.Length - 2 do
                yield i
                yield i + 1
            |]

        IndexedGeometry(IndexedGeometryMode.LineList, indices, 
                SymDict.ofList [
                    DefaultSemantic.Positions, positions :> Array
                    DefaultSemantic.Colors,  Array.init positions.Length (constF color  ) :> Array
                ], SymDict.empty) 
            |> Sg.ofIndexedGeometry

    let lineLoopGeometry (color : C4b) (positions : array<V3f>) =
        let indices =
            if positions.Length = 0 then Array.zeroCreate 0
            else
                [| for i in 0 .. positions.Length - 2 do
                    yield i
                    yield i + 1
                    yield positions.Length - 1
                    yield 0
                |]

        IndexedGeometry(IndexedGeometryMode.LineList, indices, 
                SymDict.ofList [
                    DefaultSemantic.Positions, positions :> Array
                    DefaultSemantic.Colors,  Array.init positions.Length (constF color  ) :> Array
                ], SymDict.empty) 
            |> Sg.ofIndexedGeometry


    let normalMappedQuad (diffuse : ITexture) (normal : ITexture) =
        let quad =
            let index = [|0;1;2; 0;2;3|]
            let positions = [| V3f(-1,-1,0); V3f(1,-1,0); V3f(1,1,0); V3f(-1,1,0) |]
            let texcoords = [| V2f(0, 0); V2f(1, 0); V2f(1, 1); V2f(0, 1) |]
            let normals = [| V3f(0,0,1); V3f(0,0,1); V3f(0,0,1); V3f(0,0,1) |]
            let tangents = [| V3f(1,0,0); V3f(1,0,0); V3f(1,0,0); V3f(1,0,0) |]
            let bitangents = [| V3f(0,1,0); V3f(0,1,0); V3f(0,1,0); V3f(0,1,0) |]

            IndexedGeometry(IndexedGeometryMode.TriangleList, index, SymDict.ofList [DefaultSemantic.Positions, positions :> Array
                                                                                     DefaultSemantic.DiffuseColorCoordinates, texcoords :> Array
                                                                                     DefaultSemantic.Normals, normals :> Array
                                                                                     DefaultSemantic.DiffuseColorUTangents, tangents :> Array
                                                                                     DefaultSemantic.DiffuseColorVTangents, bitangents :> Array], SymDict.empty)

        quad
            |> Sg.ofIndexedGeometry
            |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant diffuse)
            |> Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant normal)


    let solidSphere (color : C4b) n =

        let generate level =
            let vertices = List<_>()
        
            let addVertex =
                let mutable index = 0 
                fun (p:V3f) ->
                    vertices.Add <| Vec.normalize p
                    index <- index + 1
                    index - 1

            let emitTriangle (indices:List<_>) tri =
                indices.Add tri

            let getMiddlePoint =
                let cache = Dictionary()
                fun p1 p2 -> 
                    let small,great = if p1 < p2 then int64 p1,int64 p2 else int64 p2,int64 p1
                    let key = (small <<< 32) + great
                    match cache.TryGetValue key with
                        | (false,_) -> 
                            let p1 = vertices.[p1]
                            let p2 = vertices.[p2]
                            let m = V3f.op_Multiply(0.5f,p1+p2)
                            let i = addVertex m
                            //cache.[key] <- i
                            i
                        | (true,v) -> v

            let t = (1.0 + Fun.Sqrt 5.0) / 2.0

            let v = 
                [
                    V3f(-1.0,  t, 0.0);  V3f( 1.0,  t, 0.0);  V3f(-1.0, -t, 0.0); V3f( 1.0, -t, 0.0)
                    V3f( 0.0, -1.0,  t); V3f( 0.0,  1.0,  t); V3f( 0.0, -1.0, -t); V3f( 0.0,  1.0, -t)
                    V3f(  t, 0.0, -1.0); V3f(  t, 0.0,  1.0); V3f( -t, 0.0, -1.0); V3f( -t, 0.0,  1.0)
                ] |> List.iter (ignore << addVertex)

            let indices = List<_>()

            let i = 
                [ 
                  (0, 11, 5); (0,  5,  1); (0 ,  1, 7 ); (0 , 7, 10); (0, 10, 11)
                  (1, 5 , 9); (5, 11,  4); (11, 10, 2 ); (10, 7, 6 ); (7, 1 , 8 )
                  (3,  9, 4); (3,  4,  2); (3 ,  2, 6 ); (3 , 6, 8 ); (3, 8 , 9 )
                  (4,  9, 5); (2,  4, 11); (6 ,  2, 10); (8 , 6, 7 ); (9, 8 , 1 ) 
                ] |> List.iter (emitTriangle indices)
        
            let rec run faces toGo = 
                if toGo = 0 then faces
                else
                    let newFaces = List()
                    for (v1,v2,v3) in faces do
                      let a = getMiddlePoint v1 v2
                      let b = getMiddlePoint v2 v3
                      let c = getMiddlePoint v3 v1
                  
                      emitTriangle newFaces (v1, a, c)
                      emitTriangle newFaces (v2, b, a)
                      emitTriangle newFaces (v3, c, b)
                      emitTriangle newFaces (a, b, c)
                    run newFaces (toGo - 1)

            let indices = run indices level

            let normals =
                let center = V3f.OOO
                let normals = List()
                for v in vertices do
                    normals.Add ( (v - center).Normalized )
                normals

            indices.ToArray() |> Array.collect (fun (a,b,c) -> [|a;b;c|]), vertices.ToArray(), normals.ToArray()   

        let (indices,positions,normals) = generate n
        IndexedGeometry(
            Mode = IndexedGeometryMode.TriangleList,
            IndexArray = indices,

            IndexedAttributes =
                SymDict.ofList [
                    DefaultSemantic.Positions, positions :> Array
                    DefaultSemantic.Normals, normals :> Array
                    DefaultSemantic.Colors, Array.init positions.Length (constF color) :> Array
                ]
        ) |> Sg.ofIndexedGeometry