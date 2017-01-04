(*

SAMY THE SALMON

CG-UE 2016

Martin Mautner (1127229)
Lisa Kellner (1428183)

*)

open System
open System.IO
open System.Reflection

[<EntryPoint>]
let main argv =

    // set the current working directory to .exe (for relative paths to work)
    Environment.CurrentDirectory <- Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)

    // start the game
    SamyTheSalmon.Application.run()
    0 
