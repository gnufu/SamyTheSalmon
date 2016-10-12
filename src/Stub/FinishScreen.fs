namespace SamyTheSalmon

open System.IO

open Aardvark.Application
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.GL
open Aardvark.SceneGraph


module FinishScreen =

    // path to highscore file
    let private path = @"..\resources\highscore.txt"

    // reads highscore file
    // an entry looks like: playername:score;
    let private readHighscore (p : string) =
        let content = File.ReadAllText p
        let entries = content.Split ';' |> Array.toList 
        let entries = entries |> List.take (entries.Length - 1)

        entries |> List.map (fun e -> 
            let x = e.Split ':'
            (x.[0],x.[1])
        )
    
    // formated highscore list for finish screen
    let private highscoreList (p : string) =
        let mutable highscoreList = ""
        for (n,s) in (readHighscore p) do
            highscoreList <- highscoreList + n + ": " + s + "\n"
        highscoreList

    // updates highscore
    // score calculation: score + health - time in sec
    let private updateHighscore (s : State) =

        let playerName = s.playername |> Mod.force
        let playerScore = (s.score |> Mod.force)
        let playerHealth = (s.samy.health |> Mod.force) |> int
        let time =
            Config.Application.stopwatch.Stop()
            Config.Application.stopwatch.ElapsedMilliseconds |> float |> (*) 0.001 |> int
        let highscore = (playerScore + playerHealth - time).ToString()

        // add player to highscore & sort it
        let sorted = 
            readHighscore path 
            |> List.append [(playerName, highscore)] 
            |> List.sortByDescending (fun (n,s) -> s.ToDouble())

        // write only first 10 back to file
        let mutable newHighscore = ""
        let mutable idx = 1
        for (n,s) in sorted do
            if idx < 10 then
                newHighscore <- newHighscore + n + ":" + s + ";"
                idx <- idx + 1

        // update highscore file
        File.WriteAllText(path, newHighscore)

    let playerWon (r : Runtime) (win : GameWindow) (s : State) =
        // stop playing sound
        GameContent.Media.player |> Audio.stopSound

        updateHighscore s
        highscoreList path |> RenderTasks.finishScreen r win s
        { s with status = EndOfGame }

    let playerLost (r : Runtime) (win : GameWindow) (s : State) =
         // stop playing sound
        GameContent.Media.player |> Audio.stopSound
        
        highscoreList path |> RenderTasks.playerLostScreen r win s  
        { s with status = EndOfGame }