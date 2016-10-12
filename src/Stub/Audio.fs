namespace SamyTheSalmon

open System.Media
open Aardvark.Base

module Audio =
    
    // load .wav-file
    let loadPlayer path = 
        let player = new SoundPlayer()

        // only load .wav-file if background sound is enabled
        if Config.Application.playSound then
            try
                player.SoundLocation <- path
                player.LoadAsync()
            with e -> 
                Log.warn "could not load sound: %s" path
        
        player

    // plays sound in a loop
    let playSound (player : SoundPlayer) =
        if Config.Application.playSound then
            player.PlayLooping()

    // stops sound playback
    let stopSound (player : SoundPlayer) =
        if Config.Application.playSound then
            player.Stop()
            player.Dispose()