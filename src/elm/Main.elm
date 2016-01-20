module Main where

import Effects
import StartApp
import Task
import Http
import Videos.Main exposing (init, update, view)



app =
    StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs = []
        }


main =
    app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks
