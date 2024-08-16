module Lib.Browser.Dom exposing (scrollToTop)

import Browser.Dom as BD
import Task


scrollToTop : msg -> Cmd msg
scrollToTop msg =
    BD.setViewport 0 0
        |> Task.perform (always msg)
