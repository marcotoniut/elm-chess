module Theme exposing (..)

darkSpaceColor   : String
darkSpaceColor   = "#769656" -- (118,150,86)
lightSpaceColor  : String
lightSpaceColor  = "#eeeed2" -- (238,238,210)
borderColor      : String
borderColor      = "#baca44" -- (186,202,68)
whitePlayerColor : String
whitePlayerColor = "#ffffff" -- (255,255,255)
blackPlayerColor : String
blackPlayerColor = "#000000" -- (0,0,0)

intToPx : Int -> String
intToPx = String.fromInt >> (\s -> s ++ "px")

tileSize : Int
tileSize = 70