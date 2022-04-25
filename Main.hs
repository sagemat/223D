{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import SDL
import Linear (V4(..))
import Control.Monad (unless, when)
--world_objects_coord_list = [(0, 0, 0), (-5, 5, 0), (0, 10, 0)]


--Element table
oak_wood = Element '#' "Solid" "Brown" 750 0 True False
copper = Element 'o' "Solid" "Red-Brown" 9000 0 False True
pure_water = Element '~' "Liquid" "Blue" 1000 80 False False
------------------------------------------------------------

--World Objects
--world_objects = [Object oak_wood (1,1,1) [] 100 20, Object pure_water (5,7,0) [] 50 16, Object copper (-5,10,2) [] 500 20]
world_objects = [Object pure_water (7,7,0) [] 50 16, Object pure_water (7,7,1) [] 50 16, Object pure_water (7,7,2) [] 50 16, Object pure_water (7,6,2) [] 50 16, Object pure_water (7,8,2) [] 50 16, Object pure_water (7,9,2) [] 50 16, Object pure_water (7,5,2) [] 50 16]
--world_objects = [Object pure_water (1,1,1) [] 50 16]
data Element = Element { structure :: Char,
                         stateOfMatter :: String,
                         colour :: String,
                         weight :: Float,
                         transparency :: Float,
                         flamable :: Bool,
                         conductive :: Bool
                       }deriving Show

data Object = Object { element :: Element, 
                       coord :: (Float, Float, Float),
                       playerSeeCoord :: [(Int, Int)],
                       durability :: Float,
                       temperature :: Float
                     }deriving Show

data Colour = White | Red | Blue | Green | Yellow 


getElem :: [String] -> Int -> Int -> Maybe Char
getElem screen x y = if ((drop y (take (y + 1) screen)) == []) || (drop x (head (take (x + 1) (drop y (take (y + 1) screen))))) == [] then Nothing else Just (head (drop x (head (take (x + 1) (drop y (take (y + 1) screen))))))

getElem_elem elem string = head (drop elem string)
distance_xyz (x1, y1, z1) (x2, y2, z2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2)
distance_xy (x1, y1) (x2, y2) = sqrt ( fromIntegral ((x1 - x2)^2 + (y1 - y2)^2))


can_see distance_xyz = distance_xyz <= 2

screen_path = "C:\\Users\\Laptop-Alex\\Desktop\\Game\\Screens\\actor_screen.txt"

getElem_path :: String -> IO [String]
getElem_path string =
    do
        x <- readFile string
        return (lines x)

print_screen screen = putStrLn (unlines screen)

is_wall screen (x, y) = if (getElem screen x y) == Just '#' then True else False

find_size' screen (x, y) 1 = if (is_wall screen (x, y)) then find_size' screen (x, (y + 1)) 1 else (x, y - 1)
find_size' screen (x, y) acc = if (is_wall screen (x, y)) then find_size' screen ((x + 1), y) acc else find_size' screen ((x - 1), y) 1

find_size screen = find_size' screen (0, 0) 0

screen_length screen = fst (find_size screen)
screen_height screen = snd (find_size screen)

half_screen_distance screen = fromIntegral (round ((fromIntegral (screen_length screen)) / 2))

string_distance slope_list = show (distance_xy (head slope_list) (last slope_list))

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after

set :: [String] -> Int -> Int -> Char -> [String]
set screen x y char = 
    let
        line = getElem_elem y screen --screen !! y
        new_line = modify_list line x char
        new_screen = modify_list screen y new_line
    in
        new_screen

show_graph []     screen = screen
show_graph (x:xs) screen = let my_screen = set screen ((half_screen_distance screen) + fst x) ((half_screen_distance screen) + snd x + 1) '.'
                           in show_graph xs my_screen

{-
show_screen' []     screen c = screen
show_screen' (x:xs) screen c = let my_screen = set screen (fst x) (snd x) c
                               in show_screen' xs my_screen c

show_screen []     screen = screen
show_screen (x:xs) screen = let c = structure (element x)
                                my_screen = show_screen' (playerSeeCoord x) screen c
                            in show_screen xs my_screen 

-}

drawDot r (x, y) = SDL.drawPoint r (SDL.P (SDL.V2 x y))
relativeInputLocation (SDL.V2 x y) = (fromIntegral x, fromIntegral y)
absoluteInputLocation (SDL.P (SDL.V2 x y)) = (fromIntegral x, fromIntegral y) 

drawScreen' list r = mapM (\x -> drawDot r (fromIntegral (fst x), fromIntegral (snd x))) list

drawScreen listoflists r = mapM (\x -> drawScreen' (playerSeeCoord x) r) listoflists 

degrees_to_radians :: Float -> Float 
degrees_to_radians degrees  = (degrees / 320) * pi

radians_to_degrees :: Float -> Float
radians_to_degrees radians 
                 | radians_to_degrees_value < 0 = radians_to_degrees_value + 640
                 | radians_to_degrees_value >= 640 = radians_to_degrees_value - 640
                 | otherwise = radians_to_degrees_value
                 where radians_to_degrees_value = (radians / pi) * 320

slope_too_big_or_small :: Float -> Float -> Int
slope_too_big_or_small half_screen_length m = if ((abs m) <= 1) then round (half_screen_length * m) else 
                                                                                                        if (half_screen_length >= (abs m)) then round (half_screen_length / m) else 0                                     

find_slope_coord_stepx :: Int -> Float -> Int -> [(Int, Int)]
find_slope_coord_stepx 0     _ _    = [(0, 0)]
find_slope_coord_stepx steps m sign = let x = sign * steps
                                          y = round (m * (fromIntegral x))
                                in [(x, y)] ++ find_slope_coord_stepx (steps - 1) m sign ++ [(-x, -y)]

find_slope_coord_stepy :: Int -> Float -> Int -> [(Int, Int)]
find_slope_coord_stepy 0     _ _    = [(0, 0)]
find_slope_coord_stepy steps m sign = let y = sign * steps
                                          x = round ((fromIntegral y) / m)
                                in [(x, y)] ++ find_slope_coord_stepy (steps - 1) m sign ++ [(-x, -y)]


build_infinite_slope :: Float -> [(Int, Int)]
build_infinite_slope  0 = []
build_infinite_slope  half_screen_length = [(0, round half_screen_length)] ++ build_infinite_slope (half_screen_length - 1) ++ [(0, round (-half_screen_length))]

build_slope :: Float -> Float -> [(Int, Int)]
build_slope angle half_screen_length = let
                                             m = tan angle
                                             steps_x = abs (round (half_screen_length * sin (1.5*pi - angle))) --abs (slope_too_big_or_small half_screen_length m)
                                             steps_y = abs (round (half_screen_length * sin angle))
                                             slope_is_infinite = steps_x == 0  
                                         in
                                            -- (find_slope_coord steps m (-1)) ++ [(0, 0)] ++ (find_slope_coord steps m 1)
                                             if (slope_is_infinite) then build_infinite_slope half_screen_length
                                                                    else if (steps_x >= steps_y) then (find_slope_coord_stepx steps_x m (-1)) -- (find_slope_coord_stepx steps_x m (-1)) ++ [(0, 0)] ++ 
                                                                                                 else (find_slope_coord_stepy steps_y m (-1)) -- (find_slope_coord_stepy steps_y m (-1)) ++ [(0, 0)] ++ 
--final_coord screen current_coord = if (is_wall screen current_coord) then current_coord else final_coord func

truncate' :: Float -> Int -> Float
truncate' x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

{-
-- arata ca un arici de mare

actor_screen 320   320  _         = []
actor_screen 320   beta (x, y, z) = actor_screen 0 (beta + 1) (x, y, z)
actor_screen alpha beta (x, y, z) = let m = y / x -- if x > y then x / y else y / x sau cv de genul
                                        n = z / x -- aici la fel
                                        alpha_rad = degrees_to_radians alpha
                                        beta_rad  = degrees_to_radians beta
                                    in (if (truncate' m 1 == truncate' (tan alpha_rad) 1) && (truncate' n 1 == truncate' (tan beta_rad) 1)
                                        then [(alpha, beta)]
                                        else [])
                                       ++ actor_screen (alpha + 1) beta (x, y, z)   -}                            


actor_screen' :: Float -> Float -> Float -> Float -> Float -> [(Int, Int)]
actor_screen' alpha beta alpha_angle_offset beta_angle_offset initial_alpha_angle_offset
                      |(alpha_angle_offset < 0) && (beta_angle_offset < 0) = []
                      | alpha_angle_offset < 0 = actor_screen' alpha beta initial_alpha_angle_offset (beta_angle_offset - step) initial_alpha_angle_offset
                      | in_sight = [(alpha_plus_offset, beta_plus_offset)] ++ actor_screen' alpha beta (alpha_angle_offset - step) beta_angle_offset initial_alpha_angle_offset
                      | otherwise = actor_screen' alpha beta (alpha_angle_offset - step) beta_angle_offset initial_alpha_angle_offset
                      where alpha_plus_offset = round (alpha + alpha_angle_offset)
                            beta_plus_offset  = round (beta  + beta_angle_offset)
                            in_sight = (alpha_plus_offset > 0) && (alpha_plus_offset <= 319) && (beta_plus_offset > 0) && (beta_plus_offset <= 199)
                            step = 1

sharpAngleOfTwoAngles alpha beta = let difference = abs (alpha - beta) 
                                   in if difference > 320 then 640 - difference else difference

actorSeeAngle alpha beta = let difference = alpha - beta
                           in if (abs (640 + difference)) > 640 then abs (640 - difference) else abs difference                              

ajustSlope x y = if abs y > abs x then x else y

ajustAngle angle isgreater ispositive = if isgreater then 
                                                     if ispositive then 160 - angle
                                                                   else 480 - angle
                                                     else angle


player_view :: (Float, Float, Float, (Float, Float)) -> (Float, Float, Float) -> [(Int, Int)]                           
player_view (x0, y0, z0, (alpha, beta)) (x1, y1, z1) = let x = x1 - x0
                                                           y = y1 - y0
                                                           z = z1 - z0
                                                           isgreater  a b = abs a > abs b
                                                           ispositive a b = ajustSlope a b > 0
                                                           iszero = (x == 0) && (y == 0 || z == 0)
                                                           obj_slope_xy = y / x --ajustSlope y x --
                                                           obj_slope_xz = z / x --ajustSlope z x -- 
                                                       --    obj_slope_yz = z / y
                                                           omega = radians_to_degrees (atan obj_slope_xy) --ajustAngle (radians_to_degrees (atan obj_slope_xy)) (isgreater y x) (ispositive y x) --
                                                           gama  = radians_to_degrees (atan obj_slope_xz) --ajustAngle (radians_to_degrees (atan obj_slope_xz)) (isgreater z x) (ispositive z x) --
                                                       --    eta   = radians_to_degrees (atan obj_slope_yz)
                                                           omega_relative_alpha = actorSeeAngle alpha omega
                                                           gama_relative_beta   = actorSeeAngle beta  (gama + 160)
                                                           alpha_angle_offset = radians_to_degrees (2 * tan (1 / (2 * (sqrt (x^2 + y^2))) ))
                                                           beta_angle_offset  = radians_to_degrees (2 * tan (1 / (2 * (sqrt (x^2 + z^2))) ))
                                                        --   delta_angle_offset = radians_to_degrees (2 * tan (1 / (2 * (sqrt (y^2 + z^2))) ))
                                                       in  if iszero then [] else actor_screen' omega_relative_alpha gama_relative_beta alpha_angle_offset beta_angle_offset alpha_angle_offset
                        
player_view' (x0, y0, z0, (alpha, beta)) (x1, y1, z1) = let x = x1 - x0
                                                            y = y1 - y0
                                                            z = z1 - z0
                                                            isgreater  a b = abs a > abs b
                                                            ispositive a b = ajustSlope a b > 0
                                                            iszero = (x == 0) && (y == 0 || z == 0)
                                                            obj_slope_xy = y / x --ajustSlope y x --
                                                            obj_slope_xz = z / x --ajustSlope z x -- 
                                                        --    obj_slope_yz = z / y
                                                            omega = radians_to_degrees (atan obj_slope_xy) --ajustAngle (radians_to_degrees (atan obj_slope_xy)) (isgreater y x) (ispositive y x) --
                                                            gama  = radians_to_degrees (atan obj_slope_xz) --ajustAngle (radians_to_degrees (atan obj_slope_xz)) (isgreater z x) (ispositive z x) --
                                                       --    eta   = radians_to_degrees (atan obj_slope_yz)
                                                            omega_relative_alpha = actorSeeAngle alpha omega
                                                            gama_relative_beta   = actorSeeAngle beta  (gama + 160)
                                                            alpha_angle_offset = radians_to_degrees (2 * tan (1 / (2 * (sqrt (x^2 + y^2))) ))
                                                            beta_angle_offset  = radians_to_degrees (2 * tan (1 / (2 * (sqrt (x^2 + z^2))) ))
                                                        --   delta_angle_offset = radians_to_degrees (2 * tan (1 / (2 * (sqrt (y^2 + z^2))) ))
                                                        in "omega_relative_alpha = " ++ (show omega_relative_alpha) ++ " gama_relative_beta = " ++ (show gama_relative_beta) ++ " alpha_angle_offset = " ++ (show alpha_angle_offset) ++ " beta_angle_offset = " ++ (show beta_angle_offset)
        

sort_by_distance  _           []     = []
sort_by_distance actor_coord (p:xs) = (sort_by_distance actor_coord lesser) ++ [p] ++ (sort_by_distance actor_coord greater)
    where
        lesser  = filter (\x -> (distance_xyz (coord x) actor_coord) >= (distance_xyz (coord p) actor_coord)) xs
        greater = filter (\x -> (distance_xyz (coord x) actor_coord) < (distance_xyz (coord p) actor_coord)) xs
    
listoflists_to_list [] = []
listoflists_to_list (x : xs) = x ++ listoflists_to_list xs


setColor r Red    = SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound

myInputKEY 'Q' = KeycodeQ
myInputKEY 'W' = KeycodeW
myInputKEY 'A' = KeycodeA
myInputKEY 'S' = KeycodeS
myInputKEY 'D' = KeycodeD
myInputKEY '>' = KeycodeSpace
myInputKEY '<' = KeycodeLCtrl


eventIsKEYPressed event char = case eventPayload event of
                                  KeyboardEvent keyboardEvent ->
                                     keyboardEventKeyMotion keyboardEvent == Pressed &&
                                     keysymKeycode (keyboardEventKeysym keyboardEvent) == (myInputKEY char)
                                  _ -> False

correctAngle myangle
                | myangle >= 640 = myangle - 640
                | myangle < 0    = myangle + 640
                | otherwise = myangle
                                       


main :: IO ()
main = do
  initializeAll
  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 320 200
                                    , SDL.windowMode = Fullscreen
                                    , SDL.windowPosition = Centered
                                    }
  window <- createWindow "My SDL Application" winConfig
  setMouseLocationMode RelativeLocation
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer (0, 0, 0) (0, 0) world_objects
  destroyWindow window


loopMouseLocation (SDL.P (SDL.V2 x y))
              | x >= 179 = SDL.P (SDL.V2 1   y)--(0,   y)
              | x <= 0   = SDL.P (SDL.V2 178 y)--(179, y)
              | y >= 179 = SDL.P (SDL.V2 x   1)--(x,   0)
              | y <= 0   = SDL.P (SDL.V2 x 178)--(x, 179)
              | otherwise = SDL.P (SDL.V2 x y)
              

appLoop :: Renderer -> (Float, Float, Float) -> (Float, Float) -> [Object] -> IO ()
appLoop renderer (x, y, z) (alpha, beta) world_objects = do
  events <- pollEvents
  xyRelativeMouseLocation <- getRelativeMouseLocation
  xyAbsoluteMouseLocation <- getAbsoluteMouseLocation
  let (x_input_angle, y_input_angle) = relativeInputLocation xyRelativeMouseLocation
      new_alpha = correctAngle (x_input_angle + alpha) -- correctAngle x_input_angle--                                        
      new_beta  = correctAngle (y_input_angle + beta)  -- correctAngle y_input_angle
      correctMouseLocation = loopMouseLocation xyAbsoluteMouseLocation
      eventIsQPress event = eventIsKEYPressed event 'Q'
      eventIsWPress event = eventIsKEYPressed event 'W'
      eventIsAPress event = eventIsKEYPressed event 'A'
      eventIsSPress event = eventIsKEYPressed event 'S'
      eventIsDPress event = eventIsKEYPressed event 'D'
      eventIsSpacePress event = eventIsKEYPressed event '>'
      eventIsLCtrlPress event = eventIsKEYPressed event '<'
      qPressed = not (null (filter eventIsQPress events))
      wPressed = not (null (filter eventIsWPress events))
      aPressed = not (null (filter eventIsAPress events))
      sPressed = not (null (filter eventIsSPress events))
      dPressed = not (null (filter eventIsDPress events))
      spacePressed = not (null (filter eventIsSpacePress events))
      lctrlPressed = not (null (filter eventIsLCtrlPress events))
  --warpMouse WarpCurrentFocus (SDL.P (SDL.V2 90 90))
 -- xyAbsoluteMouseLocation <- correctMouseLocation
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  setColor renderer Red
  let world_objects_sorted = sort_by_distance (x, y, z) world_objects
      updated_world_objects_sorted = map (\p -> Object (element p) (coord p) (player_view (x, y, z, (new_alpha, new_beta)) (coord p)) (durability p) (temperature p)) world_objects_sorted
  drawScreen updated_world_objects_sorted renderer
  present renderer
  putStrLn (show (x, y, z, (alpha, beta)))
  when wPressed (appLoop renderer (x + 1, y, z) (new_alpha, new_beta) world_objects)
  when sPressed (appLoop renderer (x - 1, y, z) (new_alpha, new_beta) world_objects)
  when aPressed (appLoop renderer (x, y - 1, z) (new_alpha, new_beta) world_objects)
  when dPressed (appLoop renderer (x, y + 1, z) (new_alpha, new_beta) world_objects)
  when spacePressed (appLoop renderer (x, y, z + 1) (new_alpha, new_beta) world_objects)
  when lctrlPressed (appLoop renderer (x, y, z - 1) (new_alpha, new_beta) world_objects)
  unless qPressed (appLoop renderer (x, y, z) (new_alpha, new_beta) world_objects)
  SDL.destroyRenderer renderer

  SDL.quit                           

                                    
                               
                                      