-- Text-Based RPG --
-- Authors: Keng-Ming Sheu, Fareez Sanif, Johnson Nguyen
import Data.List
import Data.Char

type Location = String
type Direction = String
type Item = String
type Response = String
type Mode = String --"World” | "Combat”
type Object = String
type Property = String
type Value = String --Needs to be defined this way for get and put to work


{-
“objects_in” means in Object room there are Value thing to interact with




“n” means Object room has Value room to the north
“s” means Object room has Value room to the south
“e” means Object room has Value room to the east
“w” means Object room has Value room to the west




“strength” means Object has strength Value



“location” means that Object in in location Value



“health” mean Object has health Value, if health is zero Object is dead
-}
type Facts = [(Object, Property, Value)]
facts :: Facts
facts = [
    ("sword", "strength", "20"),
    ("sword", "location", "entrance"),
    ("sign", "location", "entrance"),
    ("potion", "location", "lobby"),
    ("entrance", "s", "lobby"),
    
    
    ("myself", "location", "entrance"),
    ("myself", "health", "100"),
    
    ("lobby", "s", "goal"),
    ("lobby", "n", "entrance"),
    ("lobby", "e", "east-room"),
    ("lobby", "w", "west-room"),



    ("east-room", "w", "lobby"),
    ("east-room", "n", "monster-room"),
     
    ("west-room", "e", "lobby"),
    ("west-room", "objects_in", "potion"),
    
    ("potion", "strength", "100"),
    
    ("monster-room", "s", "east-room"),
    
    ("mom_boss", "location", ""),
    ("mom_boss", "health", "200"),
    ("treasure", "location", "goal"),
    ("goal", "n", "lobby")
    ]

type World = (Mode, Facts, Response)
world :: IO (Mode, Facts, Response)
world = return ("World", facts,  "")

main :: IO (String)
 
main = start

start :: IO (String)
start = do
    putStrLn intro
    putStrLn instructions
    play_game $ return ("World", facts, "")
    return "Goodbye!"

game_over :: Facts -> Bool
game_over facts =
    let my_location = get "location" "myself" facts
        treasure_location = get "location" "treasure" facts
    in my_location == "dead" || (my_location == "entrance" && treasure_location == "myself")

intro =
    "Welcome to the World of Haskellia, a fantasy land full of dreams!\n" ++
    "You are a treasure hunter in search of booty and your journey throughout\n" ++
    "the land brings you to the Cave Of Logic, which houses the greatest treaure\n" ++
    "Be on your toes as the Cave Of Logic is a den full of dangerous monsters that eat unprepared travelers\n" ++
    "Your goal is to search and discover the treasure, take down all that stands in your path and use your wits to navigate this unknown territory\n" ++
    "Your possessions consist of only one sword and a bag of limited healing potions\n" ++
    "Try not to die.\n"

instructions =
    "Enter commands using one or two words.\n" ++
    "Available commands are:\n" ++
    "main               -- to start the game.\n" ++
    "n  s  e  w         -- to go in that direction.\n" ++
    "take object        -- to pick up the named object.\n" ++
    "look               -- to look around you again.\n" ++
    "i                  -- to see your inventory (what you are holding).\n" ++
    "quit               -- to end the game and quit."
    
combat_instructions = "You are now in combat mode.\n Available commands are:\n" ++
                      "attack             -- to attack an enemy.\n" ++
                      "defend             -- to block an attack.\n" ++
                      "heal               -- heals if potion.\n" ++
                      "run                -- attempt to flee the battle."
                      

play_game :: IO (World) -> IO (World)
play_game world = do
    (mode, facts, response) <- world
    putStrLn response
    putStrLn ""
    if game_over facts
        then return ("", [], "")
        else do
            putStr "command> "
            command <- getLine
            if command == "quit"
                then return (mode, facts, "Quitting.")
                else  play_game $ return (do_command command mode facts)

do_command :: String -> Mode -> Facts -> World
do_command cmd mode facts
            | (cmd == "n" && mode == "World") = go "n" facts
            | (cmd == "s" && mode == "World") = go "s" facts
            | (cmd == "e" && mode == "World") = go "e" facts
            | (cmd == "w" && mode == "World") = go "w" facts
            | (cmd == "look" && mode == "World") = look facts
            | (cmd == "i" && mode == "World") = (mode, facts, inventory facts)
            | (cmd == "attack" && mode == "Combat") = attack facts
            | (cmd == "defend" && mode == "Combat") = defend facts
            | (cmd == "heal" && mode == "Combat") = heal facts
            | (cmd == "run" && mode == "Combat") = (mode, facts, "Hah! Jokes on you! You're not getting away!")
            | (mode == "Combat" && elem cmd ["n", "s", "e", "w", "look", "i"]) = (mode, facts, "You are stuck in combat!")
            | (cmd == "quit") = (mode, facts, "quit")
            | otherwise = do_command2 cmd mode facts

do_command2 :: String -> Mode ->  Facts -> World
do_command2 cmd mode facts
    | isPrefixOf "take " cmd =
        pickup mode (tail $ snd $ span isLetter cmd) facts
    | otherwise = (mode, facts, "Well fuck you: " ++ cmd)

attack :: Facts -> World
attack facts = 
    let boss_hp = read (get "health" "mom_boss" facts)
        player_hp = read (get "health" "myself" facts)
        sword_location = get "location" "sword" facts
        monster_status = get "status" "mom_boss" facts 
        remaining_boss_hp = boss_hp - 20
        remaining_player_hp = player_hp - 30
        new_facts = put "mom_boss" "health" (show remaining_boss_hp) facts 
        newer_facts = put "myself" "health" (show remaining_player_hp) new_facts
        stunned_fact = put "myself" "health" (show player_hp) new_facts
    in  if (sword_location /= "myself")
        then ("World", put "myself" "location" "dead" facts, "You tried to attack but did no damage because you have no weapon. The momboss countered with a slap and you died.")
        else
           if remaining_boss_hp <= 0
           then ("World", put "mom_boss" "location" "hell" newer_facts, "You hit the enemy and drop its HP to 0. Mom boss is dead!! \n Exiting Combat mode. ")
           else 
               if monster_status == "stunned"
               then ("Combat", put "mom_boss" "status" "normal" stunned_fact , "You land a free hit on the stunned mom_boss, but it awakens! \nEnemy HP: " ++ (show remaining_boss_hp) ++ " ,Your HP" ++ (show player_hp))   
               else if remaining_player_hp > 0
                    then ("Combat", newer_facts, "You and the enemy exchange blows! \n Enemy HP Remaining: " ++ (show remaining_boss_hp) ++ " Your HP Remaining: " ++ (show remaining_player_hp))
                    else ("World", put "myself" "location" "dead" newer_facts, "Your health has dropped to 0, you're dead!")
                    
defend :: Facts -> World
defend facts  
             |((get "status" "mom_boss" facts) == "stunned") = ("Combat", facts, "The monster is still stunned, why are you defending?")
             |otherwise =("Combat", put "mom_boss" "status" "stunned" facts, "You mananged to counter the attack. \n mom_boss has been stunned!")

heal :: Facts -> World
heal facts = let my_health = read (get "health" "myself" facts)
                 new_health = my_health + 80
             in ("Combat", put "myself" "health" (show new_health) facts, "You healed 100.\nBut mom_boss slaps you for 20 damage. \nYou now have " ++ show new_health ++ " health.")
    
pickup :: Mode -> [Char] -> Facts -> World
pickup mode "sign" facts = (mode, facts, "This sign is bolted down solid, you can't take it.") 
pickup mode "treasure" facts = 
    let here =  get "location" "myself" facts
        there = get "location" "treasure" facts
     in if here == there
     then (mode, ("mom_boss", "location", "lobby"):(put  "treasure" "location" "myself" facts), "Picked up " ++ "treasure" ++ ".")
                
     else (mode, facts, "Cannot find " ++ "treasure" ++ ".")    
pickup mode item facts =
     let here =  get "location" "myself" facts
         there = get "location" item facts
     in if here == there
     then (mode, (put item "location" "myself" facts), "Picked up " ++ item ++ ".")
                
     else (mode, facts, "Cannot find " ++ item ++ ".")
     
inventory :: Facts -> String       
inventory list = 
                  let things = ["You have a " ++ object ++ ". " | (object, property, value) <- list, property == "location", value == "myself", object /= "myself"]
                  in if things == [] then "Your inventory is empty"
                  else intercalate "\n" things


go :: String -> Facts -> World
go direction facts = do
    let my_location = get "location" "myself" facts
    let new_location = get direction my_location facts
    if new_location /= "Not Found."
        then
            let new_facts = put "myself" "location" new_location facts
            in if get "location" "mom_boss" facts == new_location
               then  ("Combat", new_facts, "I am mom boss, and it is time for you to die. \n\n" ++ combat_instructions)
               else ("World", new_facts, "Moved to " ++ new_location ++ ". " ++ (describer_helper new_location facts))
        else ("World", facts, "Cannot go this way")




look :: Facts -> World
look list = let things = items_here list
            in if things == []
            then ("World", list, describe list)
            else ("World", list, describe list ++ "\n\n" ++ things)

            
items_here :: Facts -> String       
items_here list = let here = get "location" "myself" list
                      things = ["There is a " ++ object ++ " here. " | (object, property, value) <- list, property == "location", value == here, object /= "myself"]
                  in intercalate "\n" things



describe :: Facts -> String      
describe facts = describer_helper here facts
                where here = get "location" "myself" facts


describer_helper "entrance" facts = if get "location" "treasure" facts == "myself"
                                       then description "win_message"
                                       else description "entrance"
describer_helper "goal" facts = if get "location" "treasure" facts == "myself"
                                   then description "no_more_daki..."
                                   else description "goal"

describer_helper here _ = description here                                  
             
description "no_more_daki..." = "Wao, you really did take the dakimakura.... \nIt's not here anymore.\nIt's in your hand."

description "win_message" = "You did it! You brought it home with you!!!~~\nYou win!!!!"




             
description "entrance" = "You have found yourself in front of the entrance.\n" ++
                       "What awaits beyond remains to be seen.\n" ++
                       "You see the entrance to the south.\n"
                       
description "lobby" = "You see a grand lobby in front of you. \n" ++
                    "You wonder why this cave has such a large lobby with \n" ++
                    "four exits to it immediately upon entering.\n\n" ++
                    "Oh well. \nThe cave continues in the north, east, south and west directions."

description "goal" = "All right! You found it!!! \nThe Kirino dakimakura you've always wanted.\n" ++
                     "You can head out the way you came, from the north."   
                     
description "east-room" = "This is the room to the east of the lobby. \nIt sure is interesting.\n" ++
                          "Oh, there is a door to the north."   

description "monster-room" = "This place is omniously called monster-room, but you don't see any monsters.\n" ++
                             "All you see is a note. It says: \n" ++
                             "\"Make sure you eat your vegetables!  <3\""   

description "west-room" = "You head west further in the cave.\n" ++
                          "This room kinda surprises you.... It's full of Gundam models, books, Blu-rays\n" ++
                          "and figures of what seem to be Chinese cartoon characters. \nBut you do see something shiny over there."                             
                        
                        
description _ = "This is an ordinary room."

-- look facts

-- "get" finds the value of a key in a (key, property, value) list
get :: (Eq p, Eq o) => p -> o -> [(o, p, Value)] ->  Value
get prop key list = case lookup key filtered of
                        Just result -> result
                        Nothing -> "Not Found."
                    where filtered = [(object, val) | (object, property, val) <- list, prop == property, object == key]
-- Tests:
-- get "location" "myself" facts

-- "put" removes (object, property, oldValue) and adds (object, property, newValue) to list
put :: (Eq t, Eq t1) => t ->  t1 -> t2 -> [(t, t1, t2)] -> [(t, t1, t2)]
put object property value list =
    let without = filter (\ (x, y, z) -> (x /= object || y /= property)) list
    in (object, property, value) : without

-- Tests:
-- put "myself" "location" "banana" facts