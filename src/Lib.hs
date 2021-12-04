module Lib
    ( mainFunc
    ) where

import Data.List.Split ( splitOn )
import qualified Data.Map as Map

latencyPeriod :: Int
latencyPeriod = 2
infectiousPeriod :: Int
infectiousPeriod = 6
infectivity :: Float
infectivity = 0.1
susceptibility :: Float
susceptibility = 0.1
transmissibility :: Float
transmissibility = 0.1

rsp :: Float
rsp = infectivity*susceptibility*transmissibility
disMult :: Float
disMult = log (1-rsp)

data HealthState = Uninfected | Infected | Infectious | Recovered  deriving (Enum, Eq, Show)
data Person = Person {pid :: Int, hState :: HealthState, hStateDays :: Int} deriving (Show)
data Event = Event {ePid :: Int, loc :: Int, day :: Int, startTime :: Int, endTime :: Int, chance :: Float} deriving (Show)

transitionInfected :: Person -> Person
transitionInfected p
    | days < latencyPeriod = Person {pid = pid p, hState = Infected, hStateDays = days + 1}
    | otherwise = Person {pid = pid p, hState = Infectious, hStateDays = 0}
  where days = hStateDays p

transitionInfectious :: Person -> Person
transitionInfectious p
    | days < infectiousPeriod = Person {pid = pid p, hState = Infectious, hStateDays = days + 1}
    | otherwise = Person {pid = pid p, hState = Recovered, hStateDays = 0}
  where days = hStateDays p

-- Transion a person from one health state to another, each day
transitionHState :: Person -> Person
transitionHState p
    | state == Infected = transitionInfected p
    | state == Infectious = transitionInfectious p
    | otherwise = p
  where state = hState p

-- initial state of infectious is -1 so that we can update states on first day
createPerson :: [String] -> Person
createPerson [x,"0"] = Person {pid = read x, hState = Uninfected, hStateDays = 0}
createPerson [x,_] = Person {pid = read x, hState = Infectious, hStateDays = -1}
createPerson _ = error "wrong arguments"

initalizePeople :: IO (Map.Map Int Person)
initalizePeople = do
  contents <- readFile "people.csv"
  let records = tail $ words contents
  let parsed = map (splitOn ",") records
  let people = map createPerson parsed
  return $ Map.fromList $ map (\x -> (pid x, x)) people 

createEvent :: [String] -> Event
createEvent [day,person,loc,start,end, chance]
  = Event {ePid = read person, loc = read loc, day = read day, startTime = read start, endTime = read end, chance = read chance}
createEvent _ = error "wrong arguments"

initalizeEvents :: IO [Event]
initalizeEvents = do
  contents <- readFile "dataset.csv"
  let records = tail $ words contents
  let parsed = map (splitOn ",") records
  return $ map createEvent parsed

createEventsMap :: [Event] -> Map.Map Int (Map.Map Int [Event])
createEventsMap xs = Map.map (Map.fromListWith (++) . map (\y -> (loc y, [y]))) nxt
  where nxt = Map.fromListWith (++) $ map (\x -> (day x, [x])) xs

--Take an event, find the events for that location that overlap in time, return a map of
--Length of time -> Number of people
findOverlappingEvents :: Event -> Map.Map Int [Event] -> Map.Map Int Int
findOverlappingEvents event locMap = Map.fromListWith (+) $ map (\x -> (x,1)) lengths
  where locEvents = locMap Map.! loc event
        filtered = filter (\x -> startTime event <= endTime x && endTime event  <= startTime x) locEvents
        lengths = map (\x -> max (startTime event) (endTime x) - min (endTime event) (endTime x)) filtered

--Compute core disease formulat to see if the person got sick from the event
simulateEvent :: Int -> Int -> Float ->  Bool
simulateEvent numPeople time percentChance = percentChance < echance
  where echance = 1 - exp (timeAdjusted*pepFloat*disMult)
        timeAdjusted = fromIntegral time/15
        pepFloat = fromIntegral numPeople
-- simulateEvent _ _ _ = False 

-- Process an event, if someone is not unifected, they cant get sick, so ignore them
processEvent :: Event -> Map.Map Int Person -> Map.Map Int [Event] -> Bool
processEvent event people locMap
  | null overlap = False
  | state == Uninfected = and results 
  | otherwise = False
  where state = hState $ people Map.! ePid event
        results = map (\(x,y) -> simulateEvent x y echance) $ Map.toList overlap
        overlap = findOverlappingEvents event locMap
        echance = chance event        

processEventResults :: Person -> Map.Map Int [Bool] -> Person
processEventResults p rmap = case results of 
    Just xs -> if or xs then infected else p
    Nothing -> p
  where results = Map.lookup (pid p) rmap
        infected = Person {pid = pid p, hState = Infected, hStateDays = 0} 

-- This is the heart of the program, it processes the events for a day 
-- and updates the health status
processDailyEvents :: Map.Map Int Person -> Map.Map Int (Map.Map Int [Event]) -> Int -> Map.Map Int Person
processDailyEvents p emap day = do
  --Step 1, transition events
  let transitioned = Map.map transitionHState p
  --Step 2, process all events for a given day
  let locMap = emap Map.! day
  let eventsForDay = Map.foldr (++) [] locMap
  let eventResults = Map.fromListWith (++) $ map (\x -> (ePid x, [processEvent x p locMap])) eventsForDay
  Map.map (\x -> processEventResults x eventResults) transitioned

-- For each day, process daily events and update the health states
-- This is basically the core "loop" of the program that processes the events
-- for each day, one by one
mainFlow :: (Map.Map Int Person, Map.Map Int (Map.Map Int [Event]), Int) -> Map.Map Int Person
mainFlow (p, _, 60) = p --haskell does not let you match against variables, so 60 here is the max number of days
mainFlow (p, m, n) =  mainFlow (processDailyEvents p m n, m, n+1)

wasInfected :: Person -> Int
wasInfected p
    | state == Uninfected = 0
    | otherwise = 1
  where state = hState p

totalInfected :: Map.Map Int Person -> Int
totalInfected p = sum $ map wasInfected $ Map.elems p

mainFunc :: IO ()
mainFunc = do
  initalState <- initalizePeople
  let initialInfected = totalInfected initalState
  putStrLn $ "Starting Number of Infected: " ++ show initialInfected
  events <- initalizeEvents
  let eventsMap = createEventsMap events
  let finalState = mainFlow (initalState, eventsMap, 0)
  let numInfected = totalInfected finalState
  putStrLn $ "Total Number of Infected: " ++ show numInfected
  return ()