module Lib
    ( mainFunc
    ) where

import Data.List.Split ( splitOn, chunksOf )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Parallel.Strategies(using, parList, rseq)

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

chunkSize :: Int
chunkSize = 15

-- support versions of different parallel functions
parallelVersion :: Int
parallelVersion = 0

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

initalizePeople :: IO [Person]
initalizePeople = do
  contents <- readFile "people.csv"
  let records = tail $ words contents
  let parsed = map (splitOn ",") records
  return (map createPerson parsed)

createEvent :: [String] -> Event
createEvent [day,person,loc,start,end, chance]
  = Event {ePid = read person, loc = read loc, day = read day, startTime = read start, endTime = read end, chance = read chance}
createEvent _ = error "wrong arguments"

initalizeEvents :: IO [Event]
initalizeEvents = do
  contents <- readFile "dataset.csv"
  let records = tail $ words contents
  let parsed = map (splitOn ",") records
  return (map createEvent parsed)

createEventsMap :: [Event] -> Map.Map Int (Map.Map Int [Event])
createEventsMap xs = Map.map (Map.fromListWith (++) . map (\y -> (loc y, [y]))) nxt
  where nxt = Map.fromListWith (++) $ map (\x -> (day x, [x])) xs

createEventsMap2 :: [Event] -> Map.Map Int [Event]
createEventsMap2 xs = Map.fromListWith (++) $ map (\x -> (day x, [x])) xs

createLocMap :: Map.Map Int [Event] -> Map.Map Int (Map.Map Int [Event])
createLocMap eventsMap = Map.map (Map.fromListWith (++) . map (\y -> (loc y, [y]))) eventsMap

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
processEvent :: Event -> Map.Map Int HealthState -> Map.Map Int [Event] -> Bool
processEvent event hstates locMap
  | null overlap = False
  | state == Uninfected = and results
  | otherwise = False
  where state = hstates Map.! ePid event
        results = map (\(x,y) -> simulateEvent x y echance) $ Map.toList overlap
        overlap = findOverlappingEvents event locMap
        echance = chance event

-- Process an event, we will ignore the result of this if someone is not sick later
processEvent2 :: Event -> Map.Map Int [Event] -> Bool
processEvent2 event locMap
  | null overlap = False
  | otherwise = and results
  where results = map (\(x,y) -> simulateEvent x y echance) $ Map.toList overlap
        overlap = findOverlappingEvents event locMap
        echance = chance event

processEventResults :: Person -> Map.Map Int [Bool] -> Person
processEventResults p rmap = case results of
    Just xs -> if or xs then infected else p
    Nothing -> p
  where results = Map.lookup (pid p) rmap
        infected = Person {pid = pid p, hState = Infected, hStateDays = 0}

processEventResults2 :: Person -> Bool -> Person
processEventResults2 p infected = if infected then infectedRes else p
  where infectedRes = Person {pid = pid p, hState = Infected, hStateDays = 0}

-- This is the heart of the program, it processes the events for a day 
-- and updates the health status
processDailyEventsBasic :: [Person] -> Map.Map Int (Map.Map Int [Event]) -> Int -> [Person]
processDailyEventsBasic p emap day = do
  --Step 1, transition events
  let transitioned = map transitionHState p `using` parList rseq
  --Step 2, process all events for a given day
  let hstateslist = map (\x -> (pid x, hState x)) transitioned `using` parList rseq
  let hstates = Map.fromList hstateslist
  let locMap = emap Map.! day
  let eventsForDay = Map.foldr (++) [] locMap
  let processedEvents = map (\x -> (ePid x, [processEvent x hstates locMap])) eventsForDay `using` parList rseq
  let eventResults = Map.fromListWith (++) processedEvents
  --Step 3, process all event results
  map (\x -> processEventResults x eventResults) transitioned `using` parList rseq

processDailyEventsBasic2 :: [Person] -> Map.Map Int (Map.Map Int [Event]) -> Map.Map Int [Event] -> Int -> [Person]
processDailyEventsBasic2 p locMapFull eventMap day = do
  --Step 1, transition events
  let transitioned = map transitionHState p `using` parList rseq
  --Step 2, process all events for a given day
  let locMap = locMapFull Map.! day
  let eventsForDay = eventMap Map.! day
  let processedEvents = map (\x -> (ePid x, processEvent2 x locMap)) eventsForDay `using` parList rseq
  let changedPeople = Set.fromList $ map fst $ filter snd processedEvents
  --Step 3, process all event results
  map (\x -> processEventResults2 x ((hState x == Uninfected) && Set.member (pid x) changedPeople)) transitioned `using` parList rseq

processEventsChunk :: [Event] -> Map.Map Int [Event]-> [Int]
processEventsChunk events locMap = map fst filtered
  where filtered = filter snd $ map (\x -> (ePid x, processEvent2 x locMap)) events

processEventResultChunk :: [Person] -> Set.Set Int -> [Person]
processEventResultChunk people changedPeople = map (\x -> processEventResults2 x ((hState x == Uninfected) && Set.member (pid x) changedPeople)) people

processDailyEventsChunked2 :: [[Person]] -> Map.Map Int (Map.Map Int [Event]) -> Map.Map Int [Event] -> Int -> [[Person]]
processDailyEventsChunked2 p locMapFull eventMap day = do
  --Step 1, transition events
  let transitioned = map (map transitionHState) p `using` parList rseq
  --Step 2, process all events for a given day
  let locMap = locMapFull Map.! day
  let eventsForDay = chunksOf chunkSize $ eventMap Map.! day
  let processedEvents = map (\x -> processEventsChunk x locMap) eventsForDay `using` parList rseq
  let changedPeople = Set.fromList $ concat processedEvents
  --Step 3, process all event results
  map (\x -> processEventResultChunk x changedPeople) transitioned `using` parList rseq

processDailyEventsChunked :: [Person] -> Map.Map Int (Map.Map Int [Event]) -> Int -> [Person]
processDailyEventsChunked p emap day = do
  let pChunked = chunksOf chunkSize p
  let transitioned = map (map transitionHState) pChunked `using` parList rseq
  --Step 2, process all events for a given day
  let hstateslist = map (\x -> map (\y -> (pid y, hState y)) x) transitioned `using` parList rseq
  let hstates = Map.fromList $ concat hstateslist
  let locMap = emap Map.! day
  let eventsForDay = Map.foldr (++) [] locMap
  let eventsForDayChunked = chunksOf chunkSize eventsForDay
  let processedEvents = map (\x -> map (\y -> (ePid y, [processEvent y hstates locMap])) x)  eventsForDayChunked `using` parList rseq
  let eventResults = Map.fromListWith (++) (concat processedEvents)
  --Step 3, process all event results
  let out = map (\x -> map (\y -> processEventResults y eventResults) x) transitioned `using` parList rseq
  concat out

processDailyEvents :: [Person] -> Map.Map Int (Map.Map Int [Event]) -> Int -> [Person]
processDailyEvents p emap day
  | parallelVersion == 0 = processDailyEventsBasic p emap day
  | otherwise = processDailyEventsChunked p emap day

-- For each day, process daily events and update the health states
-- This is basically the core "loop" of the program that processes the events
-- for each day, one by one
mainFlow :: ([Person], Map.Map Int (Map.Map Int [Event]), Int) -> [Person]
mainFlow (p, _, 60) = p --haskell does not let you match against variables, so 60 here is the max number of days
mainFlow (p, m, n) =  mainFlow (processDailyEvents p m n, m, n+1)

-- For each day, process daily events and update the health states
-- This is basically the core "loop" of the program that processes the events
-- for each day, one by one
mainFlow2 :: ([Person], Map.Map Int [Event], Map.Map Int (Map.Map Int [Event]), Int) -> [Person]
mainFlow2 (p, _, _, 60) = p --haskell does not let you match against variables, so 60 here is the max number of days
mainFlow2 (p, eMap, locMap, n) =  mainFlow2 (processDailyEventsBasic2 p locMap eMap n, eMap, locMap, n+1)

mainFlow3 :: ([[Person]], Map.Map Int [Event], Map.Map Int (Map.Map Int [Event]), Int) -> [[Person]]
mainFlow3 (p, _, _, 60) = p --haskell does not let you match against variables, so 60 here is the max number of days
mainFlow3 (p, eMap, locMap, n) =  mainFlow3 (processDailyEventsChunked2 p locMap eMap n, eMap, locMap, n+1)

wasInfected :: Person -> Int
wasInfected p
    | state == Uninfected = 0
    | otherwise = 1
  where state = hState p

totalInfected :: [Person] -> Int
totalInfected p = sum $ map wasInfected p

mainFuncV1 :: IO ()
mainFuncV1 = do
  initalState <- initalizePeople
  putStrLn $ "Starting Number of Infected: " ++ show (totalInfected initalState)
  events <- initalizeEvents
  let eventsMap = createEventsMap events
  let finalState = mainFlow (initalState, eventsMap, 0)
  putStrLn $ "Total Number of Infected: " ++ show (totalInfected finalState)
  return ()

mainFuncV2 :: IO ()
mainFuncV2 = do
  putStrLn "Initializing People"
  initalState <- initalizePeople
  putStrLn $ "Starting Number of Infected: " ++ show (totalInfected initalState)
  events <- initalizeEvents
  putStrLn "Creating Events Map"
  let eventsMap = createEventsMap2 events
  putStrLn  "Creating Loc Map"
  let locMap = createLocMap eventsMap
  putStrLn "Running Main flow"


  
  let finalState = mainFlow2 (initalState, eventsMap, locMap, 0)
  putStrLn $ "Total Number of Infected: " ++ show (totalInfected finalState)
  return ()

mainFuncV3 :: IO ()
mainFuncV3 = do
  putStrLn "Initializing People"
  initalState <- initalizePeople
  putStrLn $ "Starting Number of Infected: " ++ show (totalInfected initalState)
  events <- initalizeEvents
  putStrLn "Creating Events Map"
  let eventsMap = createEventsMap2 events
  print $ head $ eventsMap Map.! 0
  putStrLn  "Creating Loc Map"
  let locMap = createLocMap eventsMap
  print $ head $ (locMap Map.! 0) Map.! 1
  putStrLn "Running Main flow"
  let finalState = mainFlow3 (chunksOf chunkSize initalState, eventsMap, locMap, 0)
  putStrLn $ "Total Number of Infected: " ++ show (totalInfected (concat finalState))
  return ()

--Two different main funcs to keep old slower version around for reference
mainFunc :: IO ()
mainFunc = mainFuncV3