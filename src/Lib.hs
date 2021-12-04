module Lib
    ( mainFunc
    ) where

import Data.List.Split ( splitOn )
import qualified Data.Map as Map

latencyPeriod :: Int
latencyPeriod = 2
infectiousPeriod :: Int
infectiousPeriod = 6

data HealthState = Uninfected | Infected | Infectious | Recovered  deriving (Enum, Eq, Show)
data Person = Person {pid :: Int, hState :: HealthState, hStateDays :: Int} deriving (Show)
data Event = Event {ePid :: Int, loc :: Int, day :: Int, startTime :: Int, endTime :: Int} deriving (Show)

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
  return $ map createPerson parsed

createEvent :: [String] -> Event
createEvent [day,person,loc,start,end]
  = Event {ePid = read person, loc = read loc, day = read day, startTime = read start, endTime = read end}
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

-- This is the heart of the program, it processes the events for a day 
-- and updates the health status
processDailyEvents :: [Person] -> Map.Map Int (Map.Map Int [Event]) -> Int -> [Person]
processDailyEvents p _ _ = do
  let transitioned = map transitionHState p
  transitioned

-- For each day, process daily events and update the health states
-- This is basically the core "loop" of the program that processes the events
-- for each day, one by one
mainFlow :: ([Person], Map.Map Int (Map.Map Int [Event]), Int) -> [Person]
mainFlow (p, _, 60) = p --haskell does not let you match against variables, so 60 here is the max number of days
mainFlow (p, m, n) =  mainFlow (processDailyEvents p m n, m, n+1)

wasInfected :: Person -> Int
wasInfected p
    | state == Uninfected = 0
    | otherwise = 1
  where state = hState p

totalInfected :: [Person] -> Int
totalInfected p = sum $ map wasInfected p

mainFunc :: IO ()
mainFunc = do
  initalState <- initalizePeople
  events <- initalizeEvents
  let eventsMap = createEventsMap events
  let finalState = mainFlow (initalState, eventsMap, 0)
  let numInfected = totalInfected finalState
  putStrLn $ "Total Number of Infected: " ++ show numInfected
  return ()