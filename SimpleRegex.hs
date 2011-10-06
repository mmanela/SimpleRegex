{-
  SimpleRegex.hs - Created by Matthew Manela 2008
  Simple regular expression matching
  A match is represented by a tuple of (Status, Match)
  If a match is successful you will have (True,"what you matched")
  If the match failed you will get (False,"")
-}
module SimpleRegex
    (
     (=~),
     (=~?),
     greedyMatch,
     shortMatch
    )
    where

import RegexToNFA
import NFAtoDFA
import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

type Pattern = String

(=~) = greedyMatch
(=~?) = shortMatch


-- Perform a greedy match when we try to match as much as we can
greedyMatch :: String -> Pattern -> (Bool,String)
greedyMatch str pattern = match str pattern longest

-- Perform a short match when we match as little as we can
shortMatch :: String -> Pattern -> (Bool,String)
shortMatch str pattern = match str pattern shortest


-- Convert pattern into a DFA and then call match using the specified matching function
match str pattern func = let machine = convertToDFA $ convertToNFA $ pattern in
                         doMatch func  machine (start $ machine) str
                         


-- Match bases on the based in function
doMatch func machine st [] = doAccept  machine st []
doMatch func machine st string =  func $ map (\f -> doMatch' st f []) (tails string)
    where
      doMatch' state [] soFar = doAccept machine st soFar
      doMatch' state (s:str) soFar = 
          case findTransition machine s state of
            Nothing -> doAccept machine state soFar
            Just (from, to, val) -> case doMatch' to str (soFar ++ [s]) of
                                      (False,_) -> case canAccept machine to of
                                                    True -> (True, soFar ++ [s])
                                                    False -> doMatch' to str (soFar ++ [s])
                                      (True,res) -> (True,res)


-- Get the shortest match
shortest matches = case  filter (\s->fst s) (sort matches) of
                     [] -> (False,"")
                     ms -> head ms

-- Get the longest match
longest matches = last.sort $ matches

-- Determie if the given state is an accept state
canAccept machine state = Set.member state (final machine)

-- Check if we are in an accept state if so return (True,what we matched)
-- otherwise return (False, "")
doAccept machine state soFar = if canAccept machine state
                               then (True,soFar) 
                               else (False, [])


-- Return a transition given a node and a value
findTransition machine value node = find thisTrans (table machine)
    where 
      thisTrans (from,to,val) = (node == from) && ((Just value) == val) 
