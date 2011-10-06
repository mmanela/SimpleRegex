{-
  NFAtoDFA.hs - Created by Matthew Manela 2008
  Converts a NFA into a DFA
-}
module NFAtoDFA
    (
     convertToDFA
    )
    where

import RegexToNFA
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map



-- The state which we pass to build the DFA
data ConvertContext = ConvertContext { nfa :: FiniteMachine,
                                       trans :: [Transition],
                                       setMap :: Map.Map (Set Node) Integer,
                                       setStack :: [Set Node],
                                       begin :: Node,
                                       accept :: Set Node,
                                       nextNode :: Node
                                     } deriving (Show, Eq)
type ConvertState a = State ConvertContext a


-- Takes the ParseContext result from the call to parseRegex
-- and converts it to the NFA structure and returns it
convertToDFA machine = let context = snd $ runConversion machine
                           transTable = trans context
                           startNode = begin context
                           finalNodes = accept context
                           valueSet = alphabet.nfa$context
                       in FiniteMachine { table = transTable, 
                                          start = startNode, 
                                          final = finalNodes,
                                          alphabet = valueSet}

-- Given a NFA initialize the state of ConvertContext and start converting
runConversion machine =
    (runState $ runConversion') (ConvertContext { nfa = machine,
                                               trans = [],
                                               setMap = Map.empty,
                                               setStack = [],
                                               begin = 0,
                                               accept = Set.empty,
                                               nextNode = 0})
    where
      runConversion' = do
        st <- get
        let startSet = epsilonClosure (table.nfa$st) $ Set.singleton (start.nfa$st)
        checkNodeSet startSet
        processNodeSets
  
-- If we have more node sets on the stack process the top node set
processNodeSets = do
  st <- get
  case  null.setStack $ st of
    True -> return ()
    False -> buildTransitions >> processNodeSets


-- For the node set on the top of the stack find the destination nodes we can reach
-- for each value in our alphabet
buildTransitions = do
  values <- gets$alphabet.nfa
  mapM_ buildTransition (Set.toList values)
  st <- get
  let tailNodeSets = tail.setStack$st
  put $ st { setStack = tailNodeSets }


buildTransition val = do
  st <- get
  let oldTrans = table.nfa$st
      nodes = head.setStack$st
      newSet = moveClosure oldTrans (Just val) nodes
  when (not $ Set.null newSet) $ do
                        fromNode <- checkNodeSet nodes
                        toNode <- checkNodeSet newSet
                        checkTransition (fromNode, toNode, Just val)
  return ()

-- Check if we already added this transition if not add it
checkTransition ts = do
  tranList <- gets trans
  case ts `elem` tranList of
    True -> return ()
    False -> addTransition ts

addTransition ts = do
  st <- get
  let transList = trans st
  put $ st { trans = (ts:transList) }


-- Checks a NodeSet to see if it has a node number value
-- If it doesnt we assign it one and add it to the nodeSet stack
checkNodeSet nSet = do
  st <- get
  let nodesMap = setMap st
  case Map.member nSet nodesMap of
    True -> return (nodesMap Map.! nSet)
    False -> addNodeSet nSet


addNodeSet nSet = do
  st <- get
  let nodesMap = setMap st
      newNodesStack = (setStack st) ++ [nSet]
      newNode = nextNode st
      newNodesMap = Map.insert nSet newNode nodesMap
      dfaAccepts = accept $ st
      nfaAccepts = final.nfa $ st
      newAccepts = updateAcceptStates nfaAccepts dfaAccepts nSet newNode
  put $ st { setMap = newNodesMap, nextNode = (newNode + 1), setStack = newNodesStack, accept = newAccepts }
  return newNode
                 

-- Check if a given node set contains a accept state
-- if so add it to the dfa accept states
updateAcceptStates nfaAccepts dfaAccepts nSet nSetIndex = 
    case (Set.intersection nSet nfaAccepts) == Set.empty of
      True -> dfaAccepts
      False -> Set.insert nSetIndex dfaAccepts

-- Given an initial set of nodes, find the set of all nodes you can reach by taking 
-- transitions on epsilon only
epsilonClosure trans nodes = foldUntilRepeat Set.union Set.empty $
                             iterate (Set.fold (closure trans epsilon) Set.empty) nodes
      

-- Given a starting set of nodes the set of all nodes that you can reach on a given value
-- This includes epislonClosure on the desitination nodes
moveClosure trans value nodes = epsilonClosure trans $ 
                                Set.fold (closure trans value) Set.empty nodes


-- Given a node and a set of nodes, union orginal set with the set of nodes you can 
-- traverse to from node on the value
-- This sits on top of findToNode to let us use this more easily in fold's by taking an initial value
-- called oldSet
closure trans value nodes oldSet = Set.union (findToNodes trans value nodes) oldSet

-- Search the table of transitions to find all nodes you can reach given an initial set of nodes
findToNodes trans value fromNodes = foldr match Set.empty trans
    where 
      match (from, to, val) nodes
          | (from == fromNodes) && (val == value) = Set.insert to nodes
          | otherwise = nodes


-- Fold values in a list together until the result of a fold is the 
-- same as the current folded value
foldUntilRepeat _ z [] = z
foldUntilRepeat f z (x:xs) = let res = f z x in
                             case res == z of
                               True -> z
                               False -> foldUntilRepeat f res xs
