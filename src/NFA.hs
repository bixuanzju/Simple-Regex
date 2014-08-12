-- | NFA

module NFA
      (
      matches
      ) where

import Control.Monad.State
import Data.List (foldl')
import Data.Maybe

import Regx

type SID = Int

data NFA = NFA { rules :: [Rule]
               , currentStates :: [SID]
               , acceptStates :: [SID]
               } deriving Show

data Rule = Rule { fromState :: SID
                 , inputChar :: Maybe Char
                 , nextStates :: [SID]
                 } deriving Show

accepts :: NFA -> String -> Bool
accepts nfa = accepted . foldl' process nfaTemp
  where accepted nfa' = any (`elem` acceptStates nfa') (currentStates nfa')
        nfaTemp = nfa { currentStates = stateClosure (rules nfa) (currentStates nfa)}

process :: NFA -> Char -> NFA
process nfa c =
  case findRules c nfa of
    [] -> nfa { acceptStates = [] }
    rs -> let directStates = followRules rs
          in nfa { currentStates = stateClosure (rules nfa) directStates }

followRules :: [Rule] -> [SID]
followRules = concatMap nextStates

findRules :: Char -> NFA -> [Rule]
findRules c nfa = filter (ruleApplies c nfa) $ rules nfa

ruleApplies :: Char -> NFA -> Rule -> Bool
ruleApplies c nfa r = maybe False (c ==) (inputChar r) &&
                      fromState r `elem` currentStates nfa

stateClosure :: [Rule] -> [SID] -> [SID]
stateClosure r cs = cs ++ go [] cs
  where go acc [] = acc
        go acc ss = let ss' = followRules $ freeMoves r ss
                    in go (acc ++ ss') ss'

freeMoves :: [Rule] -> [SID] -> [Rule]
freeMoves rs ss = filter (\r ->
                            (fromState r `elem` ss) && isNothing (inputChar r))
                         rs

type SIDPool a = State [SID] a

nextID :: SIDPool SID
nextID = do (x:xs) <- get
            put xs
            return x

toNFA :: Pattern -> NFA
toNFA p = evalState (buildNFA p) [1..]

buildNFA :: Pattern -> SIDPool NFA
buildNFA p =
  do s1 <- nextID
     case p of
       Empty -> return $ NFA [] [s1] [s1]
       Literal c -> do s2 <- nextID
                       return $ NFA [Rule s1 (Just c) [s2]] [s1] [s2]
       Concat p1 p2 -> do nfa1 <- buildNFA p1
                          nfa2 <- buildNFA p2
                          let lambdaMoves = map (freeMoveTo nfa2) (acceptStates nfa1)
                          return $ NFA (rules nfa1 ++ lambdaMoves ++ rules nfa2)
                                       (currentStates nfa1)
                                       (acceptStates nfa2)
       Choose p1 p2 -> do s2 <- nextID
                          nfa1 <- buildNFA p1
                          nfa2 <- buildNFA p2

                          let lambdaMoves = [ freeMoveTo nfa1 s2
                                            , freeMoveTo nfa2 s2
                                            ]
                          return $ NFA (lambdaMoves ++ rules nfa1 ++ rules nfa2)
                                       [s2]
                                       (acceptStates nfa1 ++ acceptStates nfa2)
       Repeat p' -> do s2 <- nextID
                       nfa <- buildNFA p'
                       let initMove = freeMoveTo nfa s2
                           lambdaMoves = map (freeMoveTo nfa) (acceptStates nfa)
                       return $ NFA (initMove : rules nfa ++ lambdaMoves)
                                    [s2]
                                    (s2 : acceptStates nfa)
  where freeMoveTo nfa s = Rule s Nothing (currentStates nfa)

matches :: String -> String -> Bool
matches s = (`accepts` s) . toNFA . parseRegx
