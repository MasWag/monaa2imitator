{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-|
Module      : Data.TimedAutomata.Imitator
Description : The data to represent a IMITATOR specification
CopyRight   : (c) Masaki Waga, 2019
License     : GPL-3
Maintainer  : masakiwaga@gmail.com
Stability   : experimental
-}
module Data.TimedAutomata.Imitator where
import Data.List.Utils
import Data.TimedAutomata.Types
import qualified Data.GraphViz as Dot
import qualified Data.GraphViz.Attributes.Complete as DotC
import qualified Data.GraphViz.Printing as P
import Data.List
import qualified Data.Text.Lazy as T
import Data.Maybe

-- | A variable of IMITATOR
data Variable =
  Variable String | -- variable without any initial value
  VariableWithV String String  | -- variable with initial value
  VariableWithC String Constraints -- variable with initial value constraint

instance Show Variable where
  show (Variable str) = "x" ++ str
  show (VariableWithV str _ ) = str
  show (VariableWithC str _ ) = str

type Label = String

-- | A constraint
data Constraints = LT String String | LE String String | GE String String | GT String String | AND Constraints Constraints | OR Constraints Constraints | True deriving Eq

instance Show Constraints where
  show (Data.TimedAutomata.Imitator.LT left right) = left ++ " < " ++ right
  show (Data.TimedAutomata.Imitator.LE left right) = left ++ " <= " ++ right
  show (Data.TimedAutomata.Imitator.GE left right) = left ++ " >= " ++ right
  show (Data.TimedAutomata.Imitator.GT left right) = left ++ " > " ++ right
  show (AND left right) = show left ++ " & " ++ show right
  show (OR left right) = show left ++ " | " ++ show right
  show Data.TimedAutomata.Imitator.True = "True"

-- | A variable update/reset
data Update = Update
  Variable -- | The updated variable 
  String           -- | The assigned value

instance Show Update where
  show (Update var newValue) = show var ++ " := " ++ newValue

-- | A location of IMITATOR
data Loc = Loc
  String               -- | The name of the location
  Constraints          -- | The invariant of the location
  [Transition] -- | The transitions from the location

instance Show Loc where
  show (Loc name invariant transitions) = "loc " ++ name ++ ": invariant " ++ show invariant ++ "\n" ++ shownTransitions
    where
      shownTransitions = unlines $ map (\t -> "\t" ++ show t) transitions

-- | The property
newtype Property = Unreachable [(Automata, Loc)]

instance Show Property where
  show (Unreachable locs) = 
    let showLoc (Automata automName _ _ _, Loc locName _ _) = "\tloc[" ++ automName ++ "] = " ++ locName
        shownLocs =  map showLoc locs
        shownLocsOr = map (++ " or") (init shownLocs) ++ [last shownLocs]
    in
      unlines $ ["property := unreachable"]  ++ shownLocsOr ++ [";"]

-- | A transition of IMITATOR
data Transition =  Transition
  Constraints       -- | The guard of the transition. 
  (Maybe Label)     -- | The synchronized label of the transition. If the transition is asynchronous, this should be 'Nothing'
  [Update]          -- | The list of the updates/resets of the transitions.
  String            -- | The name of the target location

instance Show Transition where
  show (Transition guard maybeLabel updates targetLabel) = 
    let label = 
          case maybeLabel of
            (Just labelStr) -> " sync " ++ labelStr
            _ -> ""
        shownUpdates =
          case updates of
            [] -> ""
            _ -> " do " ++ replace "]" "}" ( replace "[" "{" $ show updates )
    in
      "when " ++ show guard ++ label ++ shownUpdates ++ " goto " ++ targetLabel ++ ";"

-- | An automaton in IMITATOR
data Automata = Automata 
  String      -- | The name of this automaton
  [Label]       -- | The set of the synced labels
  [Loc] -- | The set of the locations of this automaton
  Loc   -- | The initial location

instance Show Automata where
  show (Automata name syncedLabels locations _) 
    = 
    let header = ["(************************************************************)",
                  "  automaton " ++ name,
                  "(************************************************************)"]
        synclabs = "synclabs: " ++ join ", " syncedLabels ++ ";"
        locString = init $ unlines $ map show locations
        footer = "end (* " ++ name ++ " *)"
    in
      unlines $ header ++ [synclabs, "", locString ++ footer]

-- | A model in IMITATOR. This is a network of automata
data Model = Model
  [Variable] -- | The set of the clock variables
  [Variable] -- | The set of the discrete variables
  [Variable] -- | The set of the parameters
  [Automata] -- | The set of the automata
  Property   -- | The property to be verified

showClocks :: [Variable] -> String
showClocks [] = ""
showClocks clocks =
  unlines $ map (\clock -> "\t" ++ show clock ++ ",") clocks ++ ["\t\t: clock;\n"]

showParams :: [Variable] -> String
showParams [] = ""
showParams params =
  unlines $ map (\clock -> "\t" ++ show clock ++ ",") params ++ ["\t\t: parameter;\n"]

showInitialState :: Model -> String
showInitialState (Model clockVariables discreteVariables parameters automata _) =
  unlines $ header ++ headerLoc ++ locStr ++ [""] ++ headerClock ++ clockStr ++ [""] ++ paramStr ++ [";", ""]
  where
    header = ["(************************************************************)",
               "(* Initial state *)",
               "(************************************************************)",
               "",
               "init :="]
    headerLoc = ["\t(*------------------------------------------------------------*)",
                 "\t(* Initial location *)",
                 "\t(*------------------------------------------------------------*)"]
    locStr = map (\ (Automata name _ _ (Loc locName _ _) ) -> "\t& loc[" ++ name ++ "] = " ++ locName) automata
    headerClock = ["\t(*------------------------------------------------------------*)",
                 "\t(* Initial clock constraints *)",
                 "\t(*------------------------------------------------------------*)"]
    showInitVariable (Variable _) = ""
    showInitVariable (VariableWithV name constant) = "\t& " ++ name ++ " = " ++ constant
    showInitVariable (VariableWithC name constraint) = "\t& " ++ show constraint
    clockStr = map showInitVariable clockVariables
    headerParam = ["\t(*------------------------------------------------------------*)",
                 "\t(* Parameter constraints *)",
                 "\t(*------------------------------------------------------------*)"]
    paramStr = headerParam ++ map showInitVariable parameters

instance Show Model where
  show (Model clockVariables discreteVariables parameters automata property) = 
    let automataStr = unlines $ map show automata
        clockStr    = showClocks clockVariables
        paramStr    = showParams parameters
        initStr     = showInitialState (Model clockVariables discreteVariables parameters automata property)
        propertyHeader = ["(************************************************************)",
                          "(* Property specification *)",
                          "(************************************************************)",
                          ""]
        footer      = ["(************************************************************)",
                       "(* The end *)",
                       "(************************************************************)",
                       "end"]

    in
      "var\n\n" ++ clockStr ++ paramStr ++ automataStr ++ initStr ++ unlines propertyHeader ++ show property ++ "\n" ++ unlines footer


fromTextSingle :: T.Text -> Constraints
fromTextSingle str
  | length  le > 1 = Data.TimedAutomata.Imitator.LE (T.unpack $ T.strip $ head le) (T.unpack $ T.strip $ last le)
  | length  ge > 1 = Data.TimedAutomata.Imitator.GE (T.unpack $ T.strip $ head ge) (T.unpack $ T.strip $ last ge)
  | length  lt > 1 = Data.TimedAutomata.Imitator.LT (T.unpack $ T.strip $ head lt) (T.unpack $ T.strip $ last lt)
  | length  gt > 1 = Data.TimedAutomata.Imitator.GT (T.unpack $ T.strip $ head gt) (T.unpack $ T.strip $ last gt)
  | otherwise = Data.TimedAutomata.Imitator.True
  where
    le = T.splitOn "<=" str
    lt = T.splitOn "<" str
    gt = T.splitOn ">" str
    ge = T.splitOn ">=" str

fromMONAALoc :: InputState -> [InputTransition] -> Loc
fromMONAALoc (Dot.DotNode nodeID _) edges = Loc nodeID Data.TimedAutomata.Imitator.True newTransitions
  where
    origTransitions = filter (\ (Dot.DotEdge fromNode _ _) -> fromNode == nodeID) edges
    newTransitions = map fromMONAATransition origTransitions
    fromMONAATransition :: InputTransition -> Transition
    fromMONAATransition (Dot.DotEdge fromNode toNode attributes) 
      = Transition guard label resets toNode
      where
        label = case find (DotC.sameAttribute (DotC.Label (DotC.StrLabel ""))) attributes of
          Nothing -> Nothing
          Just (DotC.Label (DotC.StrLabel str)) -> Just $ T.unpack str
          Just _ -> Nothing
        guard = case find (DotC.sameAttribute (DotC.UnknownAttribute "guard" "")) attributes of
          Nothing -> Data.TimedAutomata.Imitator.True
          Just (DotC.UnknownAttribute "guard" str) ->
            if T.head str == '{' && T.last str == '}' then
              let constraintTexts = T.splitOn "," $ T.init $ T.tail str
                  constraints = map fromTextSingle constraintTexts
              in
                case constraints of
                     [] -> Data.TimedAutomata.Imitator.True
                     x:xs -> foldl Data.TimedAutomata.Imitator.AND x xs
            else
              Data.TimedAutomata.Imitator.True
          Just _ -> Data.TimedAutomata.Imitator.True
        resets = case find (DotC.sameAttribute (DotC.UnknownAttribute "reset" "")) attributes of
          Nothing -> []
          Just (DotC.UnknownAttribute "reset" str) -> 
            if T.head str == '{' && T.last str == '}' then
              let resetTexts = T.splitOn "," $ T.init $ T.tail str
                  resetVariableNames = map (T.unpack.T.strip) resetTexts in
                map (\n -> Update (Variable n) "0") resetVariableNames
            else
              []
          Just _ -> []

fromMONAA :: InputTimedAutomaton -> Maybe Model
fromMONAA (Dot.DotGraph False Prelude.True (Just graphID) (Dot.DotStmts _ _ nodes edges)) =
  Just $ Model clocks discreteVariables parameters [automaton] property
  where
    initialMONAAStates = filter (\ (Dot.DotNode _ attributes) -> 
                                   isJust $ find (== DotC.UnknownAttribute "init" "1") attributes) nodes
    finalMONAAStates = filter (\ (Dot.DotNode _ attributes) -> 
                                  isJust $ find (== DotC.UnknownAttribute "match" "1") attributes) nodes
    finalMONAAStatesNames = map (\ (Dot.DotNode nodeID _) -> nodeID) finalMONAAStates
    finalLocs = filter (\(Loc name _ _) -> isJust $ find (== name) finalMONAAStatesNames) locs
    initialTransitions = map (\ (Dot.DotNode name attributes) -> 
                                Transition 
                                Data.TimedAutomata.Imitator.True 
                                Nothing
                                (map (`Update` "0") clocks)
                                name
                             ) initialMONAAStates
    dummyInitLoc = Loc "dummyInitLoc" Data.TimedAutomata.Imitator.True initialTransitions
    edgeAttributes = map (\(Dot.DotEdge _ _ attributes) -> attributes) edges
    guardClockNames = concatMap (\attribute -> 
                                  case find (DotC.sameAttribute (DotC.UnknownAttribute "guard" "")) attribute of
                                    Nothing -> []
                                    Just (DotC.UnknownAttribute "guard" str) ->
                                      if T.head str == '{' && T.last str == '}' then
                                        let constraintTexts = T.splitOn "," $ T.init $ T.tail str in
                                          map (T.unpack . head . T.splitOn " " . T.strip) constraintTexts
                                      else []
                                    Just _ -> []) edgeAttributes
    resetClockNames = concatMap (\attribute -> 
                                  case find (DotC.sameAttribute (DotC.UnknownAttribute "reset" "")) attribute of
                                    Nothing -> []
                                    Just (DotC.UnknownAttribute "reset" str) ->
                                      if T.head str == '{' && T.last str == '}' then
                                        let resetTexts = T.splitOn "," $ T.init $ T.tail str in
                                          map (\t -> 'x' : T.unpack (T.strip t)) resetTexts
                                      else []
                                    Just _ -> []) edgeAttributes
    clockNames = nub $ guardClockNames ++ resetClockNames
    labels = nub $ mapMaybe (\attribute ->
                               case find (DotC.sameAttribute (DotC.Label (DotC.StrLabel "")))  attribute of
                                 Nothing -> Nothing
                                 Just (DotC.Label (DotC.StrLabel str)) -> Just $ T.unpack str
                                 Just _ -> Nothing 
                            ) edgeAttributes
    clocks = map (`VariableWithV` "0") clockNames
    locs = map (`fromMONAALoc` edges) nodes
    discreteVariables = []
    parameters = []
    automaton = Automata automName labels (dummyInitLoc:locs) dummyInitLoc
    automName = T.unpack $ P.renderDot $ P.toDot graphID
    property = Unreachable $ map (automaton, ) finalLocs
fromMONAA _ = Nothing
