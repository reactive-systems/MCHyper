{-# LANGUAGE RecordWildCards #-}

-- based in part on the Aiger parser by Michael C. Schröder

module Aiger
    ( Aiger(..), Literal, Variable, Latch, Gate
    , emptyAiger
    , parseAigerFromFile
    , parseAigerFromString
    , printAiger
    , compose
    , strategy_compose
    , existential_strategy_compose
    , refinedSymbolList
    , fillLabels
    , fillStrategyLabels 
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.String.Utils

import Text.ParserCombinators.Parsec

import Debug.Trace
debug_trace _ = id
--debug_trace = trace

-----------------------------------------------------------------------

type Literal  = Int
type Variable = Int

type VarType = Char
type RawSymbol = (VarType, Int, String) -- from symbol tables
type Symbol = (Variable, String) -- translated numbers

type Latch = (Variable, Literal, Literal)
type Gate = (Literal, Literal, Literal)

data Aiger = Aiger { miloa :: [Int] -- [m,i,l,o,a]
                   , inputs :: [Literal]
                   , latches :: [Latch]
                   , outputs :: [Literal]
                   , gates :: [Gate]
                   , symbols :: [RawSymbol]
                   , comment :: String
                   } deriving (Show)

emptyAiger = Aiger [0,0,0,0,0] [] [] [] [] [] ""

-----------------------------------------------------------------------

parseAigerFromString :: String -> Either ParseError Aiger
parseAigerFromString = parse aiger "stdin"
parseAigerFromFile :: FilePath -> IO (Either ParseError Aiger)
parseAigerFromFile = parseFromFile aiger

aiger :: Parser Aiger
aiger = do
    [m,i,l,o,a] <- header
    Aiger [m,i,l,o,a]
      <$> rows i literal
      <*> rows l latch
      <*> rows o literal
      <*> rows a gate
      <*> many (try (newline >> rawSymbol))
      <*> many (noneOf "") -- just read comment as a string
    
header :: Parser [Int]
header = string "aag" >> count 5 (space >> integer)

rows :: Int -> Parser a -> Parser [a]
rows n p = count n (newline >> p)

latch :: Parser Latch
latch = (,,) <$> literal <*> (space >> literal) <*> (option 0 (char ' ' >> literal))

gate :: Parser Gate
gate = (,,) <$> literal <*> (space >> literal) <*> (space >> literal)

rawSymbol :: Parser RawSymbol
rawSymbol = (,,) <$> varType <*> integer <*> (space >> many (noneOf "\n\r"))

varType :: Parser VarType
varType = char 'i' <|> char 'l' <|> char 'o'

variable :: Parser Variable
variable = fromIntegral . flip div 2 <$> mfilter even integer

literal :: Parser Literal
literal = fromIntegral <$> integer

integer :: Parser Int
integer = read <$> many1 digit

-----------------------------------------------------------------------

printAiger :: Aiger -> String
printAiger (Aiger miloa inputs latches outputs gates symbols comment) = 
        "aag " ++ (intercalate " " . map show) miloa ++ "\n"
        ++ concatMap (\i -> show i ++ "\n")        inputs
        ++ concatMap (\(a,b,c) -> show a ++ " " ++ show b ++ " " ++ show c ++ "\n")  latches
        ++ concatMap (\o -> show o ++ "\n")        outputs
        ++ concatMap (\g -> printGate g ++ "\n")   gates
        ++ concatMap (\s -> printSymbol s ++ "\n") symbols
        ++ comment

printGate (x,y,z) = showint x ++ showint y ++ show z

printSymbol (varType,num,label) = varType : showint num ++ label

showint i = show i ++ " "

-----------------------------------------------------------------------

removeDuplicateLabels ((t,v,l):(t',v',l'):xr) = 
        if (t==t') && (v==v') 
            then (t, v, if head l == ']' then l' else l) : removeDuplicateLabels xr 
            else (t,v,l) : removeDuplicateLabels ((t',v',l'):xr) 
removeDuplicateLabels xs = xs

fillLabels :: Aiger -> Aiger
fillLabels (Aiger miloa  inputs  latches  outputs  gates  symbols  comment ) =
    let iSymbols  = map (\(_,n) ->        ('i',n,"]i" ++ show n)) $ zip inputs  [0..]
        lSymbols  = map (\((_,_,_),n) -> ('l',n,"]l" ++ show n)) $ zip latches [0..]
        oSymbols  = map (\(_,n) ->        ('o',n,"]o" ++ show n)) $ zip outputs [0..]
        symbols'  = sortBy compare $ iSymbols ++ lSymbols ++ oSymbols ++ symbols
        symbols'' = removeDuplicateLabels symbols'
    in Aiger miloa inputs latches outputs gates symbols'' comment

setOfInputLabels :: [(Char,Int,String)] -> [String]
setOfInputLabels symbols =
    let systemInputSymbols = filter (\(varType, index, label) -> varType == 'i') symbols -- "[('i',1,\"test\"),('i',2,\"lala\")]"
    in map (\(_, _, label) -> label ++ "_") systemInputSymbols

fillStrategyLabels :: Aiger -> Aiger -> Aiger
fillStrategyLabels (Aiger [m,  i,  l,  o,  a]  inputs  latches  outputs  gates  symbols  comment) -- strategy
                   (Aiger [m', i', l', o', a'] inputs' latches' outputs' gates' symbols' comment') = -- system copy to get labels from
    let systemInputLabels = setOfInputLabels symbols' -- "[\"test_\",\"lala_\"]"
        numCopies = (i + o) `div` i' -- number of quantifiers (e.g. 4)
        strategyLabels = map (\(label, num) -> label ++ (show num)) $ zip (cycle systemInputLabels) $ sort $ concat $ replicate i' [0..(numCopies - 1)] -- "[\"test_0\",\"lala_0\",\"test_1\",\"lala_1\",\"test_2\",\"lala_2\",\"test_3\",\"lala_3\"]"
        (strategyInputLabels, strategyOutputLabels) = splitAt i strategyLabels -- split for universal and existential quantifiers / inputs and outputs
        iSymbols    = map (\(_, n, label)       -> ('i', n, label))          $ zip3 inputs  [0..] strategyInputLabels
        lSymbols    = map (\((_,_,_), n)        -> ('l', n, "}l" ++ show n)) $ zip  latches [0..]
        oSymbols    = map (\(_, n, label)       -> ('o', n, label))          $ zip3 outputs [0..] strategyOutputLabels
        symbols_    = sortBy compare $ iSymbols ++ lSymbols ++ oSymbols 
        -- simply ignore user given symbol table, only use self-created one, can only be called, when we use the positional approach. Then the user labels are not needed. For the naming approach, the labels are important and we require the user to give a complete symbol table anyhow. So in that case, there is no need to fill the labels. 
    in Aiger [m,i,l,o,a] inputs latches outputs gates symbols_ comment

compose :: (Aiger, Map.Map String Literal) -> Aiger -> Bool -> (String -> String) -> (Aiger, Map.Map String Literal) 
compose ((Aiger miloa  inputs  latches  outputs  gates  symbols  comment ), emptyStringToLiteralMap)
        (Aiger miloa' inputs' latches' outputs' gates' symbols' comment')
        controllable
        labelModifier =
    let offset         = 2 * head miloa -- offset for second AIG
        contr_symbols' = if controllable then map prependControllable symbols' else symbols'
        plus' x y = case y of 
            0 -> 0  -- reserved symbol for false
            1 -> 1  -- reserved symbol for true
            y -> x + y
    in ((Aiger (map (uncurry (+)) $ zip miloa miloa')
          (inputs  ++ map (plus' offset) inputs')
          (latches ++ map (mapTriple (plus' offset)) latches')
          (outputs ++ map (plus' offset) outputs')
          (gates   ++ map (mapTriple (plus' offset)) gates')
          (debug_trace ("Symbols: " ++ show symbols) $
              symbols ++ map (shiftSymbol miloa labelModifier) contr_symbols')
              -- important, the "other" miloa is needed
          ""), emptyStringToLiteralMap)        -- this deletes any comment, do not change the map

--apply self-composition but according to the given strategy (e.g. no inputs for existentially quantified system copies)
strategy_compose :: (Aiger, (Map.Map String Literal)) -> Aiger -> Int -> Bool -> (String -> String) -> (Aiger, (Map.Map String Literal))
strategy_compose -- (Aiger [m,  i,  l,  o,  a  ] inputs   latches   outputs   gates   symbols   comment  ) -- strategy Aiger (only for reference) -- Not necessary!
                 ((Aiger [m', i', l', o', a' ] inputs'  latches'  outputs'  gates'  symbols'  comment' ), stringToLiteralMap)  -- accu Aiger, initially strategy Aiger, add system copies to it  AND  mapping from strings to corresponding literals in system
                 (Aiger [m'',i'',l'',o'',a''] inputs'' latches'' outputs'' gates'' symbols'' comment'') -- new system copy to add
                 copyIndex
                 controllable -- flag if we are adding a copy corresponding to an existentially quantified copy
                 labelModifier =     
    let offset   = 2 * (m' - i'')
        plus' x y = case y of 
            0 -> 0 
            1 -> 1 
            y -> x + y 

        negateLiteral lit = if even lit then lit + 1 else lit - 1
        correspondingInput lit = if even lit then lit else negateLiteral lit -- always even (following Aiger Format: "After the header the 'I' inputs are listed, one per line, as unnegated literal, e.g. represented as even positive integers." -- http://fmv.jku.at/aiger/FORMAT)
        inputSymbolIndex lit = case elemIndex (correspondingInput lit) inputs'' of 
                                  Nothing -> -1
                                  Just index -> index
        inputLabel lit = tripleThird $ head $ filter (\(varType, index, label) -> (varType == 'i') && (index == (inputSymbolIndex lit))) symbols''
        inputLabelWithCopyIndex lit = (inputLabel lit) ++ "_" ++ (show copyIndex)
        correspondingInputInStrategy lit = stringToLiteralMap Map.! (inputLabelWithCopyIndex lit)
        lookup lit = if even lit then correspondingInputInStrategy lit else negateLiteral $ correspondingInputInStrategy lit
        isInput lit = (lit > 1 && lit < ( i'' + 1) * 2 ) -- check if current literal is an input literal (or relies on an input literal) in the system copy to be added (every bigger literal is no input literal - by assumption that the input literals are the smallest literals in the system.)
        updateLiteral lit = if isInput lit  then lookup lit else (plus' offset lit) -- update current literal such that it fits to the self-composed combination of the strategy and the system copies

        relevantOutputLables = map (\label_ -> label_ ++ (show copyIndex)) $ setOfInputLabels symbols'' -- strategy output labels recycled as inputs in this copy
        indexesOfOutputsToKeep = sort $ map (\(_, index, _) -> index) $ filter (\(varType, index, label) -> (varType == 'o') && (notElem label relevantOutputLables)) symbols'
        existentiallyFilterOutputList outputLiteralList = 
          let filterOutputList :: [(Int, Int)] -> [Int]
              filterOutputList ((lit, index):outLitsWithIndex) = 
                if index `elem` indexesOfOutputsToKeep 
                  then lit:(filterOutputList outLitsWithIndex) 
                  else filterOutputList outLitsWithIndex
              filterOutputList []          = []
          in filterOutputList $ zip outputLiteralList [0..]
        keep_outputs = if controllable then existentiallyFilterOutputList outputs' else outputs'

        -- delete one output label from strategy's symbol table for every input label from new system. shift other list indexes accordingly
        deleteOutputLabel shift ((varType, listIndex, label):symbolList) (varType', listIndex', label') = 
            case varType of 
                'o' -> if label == (label' ++ "_" ++ (show copyIndex)) then deleteOutputLabel 1 symbolList (varType', listIndex', label') else (varType, listIndex - shift, label):(deleteOutputLabel shift symbolList (varType', listIndex', label'))
                _   -> (varType, listIndex, label):(deleteOutputLabel shift symbolList (varType', listIndex', label'))
        deleteOutputLabel _ [] _ = []
        -- delete input labels as these literals are only internally. referencing is still possible via the map. delete strategy output labels, add latch and output labels with appropriate offsets
        existentiallyAdaptSymbolTable accuSymbols (varType, listIndex, userLabel) = 
            case varType of 
                'i' -> deleteOutputLabel 0 accuSymbols (varType, listIndex, userLabel)
                'l' -> accuSymbols ++ [shiftSymbol [m', i', l', o' - i'', a'] labelModifier (varType, listIndex, userLabel)]
                'o' -> accuSymbols ++ [shiftSymbol [m', i', l', o' - i'', a'] labelModifier (varType, listIndex, userLabel)] -- need "- i''" to account for deleted outputs when calculating new listIndex

        -- adapt labels of inputs in strategy, keep latch and output labels but with adapted listIndex. append copyIndex to label
        universallyAdaptSymbolTable accuSymbols (varType, listIndex, userLabel) = -- shiftSymbol miloa labelModifier
            case varType of 
                'i' -> accuSymbols 
                _   -> accuSymbols ++ [shiftSymbol [m', i', l', o', a'] labelModifier (varType, listIndex, userLabel)] -- append copyIndex to user string, adapt list index

        insertIntoMap map (varType, listIndex, label) =
            let literal = case varType of 
                            'i' -> map Map.! (labelModifier label) 
                            'l' -> if isInput (tripleFirst $ latches'' !! listIndex) then updateLiteral (tripleFirst $ latches'' !! listIndex) else plus' offset $ tripleFirst $ latches'' !! listIndex
                            'o' -> if isInput (outputs'' !! listIndex) then updateLiteral (outputs'' !! listIndex) else plus' offset $ outputs'' !! listIndex 
            in Map.insert (labelModifier label) literal map

        o_ = if controllable then ((o' - i'') + o'') else (o' + o'')

        miloa_   = [m' + (m'' - i''), i', l' + l'', o_, a' + a''] 
        inputs_  = inputs'
        latches_ = latches' ++ map (mapTriple updateLiteral) latches''
        outputs_ = keep_outputs ++ map updateLiteral outputs''
        gates_   = gates' ++ map (mapTriple updateLiteral) gates''
        symbols_ = if controllable then foldl existentiallyAdaptSymbolTable symbols' symbols'' else foldl universallyAdaptSymbolTable symbols' symbols''
        comment_ = ""
        composedAiger = Aiger miloa_ inputs_ latches_ outputs_ gates_ symbols_ comment_
        stringToLiteralMap' = foldl insertIntoMap stringToLiteralMap symbols''
 
    in (composedAiger, stringToLiteralMap')

existential_strategy_compose :: (Aiger, (Map.Map String Literal)) -> Aiger -> Int -> Bool -> (String -> String) -> (Aiger, (Map.Map String Literal))
existential_strategy_compose ((Aiger [m', i', l', o', a' ] inputs'  latches'  outputs'  gates'  symbols'  comment'), stringToLiteralMap) -- accu (initially strategy) and string mapping to literals
                              (Aiger [m'',i'',l'',o'',a''] inputs'' latches'' outputs'' gates'' symbols'' comment'') -- new system copy to add
                              copyIndex
                              universal -- flag if we are adding a universally quantified copy
                              labelModifier =
                              
    let offset = if universal then 2 * m' else 2 * (m' - i'') -- inputs of existentially quantified system copies are replaced by the outputs of the strategy, inputs of the universally quantified system copies are kept
        plus' x y = case y of 
            0 -> 0 
            1 -> 1 
            y -> x + y 

        negateLiteral lit = if even lit then lit + 1 else lit - 1
        correspondingInput lit = if even lit then lit else negateLiteral lit -- always even (following Aiger Format: "After the header the 'I' inputs are listed, one per line, as unnegated literal, e.g. represented as even positive integers." -- http://fmv.jku.at/aiger/FORMAT)
        inputSymbolIndex lit = case elemIndex (correspondingInput lit) inputs'' of 
                                  Nothing -> -1
                                  Just index -> index
        inputLabel lit = tripleThird $ head $ filter (\(varType, index, label) -> (varType == 'i') && (index == (inputSymbolIndex lit))) symbols'' -- find the label for a given input literal
        inputLabelWithCopyIndex lit = (inputLabel lit) ++ "_" ++ (show copyIndex)
        correspondingOutputInStrategy lit = stringToLiteralMap Map.! (inputLabelWithCopyIndex lit) -- lookup strategy output literal to use for this input literal
        lookup lit = if even lit then correspondingOutputInStrategy lit else negateLiteral $ correspondingOutputInStrategy lit
        isInput lit = (lit > 1 && lit < ( i'' + 1) * 2 ) -- check if current literal is an input literal (or relies on an input literal) in the system copy to be added (every bigger literal is no input literal - by assumption that the input literals are the smallest literals in the system.)
        updateLiteral lit = if isInput lit 
                              then if universal 
                                      then (plus' offset lit)
                                      else lookup lit 
                              else (plus' offset lit) -- update current literal such that it fits to the self-composed combination of the strategy and the system copies

        relevantOutputLables = map (\label_ -> label_ ++ (show copyIndex)) $ setOfInputLabels symbols'' -- strategy output labels recycled as inputs in this copy
        indexesOfOutputsToKeep = sort $ map (\(_, index, _) -> index) $ filter (\(varType, index, label) -> (varType == 'o') && (notElem label relevantOutputLables)) symbols'
        existentiallyFilterOutputList outputLiteralList = 
          let filterOutputList :: [(Int, Int)] -> [Int]
              filterOutputList ((lit, index):outLitsWithIndex) = 
                if index `elem` indexesOfOutputsToKeep 
                  then lit:(filterOutputList outLitsWithIndex) 
                  else filterOutputList outLitsWithIndex
              filterOutputList []          = []
          in filterOutputList $ zip outputLiteralList [0..]
        keep_outputs = if universal then outputs' else existentiallyFilterOutputList outputs'

        -- delete one output label from strategy's symbol table for every input label from new system. shift other list indexes accordingly
        deleteOutputLabel shift ((varType, listIndex, label):symbolList) (varType', listIndex', label') = 
            case varType of 
                'o' -> if label == (label' ++ "_" ++ (show copyIndex)) then deleteOutputLabel 1 symbolList (varType', listIndex', label') else (varType, listIndex - shift, label):(deleteOutputLabel shift symbolList (varType', listIndex', label'))
                _   -> (varType, listIndex, label):(deleteOutputLabel shift symbolList (varType', listIndex', label'))
        deleteOutputLabel _ [] _ = []
        -- delete input labels as these literals are only internally. referencing is still possible via the map. delete strategy output labels, add latch and output labels with appropriate offsets
        existentiallyAdaptSymbolTable accuSymbols (varType, listIndex, userLabel) = 
            case varType of 
                'i' -> deleteOutputLabel 0 accuSymbols (varType, listIndex, userLabel)
                _   -> accuSymbols ++ [shiftSymbol [m', i', l', o' - i'', a'] labelModifier (varType, listIndex, userLabel)] -- need "- i''" to account for deleted outputs when calculating new listIndex

        -- adapt labels of inputs in strategy, keep latch and output labels but with adapted listIndex. append copyIndex to label
        universallyAdaptSymbolTable accuSymbols (varType, listIndex, userLabel) = -- shiftSymbol miloa labelModifier
            accuSymbols ++ [shiftSymbol [m', i', l', o', a'] labelModifier (varType, listIndex, userLabel)] -- append copyIndex to user string, adapt list index

        insertIntoMap map (varType, listIndex, label) =
            let literal = case varType of 
                            'i' -> if universal then updateLiteral (inputs'' !! listIndex) else map Map.! (labelModifier label) 
                            'l' -> if isInput (tripleFirst $ latches'' !! listIndex) then updateLiteral (tripleFirst $ latches'' !! listIndex) else plus' offset $ tripleFirst $ latches'' !! listIndex
                            'o' -> if isInput (outputs'' !! listIndex) then updateLiteral (outputs'' !! listIndex) else plus' offset $ outputs'' !! listIndex 
            in Map.insert (labelModifier label) literal map

        m_ = if universal then m' + m'' else m' + (m'' - i'')
        i_ = if universal then i' + i'' else i'
        o_ = if universal then (o' + o'') else ((o' - i'') + o'')

        miloa_   = [m_, i_, l' + l'', o_, a' + a''] 
        inputs_  = if universal then inputs' ++ (map updateLiteral inputs'') else inputs' -- keep the inputs of the universal system copies, but add offset, in the existential case there should be no inputs
        latches_ = latches' ++ map (mapTriple updateLiteral) latches''
        outputs_ = keep_outputs ++ map updateLiteral outputs''
        gates_   = gates' ++ map (mapTriple updateLiteral) gates''
        symbols_ = if universal then foldl universallyAdaptSymbolTable symbols' symbols'' else foldl existentiallyAdaptSymbolTable symbols' symbols''
        comment_ = ""
        composedAiger = Aiger miloa_ inputs_ latches_ outputs_ gates_ symbols_ comment_
        stringToLiteralMap' = foldl insertIntoMap stringToLiteralMap symbols''
 
    in (composedAiger, stringToLiteralMap')

mapTuple f (a,b) = (f a, f b)

mapTriple f (a,b,c) = (f a, f b, f c)

prependControllable :: RawSymbol -> RawSymbol
prependControllable (a,b,c) = if a=='i' then (a,b,"controllable_"++c) else (a,b,c)

stripControllable :: RawSymbol -> RawSymbol
stripControllable (a,b,c) = if a=='i' && startswith "controllable_" c then (a,b,drop 13 c) else (a,b,c)

stringStripControllable :: String -> String
stringStripControllable s = if startswith "controllable_" s then drop 13 s else s

shiftSymbol :: [Int] -> (String -> String) -> RawSymbol -> RawSymbol
shiftSymbol miloa labelModifier (varType,num,label) =
    let offset = case varType of
                    'i' -> miloa !! 1
                    'l' -> miloa !! 2
                    'o' -> miloa !! 3 in
    (varType, num+offset, labelModifier label)

-----------------------------------------------------------------------

--stripControllableLabel :: RawSymbol -> RawSymbol
--stripControllableLabel (a,b,c) =
--    if noOfAlternations>0 && startswith "controllable_" c
 --   then (a,b,drop 13 c)
 --   else (a,b,c)

refinedSymbolList :: Aiger -> [(String,Literal)]
refinedSymbolList aig@(Aiger miloa i l o a s c) = 
    case s of 
          ('i',n,label):ss -> (stringStripControllable label, i !! n) : refinedSymbolList (Aiger miloa i l o a ss c)
          ('l',n,label):ss -> (label, tripleFirst (l !! n)) : refinedSymbolList (Aiger miloa i l o a ss c)
          ('o',n,label):ss -> (label, o !! n) : refinedSymbolList (Aiger miloa i l o a ss c)
          []           -> []
          _            -> error "unexpected symbol type"

tripleFirst (a,_,_) = a

tripleThird (_,_,c) = c

numInputs :: Aiger -> Int
numInputs  (Aiger miloa _ _ _ _ _ _) = miloa !! 1
numLatches :: Aiger -> Int
numLatches (Aiger miloa _ _ _ _ _ _) = miloa !! 2
numOutputs :: Aiger -> Int
numOutputs (Aiger miloa _ _ _ _ _ _) = miloa !! 3

