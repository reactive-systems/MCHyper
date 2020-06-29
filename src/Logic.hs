{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Reactive Systems Group
-- License     :  ISC
--
-- Maintainer  : Norine Coenen
-- Stability   : Development
-- Portability : 
--
-- |
--
-----------------------------------------------------------------------------

module Logic (
    Formula, negNF, formula2Aiger, PCircuit, sf
) where

import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.Hashable
import Data.String.Utils

import Aiger 
import Debug.Trace
debug_trace = trace
computeAndPrint s x = debug_trace (s ++ show x) x 

data Quantifier = Universal | Existential

data Formula = 
      Const Bool
    | AP String Int -- atomic proposition using de Bruin indices
    | Neg Formula
    | X Formula  -- Next
    | Y Formula  -- Yesterday, past dual of next
    | And Formula Formula
    | Or Formula Formula
    | Until Formula Formula
    | Release Formula Formula
    | Since Formula Formula
    | Trigger Formula Formula -- dual of since, past dual of release
    | Forall Formula
    | Exists Formula
    -- Derived operators:
    | G Formula  -- Globally
    | F Formula  -- Finally
    | WUntil Formula Formula -- Weak Until
    | Eq Formula Formula
    | Neq Formula Formula
    | Implies Formula Formula
    | O Formula  -- O, past dual of F
    | H Formula  -- Hitherto, past dual of G
    | SameInputsExcept Int Int [String]
    | EqualOn Int Int [String]
    | SameState Int Int
    deriving (Show, Eq, Read, Ord)
    

sf :: Formula -> [Formula] -- immediate subformulas
sf (Const _)         = []
sf (AP _ _)          = []
sf (Neg phi)         = [phi]
sf (And phi psi)     = [phi,psi]
sf (Or phi psi)      = [phi,psi]
sf (X phi)           = [phi]
sf (Y phi)           = [phi]
sf (Until phi psi)   = [phi,psi]
sf (Release phi psi) = [phi,psi]
sf (Since phi psi)   = [phi,psi]
sf (Trigger phi psi) = [phi,psi]
sf (Forall phi)      = [phi]
sf (Exists phi)      = [phi]
sf  x                = error ("Unexpected operator in sf: " ++ show x)

-- Converts to negation normal form
negNF :: Formula -> Formula
negNF (Neg (Const b))         = Const (not b)
negNF (Neg (AP a i))          = Neg (AP a i)
negNF (Neg (Neg phi))         = negNF phi
negNF (Neg (X phi))           = X $ negNF (Neg phi)
negNF (Neg (Y phi))           = Y (negNF (Neg phi)) 
negNF (Neg (And phi psi))     = Or (negNF (Neg phi)) (negNF (Neg psi))
negNF (Neg (Or phi psi))      = And (negNF (Neg phi)) (negNF (Neg psi))
negNF (Neg (Until phi psi))   = Release (negNF (Neg phi)) (negNF (Neg psi))
negNF (Neg (Release phi psi)) = Until (negNF (Neg phi)) (negNF (Neg psi))
negNF (Neg (Since phi psi))   = Trigger (negNF (Neg phi)) (negNF (Neg psi))
negNF (Neg (Trigger phi psi)) = Since (negNF (Neg phi)) (negNF (Neg psi))
negNF (Neg (Forall phi))      = Exists (negNF $ Neg phi)
negNF (Neg (Exists phi))      = Forall (negNF $ Neg phi)
negNF (Const b)           = Const b
negNF (AP a i)            = AP a i
negNF (X phi)             = X (negNF phi)
negNF (Y phi)             = Y (negNF phi)
negNF (And phi psi)       = And (negNF phi) (negNF psi)
negNF (Or phi psi)        = Or (negNF phi) (negNF psi)
negNF (Until phi psi)     = Until (negNF phi) (negNF psi)
negNF (Release phi psi)   = Release (negNF phi) (negNF psi)
negNF (Since phi psi)     = Since (negNF phi) (negNF psi)
negNF (Trigger phi psi)   = Trigger (negNF phi) (negNF psi)
negNF (Exists phi)        = Exists (negNF phi)
negNF (Forall phi)        = Forall (negNF phi) -- negates the inside
negNF x  = error ("Unexpected operator in negNF: " ++ show x)

-- Expand Derived Operators
edo :: Aiger -> Formula -> Formula 
edo aig  (Const b)           = Const b
edo aig (Neg phi)           = Neg (edo aig phi)
edo aig (AP a i)            = AP a i
edo aig (X phi)             = X (edo aig phi)
edo aig (Y phi)             = Y (edo aig phi)
edo aig (And phi psi)       = And (edo aig phi) (edo aig psi)
edo aig (Or phi psi)        = Or (edo aig phi) (edo aig psi)
edo aig (Until phi psi)     = Until (edo aig phi) (edo aig psi)
edo aig (Release phi psi)   = Release (edo aig phi) (edo aig psi)
edo aig (Since phi psi)     = Since (edo aig phi) (edo aig psi)
edo aig (Trigger phi psi)   = Trigger (edo aig phi) (edo aig psi)
edo aig (Exists phi)        = Exists (edo aig phi)
edo aig (Forall phi)        = Neg $ Exists (edo aig $ Neg phi)
-- Derived operators
edo aig (F phi)             = Until (Const True) (edo aig phi)
edo aig (G phi)             = Neg (edo aig (F (Neg phi)))
edo aig (WUntil phi psi)    = edo aig $ Release psi (Or phi psi)
edo aig (Eq phi psi)        = Or (And phi' psi') (And (Neg phi') (Neg psi'))
    where phi' = edo aig phi
          psi' = edo aig psi
edo aig (Neq phi psi)       = Neg (edo aig (Eq phi psi))
edo aig (Implies phi psi)   = Or (Neg phi') psi'
    where phi' = edo aig phi
          psi' = edo aig psi
edo aig (O phi)             = Since (Const True) (edo aig phi)
edo aig (H phi)             = Neg (edo aig (O (Neg phi)))
edo aig (SameInputsExcept i1 i2 expt) = 
    edo aig . foldl And (Const True) . map (\(_,_,l) -> (Eq (AP l i1) (AP l i2))) . filter (\(t,v,l) -> t=='i' && l `notElem` expt) $ symbols aig 
edo aig (EqualOn i1 i2 equalSyms) = 
        edo aig . foldl And (Const True) . map (\(_,_,l) -> (Eq (AP l i1) (AP l i2))) . filter (\(t,v,l) -> l `elem` equalSyms) $ symbols aig
edo aig (SameState i1 i2) =
    edo aig . foldl And (Const True) . map (\(_,_,l) -> (Eq (AP l i1) (AP l i2))) . filter (\(t,v,l) -> t=='l') $ symbols aig 

isFutureOp :: Formula -> Bool 
isFutureOp (X phi)         = True
isFutureOp (Until phi psi)    = True
isFutureOp (Release phi psi)  = True
isFutureOp _                  = False

isPastOp :: Formula -> Bool 
isPastOp (Y phi)     = True
isPastOp (Since phi psi)    = True
isPastOp (Trigger phi psi)  = True
isPastOp _                  = False

pastFormulas :: Formula -> [Formula]
pastFormulas phi = (if isPastOp phi then [phi] else []) ++ concatMap pastFormulas (sf phi)

futureFormulas :: Formula -> [Formula] 
futureFormulas phi = (if isFutureOp phi then [phi] else []) ++ concatMap futureFormulas (sf phi)

rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' Set.empty where
  rmdups' _ [] = []
  rmdups' a (b : c) = if Set.member b a
    then rmdups' a c
    else b : rmdups' (Set.insert b a) c
    
futureObligations :: Formula -> [Formula]
futureObligations phi = 
    let stripNext (X phi) = phi
        stripNext phi     = phi
    in rmdups $ map stripNext $ futureFormulas phi
    

temporalFormulas :: Formula -> [Formula] 
temporalFormulas phi = (if isPastOp phi || isFutureOp phi then [phi] else []) ++ concatMap temporalFormulas (sf phi)

stripQuantifiers :: Formula -> Formula
stripQuantifiers (Forall phi) = stripQuantifiers phi
stripQuantifiers (Exists phi) = stripQuantifiers phi
stripQuantifiers phi          = phi

lengthOfquantifierPrefix (Forall phi) = 1 + lengthOfquantifierPrefix phi
lengthOfquantifierPrefix (Exists phi) = 1 + lengthOfquantifierPrefix phi
lengthOfquantifierPrefix (Neg phi)    = lengthOfquantifierPrefix phi 
lengthOfquantifierPrefix phi          = 0

alternationDepth (Exists phi') = 
    let qc = alternationDepthHelp phi' Existential [1] 
    in (length qc -1, reverse qc)
alternationDepth (Forall phi') = 
    let qc = alternationDepthHelp phi' Universal [1]
    in (length qc -1, reverse qc)
    
alternationDepthHelp (Exists phi') Existential (x:quantCounts) = 
    alternationDepthHelp phi' Existential ((x+1):quantCounts)
alternationDepthHelp (Forall phi') Existential (x:quantCounts) = 
    alternationDepthHelp phi' Universal (1:x:quantCounts)
alternationDepthHelp (Exists phi') Universal   (x:quantCounts) = 
    alternationDepthHelp phi' Existential (1:x:quantCounts)
alternationDepthHelp (Forall phi') Universal   (x:quantCounts) = 
    alternationDepthHelp phi' Universal ((x+1):quantCounts)
alternationDepthHelp _ _ quantCounts = quantCounts

{-
0.)  Check whether the formula is in the fragment. 
1.)  Pull quantifiers (omitted for now). 
2.)  Get prime formulas.
3.)  Introduce additional input variables (one for each prime formula).
4.)  Create map that indicates which latches and variables correspond to which formulas.
5.)  Unroll each primeFormula to a FCircuit.
6.)  Make the formula automata input deterministic.
7.)  Create one copy of the AIG for each quantifier and create a map indicating the name offsets.
8.)  Compile a list of AIGs from the FCircuit.
-}

formula2Aiger :: Aiger -> Formula -> Maybe Aiger -> Aiger 
formula2Aiger aiger phi maybeStrategy = 
    let quantCount = computeAndPrint "QuantCount: " $ lengthOfquantifierPrefix phi
        aiger'@(Aiger miloa' i' l' o' a' s' c') = fillLabels aiger 
        phiNF =  negNF (edo aiger' phi)
        (noOfAlternations, quantCounts) = computeAndPrint "Alternations: " $ alternationDepth phiNF
        (phi',(universals,existentials)) = computeAndPrint "Processed formula: " $ -- transform to normal form
            case (quantCount, noOfAlternations, phiNF) of
                (0, _, _)         -> (negNF . Neg $ phiNF, (0,1)) -- interpret as LTL formula
                (_, 0, Forall _)  -> (negNF . Neg . stripQuantifiers $ phiNF, (0,quantCount))
                (_, 0, Exists _)  -> (stripQuantifiers phiNF, (0,quantCount))
                (_, 1, Forall _)  -> -- (negNF . Neg . stripQuantifiers $ phiNF, (quantCounts!!0, quantCounts!!1)) 
                    -- (The inner player will try to avoid the "winningOutput", which is set to 1 when the formula is fulfilled. )
                    -- require that a strategy was given, then provide the required normal form
                    case maybeStrategy of 
                        Nothing -> error "Please provide a strategy as input. "
                        Just _ -> (negNF . Neg . stripQuantifiers $ phiNF, (quantCounts!!0, quantCounts!!1))
                (_, 1, Exists _)  -> -- (stripQuantifiers phiNF, (quantCounts!!0,quantCounts!!1)) 
                    case maybeStrategy of
                        Nothing -> error "Please provide the existentially quantified path as strategy input. "
                        Just _ -> (negNF . Neg . stripQuantifiers $ phiNF, (quantCounts!!1, quantCounts!!0))
                (_, n, _)         -> 
                    if n>1
                        then error "Only one quantifier alternation is supported at the moment."
                        else error "Formula must start with a quantifier or must have none (i.e. be an LTL formula)."
        
        phi'' = debug_trace ("Phi': " ++ show phi') $ phi'
        
        selfComposeSystem = foldl (\((aig1,emptyMap),n) -> \aig2 -> 
                                (compose 
                                    (aig1,emptyMap) 
                                    aig2 
                                    (noOfAlternations>0 && n>=quantCounts!!0) 
                                    (indexSymbol n),n+1))
                           ((emptyAiger, Map.empty),0)
                           (replicate quantCount aiger')

        selfComposeSystemWithStrategy strategyAiger = foldl (\((accuAig, accuMap),n) -> \systemAig ->
                                                        (strategy_compose 
                                                            (accuAig, accuMap) 
                                                            systemAig 
                                                            n 
                                                            (noOfAlternations>0 && n>=quantCounts!!0) 
                                                            (indexSymbol n), n+1))
                                                        ((strategyAiger, Map.fromList (refinedSymbolList strategyAiger)), 0) 
                                                        (replicate quantCount aiger')

        existentiallySelfComposeSystemWithStrategy strategyAiger = foldl (\((accuAig, accuMap), n) -> \systemAig ->
                                                                        (existential_strategy_compose
                                                                            (accuAig, accuMap)
                                                                            systemAig
                                                                            n
                                                                            (noOfAlternations>0 && n>=quantCounts!!0)
                                                                            (indexSymbol n), n+1))
                                                                    ((strategyAiger, Map.fromList (refinedSymbolList strategyAiger)), 0)
                                                                    (replicate quantCount aiger')

        ((pre_aigerK,resultMap),_) = 
            case (quantCount, noOfAlternations, phiNF, maybeStrategy) of
                (n, 1, Forall _, Just strategyAiger@(Aiger miloa i l o a s c)) -> if n == 0 
                                                                                    then selfComposeSystem 
                                                                                    else if (null s)
                                                                                        then selfComposeSystemWithStrategy $ fillStrategyLabels strategyAiger aiger' -- use positional approach by giving labels such that the naming approach and the positional approach coincide
                                                                                        else if (((length s) >= ((miloa !! 1) + (miloa !! 3))) -- strategy has enough symbol table entries (may still include some latch symbol table entires, but at least has a chance)
                                                                                                    && ((miloa !! 1) >= ((quantCounts!!0) * (miloa' !! 1))) -- number of strategy inputs is at least the same as the number of system inputs times the number of universal quantifiers, it is possibly greater to account for prophecy variables
                                                                                                    && ((miloa !! 3) == ((quantCounts!!1) * (miloa' !! 1)))) -- number of strategy outputs is equal to the number of system inputs times the number of existential quantifiers
                                                                                            then selfComposeSystemWithStrategy strategyAiger -- use pure naming approach, assumes full symbol table in strategy at least for inputs and outputs matching the input labels from the system. 
                                                                                            else error "Error: Strategy structure does not fit to the system (number of strategy inputs or outputs seems to be wrong or strategy symbol table exists but is not complete)"
                (n, 1, Exists _, Just strategyAiger@(Aiger miloa i l o a s c)) -> if n == 0
                                                                                    then selfComposeSystem
                                                                                    else if (null s)
                                                                                        then existentiallySelfComposeSystemWithStrategy $ fillStrategyLabels strategyAiger aiger'
                                                                                        else if (((length s) >= ((miloa !! 1) + (miloa !! 3))) -- require complete symbol table for the strategy inputs and strategy outputs; is not ensured here but this is just a sanity check
                                                                                                    && ((miloa !! 1) >= 0) -- strategy here only defines the existentially quantified path and thus does not need to have any inputs, but allows for the inputs of prophecy variables 
                                                                                                    && ((miloa !! 3) == ((quantCounts!!0) * (miloa' !! 1)))) -- number of strategy outputs is equal to the number of system inputs times the number of existential quantifiers
                                                                                            then existentiallySelfComposeSystemWithStrategy strategyAiger
                                                                                            else error "Error: Given path does not fit to the system and strategy to be checked. "
                _ -> selfComposeSystem

        aigerK = computeAndPrint "Self-Composed system: " $ pre_aigerK

        Aiger header i l o a s c = aigerK
        
        atomicMap = computeAndPrint ("symbolTable: " ++ (show s) ++ " and \nAtomicMap: ") $ if Map.null resultMap then Map.fromList (refinedSymbolList aigerK) else resultMap 
        
        futures   = futureObligations phi''
        pasts     = pastFormulas phi''
        
        pastNum   = length pasts
        futNum    = length futures
        tempNum   = pastNum + futNum 
        
        -- reserve literals for past and future formula latches ... 
        maxLit0        = 2* (head header + 1) -- next free literal
        pastLatchLits  = zip pasts   [maxLit0,maxLit0+2..]  :: [(Formula,Literal)]
        maxLit1        = maxLit0 + 2*pastNum
        futLatchLits   = zip futures [maxLit1,maxLit1+2..]  :: [(Formula,Literal)]
        maxLit2        = maxLit1 + 2*futNum
        pastMap        = computeAndPrint "pastMap: " $ Map.fromList (pastLatchLits ++ futLatchLits)
        
        -- reserve literals for future formula inputs and past result variables (we do not need inputs for past formulas, but we may need to refer to their X value)
        futureInputs   = zip futures [maxLit2,maxLit2+2..]  :: [(Formula,Literal)]
        maxLit3        = maxLit2+2*futNum
        pastResultVars = zip pasts [maxLit3,maxLit3+2..]  :: [(Formula,Literal)]
        maxLit4        = maxLit3+2*pastNum
        futMap         = computeAndPrint "futMap: " $ Map.fromList (futureInputs ++ pastResultVars)

        circCompile :: Int -> Int -> PCircuit -> (Int,[Gate])
        circCompile outLit firstFreeLit circ = compile atomicMap pastMap futMap outLit [] firstFreeLit circ

        -- introduce a latch indicating it is the first step
        initLatch@(initLatchLit,initLatchLitIn,0) = (maxLit4, 1, 0) -- assigns true to init latch, s.t. it is only false in the first step
        
        initialStepCorrectLit = maxLit4 + 2
        ic = computeAndPrint "Init Circ: " $ initCirc futMap pastMap phi''
        (maxLit5,initGates) = circCompile initialStepCorrectLit (initialStepCorrectLit+2) ic
        -- compile past formulas
        --pastCircs = map unroll pasts
        --(maxLit6, pastGates) =  -- this also prepends the reserved "result variable" for each past formula
        --    foldl 
        --        (\(max,gs) -> \(circ,resultVar) ->
        --            let (max',gs') = 
        --                    debug_trace ("Compiling past circ: " ++ show circ) $
        --                    circCompile (max,gs) circ 
        --            in (max', (resultVar,fstt $ head gs',fstt $ head gs'):gs')) 
        --        (maxLit5, [])
        --        (zip pastCircs (map snd pastResultVars)) :: (Literal,[Gate])
        --    where
        --        fstt (x,_,_) = x
        --pastLatches = zip (map snd pastLatchLits) (map snd pastResultVars)  :: [(Literal,Literal)]
        maxLit6 = maxLit5
        
        -- compile future formulas
        -- for each future circuit, this is how to violate it:
        futViolations = 
            map 
                (\(form,latch) -> 
                    let pcirc = precompile futMap pastMap (unroll form)
                    in CAnd (CLit latch) (CNeg pcirc))
                futLatchLits
        --futPCircs = map (precompile futMap pastMap) $ map unroll futures
        sinkLatch@(sinkLatchLit,sinkLatchLitIn,0) = computeAndPrint "sinkLatch: " (maxLit6,maxLit6+2,0)
        sinkCirc      = computeAndPrint "sinkCirc: " $ 
            foldl 
                COr 
                (COr 
                    (CLit sinkLatchLit) -- it's a trap!
                    (CAnd (CLit (initLatchLit+1)) (CNeg (CLit initialStepCorrectLit)))
                ) 
                futViolations
        
        -- compile the formula for the sink latch
        (maxLit7,sinkGates) =  circCompile sinkLatchLitIn (sinkLatchLitIn+2) sinkCirc
        
        
        -- Safety conditions: all obligations are satisfied and we are not in the sink
        safetyLit = maxLit7
        futureLatches = zip3 (map snd futLatchLits) (map snd futureInputs) (repeat 0)  :: [(Literal,Literal,Literal)]
        openObligations = map (CLit . snd) futureInputs 
            -- here we select the X value of all obligations to output the winning flag early on
        winningCirc = computeAndPrint "winningCirc: " $
            CNeg $ foldl COr (COr (CLit sinkLatchLitIn) (CLit (initLatchLit+1))) openObligations
        (maxLit8, winningGates) = circCompile safetyLit safetyLit winningCirc
        
        -- Liveness: we arrive at a previously visited state and have satisfied all obligations that were open in the loop entry.
        -- introduce an additional input which indicates that the current state should be remembered.
        stateCopyInput = maxLit8
        stateCopyLatch@(stateCopyLatchLit,stateCopyLatchLitIn,0) = (maxLit8+2,maxLit8+4,0) 
        stateCopyLatchCirc = COr (CLit stateCopyInput) (CLit stateCopyLatchLit)
        (maxLit9, stateCopyLatchGates) = 
            circCompile stateCopyLatchLitIn stateCopyLatchLitIn stateCopyLatchCirc
        
        -- copy the state when the stateCopyInput is set, but stateCopyLatchLit is not
        stateLatches = l ++ initLatch : futureLatches
        (maxLit10,stateLatchCopies) =
            foldr
                (\(latch,latchIn,latchInitVal) -> \(mL,lCs) -> 
                    (mL+4,
                        (mL,
                         mL+2,
                         case latchInitVal of 
                             0 -> 0 
                             1 -> 1 
                             _ -> mL)
                        :lCs))
                (maxLit9,[])
                stateLatches
        stateCopySignal = computeAndPrint "stateCopySignal: " maxLit10 -- indicates when to copy
        stateCopySignalCirc = CAnd (CLit stateCopyLatchLitIn) (CNeg (CLit stateCopyLatchLit))
        (maxLit11,stateCopySignalGates) = 
            circCompile stateCopySignal stateCopySignal stateCopySignalCirc
        
        (maxLit12, copyGates) = -- make sure the copy is correctly updated
            foldl 
                (\(maxLit_in,gates) -> \((stateLatch,_,_),(stateLatchCopy,stateLatchCopyIn,_)) -> 
                    let copyCirc = 
                            COr (CAnd (CLit stateCopySignal) (CLit stateLatch)) 
                                (CAnd (CNeg (CLit stateCopySignal)) (CLit stateLatchCopy)) 
                        (maxLit_out,newGates) = circCompile stateLatchCopyIn maxLit_in copyCirc
                    in (maxLit_out,newGates ++ gates))
                (maxLit11,[])
                $ zip stateLatches stateLatchCopies
        
        -- a finite path represents a lasso, if the last state is equal to the saved state, ... and we are not in the sink state.
        isLassoLit = maxLit12
        isLassoCirc = 
                foldl 
                    (\circ -> \((latchLit,_,_),(latchLitCopy,_,_)) -> 
                        CAnd circ (CNeg (CXOR (CLit latchLit) (CLit latchLitCopy))))
                    (CConst True)
                    $ zip stateLatches stateLatchCopies
        (maxLit13, isLassoGates) = circCompile isLassoLit isLassoLit isLassoCirc
        
        -- find all untils
        isUntil (Until _ _) = True
        isUntil _ = False
        untils = filter isUntil futures
        
        -- introduce an additional latch for every until
        (maxLit14,untilLatches) = 
            foldr (\u -> \(maxLitCur,uLatches) -> (maxLitCur+4,(maxLitCur,maxLitCur+2,0):uLatches))
                (maxLit13,[]) 
                untils
        -- for all untils: if their right subformula is satisfied, 
        untilCircs = 
            map (\(u@(Until _ psi),(l,l_in,_)) -> 
                    let psiCirc = precompile futMap pastMap . unroll $ psi
                    in  COr (CAnd (CLit stateCopySignal) (CLit ((Map.!) pastMap u))) 
                            (CAnd (CNeg (CLit stateCopySignal)) (CAnd (CLit l) (CNeg psiCirc))))
                $ zip untils untilLatches
        (maxLit15,untilGates) = 
            foldr
                (\(uc,(_,l_in,_)) -> \(maxLitCur,gates) -> 
                    let (maxLitNext,newGates) = circCompile l_in maxLitCur uc
                    in  (maxLitNext,newGates ++ gates))
                (maxLit14,[])
                $ zip untilCircs untilLatches
                
        livenessLit  = maxLit15
        livenessCirc = 
            foldr
                (\(l,l_in,0) -> \circ -> CAnd (CNeg (CLit l)) circ)
                (CAnd (CLit stateCopyLatchLit) (CAnd (CNeg (CLit sinkLatchLitIn)) (CLit isLassoLit)))
                untilLatches
        (maxLit16,livenessGates) = 
            circCompile livenessLit livenessLit livenessCirc
        
        maxLitLast   = maxLit16
        
        -- Create new header
        newInputs  = map snd futureInputs ++ [stateCopyInput]
        inputNum   = (header !! 1) + length newInputs
        inputSym   = 
                map (\((phi,_),n) -> ('i',n,"I:" ++ show_friendly phi)) (zip futureInputs [header !! 1..])
                ++ [('i',inputNum-1,"I:remember_state")]
                
        newLatches = [sinkLatch, initLatch, stateCopyLatch]
                     ++ futureLatches
                     ++ stateLatchCopies
                     ++ untilLatches
                     {-++ pastLatches-}
        latchNum   = header !! 2 + length newLatches
        latchSym   =    [('l',(header !! 2) +0, "sink")]
                     ++ [('l',(header !! 2) +1, "init")]
                     ++ [('l',(header !! 2) +2, "entered_lasso")]
                     ++ map (\((phi,_),n) -> ('l',n,"L:" ++ show_friendly phi)) (zip futLatchLits [(header !! 2) +3..])
                     ++ [('l',n,"l" ++ show (x-1) ++"_copy") | (x,n) <- zip [1..length stateLatchCopies] [(header !! 2) +3 + length futLatchLits..]] 
                     ++ [ ('l',n,"L_MH:" ++ show_friendly phi) | (phi,n) <- zip untils [(header !! 2) +3 + length futLatchLits + length stateLatchCopies..]]
        newOutputs = [safetyLit,livenessLit]
        outNum = 2
        filteredSyms = filter (\(t,n,str) -> t /= 'o') s

        newGates   = winningGates 
                     ++ sinkGates 
                     ++ initGates 
                     ++ livenessGates 
                     ++ isLassoGates 
                     ++ copyGates 
                     ++ stateCopySignalGates 
                     ++ stateCopyLatchGates 
                     ++ untilGates
                     {- ++ pastGates -}
        gateNum    = header !! 4 + length newGates
        
        newHeader  = [div maxLitLast 2 + 1, inputNum, latchNum, outNum, gateNum]
        
    in --aigerK
    Aiger newHeader 
        (i ++ newInputs)
        (l ++ newLatches)
        newOutputs {- ++ o -}
        (a ++ newGates)
        (filteredSyms ++ inputSym ++ latchSym)
        c
        
        
show_friendly phi = 
        let orig = show phi 
            escaped  = filter (/='\"') . filter (/=' ') $ orig
            filtered = replace "(AP" "(" $  replace "R(False)" "G" $ replace "U(True)" "F" $ replace "Release" "R" $ replace "Until" "U" $ replace "(Const" "(" escaped
            cutoff = 57
            cut = if length filtered >30 then take cutoff filtered ++ "..." ++ show (mod (hash $ drop cutoff filtered) 1000) else filtered
        in cut
--show_friendly (Until (Const True) y) = "F " ++ show  (mod (hash (show y)) 10000)
--show_friendly (Until x y) = "U "  ++ show  (mod (hash (show x ++ show y)) 10000)
--show_friendly (Release (Const False) y) = "G " ++ show  (mod (hash (show y)) 10000)
--show_friendly (Release x y) = "R " ++ show  (mod (hash (show x ++ show y)) 10000)
--show_friendly (X x) = "X " ++ show  (mod (hash (show x)) 10000)
--show_friendly phi = (take 6 $ filter (/='\"') $ show phi) ++ "..."

data PCircuit = -- Pre-Circuit
      CConst Bool
    | CNeg PCircuit
    | CAnd PCircuit PCircuit
    | COr  PCircuit PCircuit
    | CXOR PCircuit PCircuit
    | CAtomic String Int -- An atomic proposition that must hold now, additional integer indicates the path it refers to
    | CLit  Literal
    deriving (Show,Eq)
    
precompile :: 
    (Map.Map Formula Variable) -> 
    (Map.Map Formula Variable) -> 
    Formula -> PCircuit
precompile futs pasts (Const b)         = CConst b
precompile futs pasts (AP l p)          = CAtomic l p
precompile futs pasts (Neg phi)         = CNeg $ precompile futs pasts phi
precompile futs pasts (And phi psi)     = CAnd (precompile futs pasts phi) (precompile futs pasts psi)
precompile futs pasts (Or phi psi)      = COr (precompile futs pasts phi) (precompile futs pasts psi)
precompile futs pasts (X phi)           = debug_trace ("Looking up fut: " ++ show (phi)) $ 
                                            CLit ((Map.!) futs (phi))
--precompile futs pasts (Until phi psi)   = CLit ((Map.!) futs  (Until   phi psi))
--precompile futs pasts (Release phi psi) = CLit ((Map.!) futs  (Release phi psi))
precompile futs pasts (Y phi)           = debug_trace ("Looking up past: " ++ show (phi)) $ 
                                            CLit ((Map.!) pasts (Y phi))
--precompile futs pasts (Since phi psi)   = CLit ((Map.!) pasts (Since   phi psi))
--precompile futs pasts (Trigger phi psi) = CLit ((Map.!) pasts (Trigger phi psi))
precompile futs pasts  x                = error ("Unexpected operator in precompile: " ++ show x)

initCirc :: 
    (Map.Map Formula Variable) -> 
    (Map.Map Formula Variable) -> 
    Formula -> PCircuit
initCirc futs pasts (Const b)         = CConst b
initCirc futs pasts (AP l p)          = CAtomic l p
initCirc futs pasts (Neg phi)         = CNeg $ initCirc futs pasts phi
initCirc futs pasts (And phi psi)     = CAnd (initCirc futs pasts phi) (initCirc futs pasts psi)
initCirc futs pasts (Or phi psi)      = COr (initCirc futs pasts phi) (initCirc futs pasts psi)
initCirc futs pasts (X phi)           = precompile futs pasts $ X phi
initCirc futs pasts (Until phi psi)   = precompile futs pasts $ unroll (Until   phi psi)
initCirc futs pasts (Release phi psi) = precompile futs pasts $ unroll (Release phi psi)
initCirc futs pasts (Y phi)           = precompile futs pasts $ Y phi
initCirc futs pasts (Since phi psi)   = precompile futs pasts $ backroll (Since phi psi)
initCirc futs pasts (Trigger phi psi) = precompile futs pasts $ backroll (Trigger phi psi)
initCirc futs pasts  x                = error ("Unexpected operator in initCirc: " ++ show x)

backroll :: Formula -> Formula
backroll (Y phi)            = Y phi
backroll (Since phi psi)    = backroll . Or psi . And phi $ Y (Since phi psi)
backroll (Trigger phi psi)  = backroll . And psi . Or phi $ Y (Trigger phi psi)
backroll (Const b)          = Const b
backroll (Neg phi)          = Neg (backroll phi)
backroll (AP a i)           = AP a i
backroll (And phi psi)      = And (backroll phi) (backroll psi)
backroll (Or phi psi)       = Or (backroll phi) (backroll psi)
--backroll (X phi)            = X phi -- TODO: correct?
--backroll (Until phi psi)    = Until phi psi -- TODO: correct?
--backroll (Release phi psi)  = Release phi psi -- TODO: correct?
backroll phi                = error $ "Error: unexpected operator: " ++ show phi

-- unroll one temporal step in a quantifier-free formula in NNF.
unroll :: Formula -> Formula
unroll (X phi)            = X phi
unroll (Until phi psi)    = unroll . Or psi . And phi $ X (Until phi psi)
unroll (Release phi psi)  = unroll . And psi . Or phi $ X (Release phi psi)
unroll (Const b)          = Const b
unroll (Neg phi)          = Neg (unroll phi)
unroll (AP a i)           = AP a i
unroll (And phi psi)      = And (unroll phi) (unroll psi)
unroll (Or phi psi)       = Or (unroll phi) (unroll psi)
--unroll (Y phi)            = Y phi -- TODO: correct?
--unroll (Since phi psi)    = Since phi psi -- TODO: correct?
--unroll (Trigger phi psi)  = Trigger phi psi -- TODO: correct?
unroll phi                = error $ "Error: unexpected operator in unroll function: " ++ show phi

nextFreeLatch outl ffl = if outl==ffl then ffl+2 else ffl

-- creates a list of and gates, creates new signals, enumerating the newly introduced variables in preorder (as seen from the formula tree)
compile :: (Map.Map String Variable) 
            -> (Map.Map Formula Variable) 
            -> (Map.Map Formula Variable) 
            -> Int  -- the output literal, the wire this circuit should result in
            -> [Gate]
            -> Int -- the first free lit ... typically the same as the output literal
            -> PCircuit 
            -> (Int, [Gate])
compile aM pM fM outl gates ffl (CConst True)  = 
    (nextFreeLatch outl ffl,(outl,1,1):gates)
compile aM pM fM outl gates ffl (CConst False) = 
    (nextFreeLatch outl ffl,(outl,0,0):gates)
compile aM pM fM outl gates ffl (CNeg circ)    = 
            let nfl = nextFreeLatch outl ffl
                (k',gates') = compile aM pM fM nfl gates nfl circ
            in (k',(outl,nfl+1,nfl+1):gates')
compile aM pM fM outl gates ffl (CAnd c1 c2)   = 
            let nfl = nextFreeLatch outl ffl
                (k' , gates' ) = compile aM pM fM nfl gates nfl c1
                (k'', gates'') = compile aM pM fM k' gates' k' c2
            in (k'',(outl,nfl,k'):gates'')
compile aM pM fM outl gates ffl (COr  c1 c2)   = 
            let  nfl = nextFreeLatch outl ffl
                 (k' , gates' ) = compile aM pM fM (nfl+2) gates (nfl+2) c1
                 (k'', gates'') = compile aM pM fM k' gates' k' c2
            in   (k'',(outl,nfl+1,nfl+1):(nfl,nfl+3,k'+1):gates'')
compile aM pM fM outl gates ffl (CXOR c1 c2)   = 
            let nfl = nextFreeLatch outl ffl -- next free lit ... usually==ffl+2
                sfl = nfl + 2 -- second free lit
                tfl = nfl + 4 -- third free lit
                fourthfl = nfl + 6 
                (k' , gates' ) = compile aM pM fM fourthfl gates fourthfl c1
                (k'', gates'') = compile aM pM fM k' gates' k' c2
            in  (k'',(outl,nfl+1,nfl+1):(nfl,sfl+1,tfl+1):(sfl,fourthfl,k'+1):(tfl,fourthfl+1,k'):gates'')
            -- (k'',(k,k+3,k+3):(k+2,k+5,k+7):(k+4,k+8,k'+1):(k+6,k+9,k'):gates'') 
compile aM pM fM outl gates ffl (CLit lit)  = (nextFreeLatch outl ffl,(outl,lit,lit):gates)
compile aM pM fM outl gates ffl (CAtomic l p)   = (nextFreeLatch outl ffl,(outl,x,x):gates)
                where x = debug_trace ("Looking for atom: " ++ (indexSymbol p l)) $ (Map.!) aM (indexSymbol p l)

indexSymbol n s = s ++ "_" ++ show n



