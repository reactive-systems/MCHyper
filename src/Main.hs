{-# LANGUAGE CPP, TemplateHaskell, FlexibleContexts #-}
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

module Main (
    main
) where

import Logic
import Aiger
import Tests

import Control.Monad (unless)
import Data.Maybe
import Data.List
import Data.Set (Set)
import Data.Array
import Data.Char
import qualified GHC.Int
import qualified Data.Set as Set
import qualified Data.Map as Map 
import System.Exit (exitFailure)
import System.Environment

import Numeric

import System.IO
import System.Process

import Text.ParserCombinators.Parsec(ParseError) 

header = "Usage: main [OPTIONS] FORMULA [FILE_IN | FILE_IN FILE_OUT]  \n\
Input and output files are .aag (ASCII Aiger format).\n\
If no input file is given, contents from stdin are read and outputs written to stdout.\n\n\
Available options are:\n\
  -h --help      print this info\n\
  -v --version   shows the version number\n"


main = do
    args <- getArgs -- file name
    --putStrLn $ show args
    processArgs args
   -- putStrLn "test" 

debug Nothing _ = return ()
debug _       s = putStrLn s

processArgs :: [String] -> IO ()
processArgs args =
    case args of 
        [] -> do
            putStrLn "Err: Need at least one argument."
            putStrLn header
        args | elem "-h" args || elem "--help" args -> do
            putStrLn "User requested help."
            putStrLn header
        args | elem "-v" args || elem "--version" args -> 
            putStrLn "Version 2.0" 
        args | elem "-t" args || elem "--test" args -> do
            putStrLn "Running tests"
            sequence $ map processArgs (and_tests testDir)
            return ()
        [formula] -> do 
            s <- getContents
            proceed (parseAigerFromString s) formula Nothing Nothing Nothing 
        [formula,"-s",strategy_file] -> do 
            -- putStrLn strategy_file
            s <- getContents
            -- read strategy aiger from file and give it to the necessary functions
            strategy_string <- readFile strategy_file
            -- putStrLn "strategy AIGER:" 
            -- putStrLn strategy_string
            proceed (parseAigerFromString s) formula (Just (parseAigerFromString strategy_string)) Nothing Nothing
        [file,formula] -> do
            eitherAIG <- parseAigerFromFile file
            proceed eitherAIG formula Nothing (Just file) Nothing 
        [file,formula,out,"-s",strategy_file] -> do 
            s <- getContents
            -- putStrLn "correct case!"
            strategy_string <- readFile strategy_file
            proceed (parseAigerFromString s) formula (Just (parseAigerFromString strategy_string)) (Just file) (Just out)
        file : formula : out : _ ->  do
            -- putStrLn "wrong Case..." 
            eitherAIG <- parseAigerFromFile file
            proceed eitherAIG formula Nothing (Just file) (Just out) 
--        [_] -> error "An error occurred. Check usage of the tool."
    
        
proceed :: Either ParseError Aiger -> String -> Maybe (Either ParseError Aiger) -> Maybe String -> Maybe String -> IO () 
proceed eitherAIG formula maybeEitherStrategy inM outM = do 
    case eitherAIG of 
        Right aig -> do 
            debug inM ("\n\nChecking " ++ show inM ++ " for property " ++ show formula)
            debug inM ("Original AIG: ")
            debug inM (show aig)
            let phi = read formula 
            --let aig' = formula2Aiger aig phi
            let aig' = case maybeEitherStrategy of 
                           Just eitherStrategy -> do
                               case eitherStrategy of
                                   Right strategy_aiger ->
                                       formula2Aiger aig phi (Just strategy_aiger)
                                   Left x -> emptyAiger
                           Nothing -> 
                               formula2Aiger aig phi Nothing
            debug inM "Final AIG:"
            debug inM $ show aig'
            case inM of
                Nothing -> putStr $ printAiger aig'
                _       -> writeFile (show inM ++ ".out.aag") $ printAiger aig' 
            
            {- translate to binary format: -}
            -- createProcess (shell $ "aiger-1.9.4/aigtoaig " 
                -- ++ file ++ "out.aag " 
                -- ++ file ++ "out.aig") 
            
            -- createProcess (shell "abc10216.exe -c ra " ++ file ++ "out.aig") 
        Left x -> print x
    debug inM "Done!"

testDir = "testmodels\\"


duplicate :: String -> Maybe String -> IO ()
duplicate file outM = do
    eitherAIG <- parseAigerFromFile file
    case eitherAIG of 
        Right aig -> do 
            print "Original AIG:"
            print aig
            print "\n"
            let (aig2, emptyMap) = compose (aig, Map.empty) aig False (flip (++) "'") 
            -- let aig2 = foldl compose aig $ replicate 3 aig
            print "Self composed AIG:"
            print aig2
            writeFile "aiger-1.9.4/examples/testoutput.aag" $ printAiger aig2
            
        Left x -> print x
    print "Done!"






