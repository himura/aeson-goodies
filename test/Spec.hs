{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson
import Data.Aeson.Goodies.NumBool
import Data.Aeson.Goodies.StringInt

main :: IO ()
main = defaultMain $ testGroup "Tests" [numBoolTests, stringIntTests]

numBoolTests :: TestTree
numBoolTests =
    testGroup
        "NumBool"
        [ testCase "valid:   bool true" $ (decode "[true]" :: Maybe [NumBool]) @?= Just [NumBool True]
        , testCase "valid:   bool false" $ (decode "[false]" :: Maybe [NumBool]) @?= Just [NumBool False]
        , testCase "valid:   number 1" $ (decode "[1]" :: Maybe [NumBool]) @?= Just [NumBool True]
        , testCase "valid:   number 100" $ (decode "[100]" :: Maybe [NumBool]) @?= Just [NumBool True]
        , testCase "valid:   number 0" $ (decode "[0]" :: Maybe [NumBool]) @?= Just [NumBool False]
        , testCase "valid:   number -1" $ (decode "[-1]" :: Maybe [NumBool]) @?= Just [NumBool False]
        , testCase "valid:   string true" $ (decode "[\"true\"]" :: Maybe [NumBool]) @?= Just [NumBool True]
        , testCase "valid:   string false" $ (decode "[\"false\"]" :: Maybe [NumBool]) @?= Just [NumBool False]
        , testCase "valid:   string True" $ (decode "[\"True\"]" :: Maybe [NumBool]) @?= Just [NumBool True]
        , testCase "valid:   string False" $ (decode "[\"False\"]" :: Maybe [NumBool]) @?= Just [NumBool False]
        , testCase "valid:   string 1" $ (decode "[\"1\"]" :: Maybe [NumBool]) @?= Just [NumBool True]
        ]

stringIntTests :: TestTree
stringIntTests =
    testGroup
        "StringInt Int"
        [ testCase "valid:   number 43" $ (decode "[43]" :: Maybe [StringInt Int]) @?= Just [StringInt 43]
        , testCase "valid:   number -100" $ (decode "[-100]" :: Maybe [StringInt Int]) @?= Just [StringInt (-100)]
        , testCase "invalid: number 1.5" $ (decode "[1.5]" :: Maybe [StringInt Int]) @?= Nothing
        , testCase "invalid: number -100.5" $ (decode "[-100.5]" :: Maybe [StringInt Int]) @?= Nothing
        , testCase "valid:   number 1e10" $ (decode "[1e10]" :: Maybe [StringInt Int]) @?= Just [StringInt 10000000000]
        , testCase "valid:   number 1e50000000" $ (decode "[1e50000000]" :: Maybe [StringInt Int]) @?= Nothing -- reject huge exponents
        , testCase "valid:   string 43" $ (decode "[\"43\"]" :: Maybe [StringInt Int]) @?= Just [StringInt 43]
        , testCase "valid:   string -100" $ (decode "[\"-100\"]" :: Maybe [StringInt Int]) @?= Just [StringInt (-100)]
        , testCase "invalid: string 1.5" $ (decode "[\"1.5\"]" :: Maybe [StringInt Int]) @?= Nothing
        , testCase "invalid: string -100.5" $ (decode "[\"-100.5\"]" :: Maybe [StringInt Int]) @?= Nothing
        , testCase "invalid: string 1e10" $ (decode "[\"1e10\"]" :: Maybe [StringInt Int]) @?= Nothing
        , testCase "invalid: string 1e-10" $ (decode "[\"1e10\"]" :: Maybe [StringInt Int]) @?= Nothing
        ]
