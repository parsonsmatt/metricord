module Metricord.MigrateSpec where

import TestImport

import Text.Read

import Metricord.Migrate

spec :: Spec
spec = do
    describe "MigrateColumn" $ do
        describe "Read/Show" $ do
            let subject = MigrateColumn "name" "int"
            it "renders as a colon separated values" $ do
                show subject
                    `shouldBe`
                        "name:int"
            it "round-trips" $ do
                read (show subject)
                    `shouldBe`
                        subject
