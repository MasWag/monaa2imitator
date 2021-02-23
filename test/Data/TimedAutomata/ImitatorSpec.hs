{-# LANGUAGE OverloadedStrings #-}
module Data.TimedAutomata.ImitatorSpec (spec) where

import Test.Hspec
import Data.TimedAutomata.Imitator

spec :: Spec
spec = do
  describe "fromTextSingle" $ do
    it "le" $ do
      let input = "x0 <= 100"
          expected = Data.TimedAutomata.Imitator.LE "x0" "100"
        in
        fromTextSingle input `shouldBe` expected
    it "lt" $ do
      let input = "x0 < 10"
          expected = Data.TimedAutomata.Imitator.LT "x0" "10"
        in
        fromTextSingle input `shouldBe` expected
    it "ge" $ do
      let input = "x0 >= -100"
          expected = Data.TimedAutomata.Imitator.GE "x0" "-100"
        in
        fromTextSingle input `shouldBe` expected
    it "gt" $ do
      let input = "x0 > 20"
          expected = Data.TimedAutomata.Imitator.GT "x0" "20"
        in
        fromTextSingle input `shouldBe` expected
  describe "show update" $ do
    it "show update" $ do
      let variable = Variable "roboClock"
          update = Update variable "0"
          expected = "xroboClock := 0"
        in
        show update `shouldBe` expected
        
  describe "show transition" $ do
    it "show transition" $ do
      let bcet = "BCET_F3_F4"
          wcet = "WCET_F3_F4"
          clock = "roboClock"
          guard = AND (Data.TimedAutomata.Imitator.LT bcet clock) (Data.TimedAutomata.Imitator.LT clock wcet)
          label = "enterF4"
          update = Update (Variable clock) "0"
          target = "enterF4"
          transition = Transition guard (Just label) [update] target
          expected = "when BCET_F3_F4 < roboClock & roboClock < WCET_F3_F4 sync enterF4 do {xroboClock := 0} goto enterF4;"
        in
        show transition `shouldBe` expected

  describe "show location" $ do
    it "show location" $ do
      let bcet = "BCET_F3_F4"
          wcet = "WCET_F3_F4"
          clock = "roboClock"
          guard = AND (Data.TimedAutomata.Imitator.LT bcet clock) (Data.TimedAutomata.Imitator.LT clock wcet)
          label = "enterF4"
          update = Update (Variable clock) "0"
          target = "enterF4"
          transition = Transition guard (Just label) [update] target
          invariant = Data.TimedAutomata.Imitator.LE clock wcet
          location = Loc "leaveF3" invariant [transition]
          expected = ["loc leaveF3: invariant roboClock <= WCET_F3_F4",
                      "\twhen BCET_F3_F4 < roboClock & roboClock < WCET_F3_F4 sync enterF4 do {xroboClock := 0} goto enterF4;"]
        in
        show location `shouldBe` (unlines expected)

    describe "Property" $ do
      it "show Property" $ do
        let unsafeLoc = (Loc "unsafe" Data.TimedAutomata.Imitator.True [])
            g1 = Automata "G1" [] [] unsafeLoc
            g2 = Automata "G2" [] [] unsafeLoc
            f1 = Automata "F1" [] [] unsafeLoc
            f2 = Automata "F2" [] [] unsafeLoc
            f3 = Automata "F3" [] [] unsafeLoc
            f4 = Automata "F4" [] [] unsafeLoc
            property = Unreachable [(g1, unsafeLoc), (g2, unsafeLoc), (f1, unsafeLoc), (f2, unsafeLoc), (f3, unsafeLoc), (f4, unsafeLoc)]
            expected = ["property := unreachable",
                        "\tloc[G1] = unsafe or",
                        "\tloc[G2] = unsafe or",
                        "\tloc[F1] = unsafe or",
                        "\tloc[F2] = unsafe or",
                        "\tloc[F3] = unsafe or",
                        "\tloc[F4] = unsafe",
                        ";"]
          in
          show property `shouldBe` (unlines expected)

  describe "Automata" $ do
    it "show Automata" $ do
      let clock = VariableWithV "G1Clock" "0"
          initialLoc = (Loc "initial" Data.TimedAutomata.Imitator.True [Transition Data.TimedAutomata.Imitator.True (Just "waterG1") [Update clock "0"] "loop"])
          waterG1  = "waterG1"
          initialToLoop = Transition Data.TimedAutomata.Imitator.True (Just waterG1) [Update clock "0"] "loop"
          initial  = Loc "initial" Data.TimedAutomata.Imitator.True [initialToLoop]
          loopTransition = Transition (Data.TimedAutomata.Imitator.LE "G1Clock" "worst_watering_interval") (Just waterG1) [Update clock "0"] "loop"
          loopToUnsafe = Transition (Data.TimedAutomata.Imitator.GT "G1Clock" "worst_watering_interval") Nothing [] "unsafe"
          loop     = Loc "loop" Data.TimedAutomata.Imitator.True [loopTransition, loopToUnsafe]
          unsafe   = Loc "unsafe" Data.TimedAutomata.Imitator.True []
          automata = Automata "G1" [waterG1] [initial, loop, unsafe] initial
          expected = ["(************************************************************)",
                      "  automaton G1",
                      "(************************************************************)",
                      "synclabs: waterG1;",
                      "",
                      "loc initial: invariant True",
                      "\twhen True sync waterG1 do {G1Clock := 0} goto loop;",
                      "",
                      "loc loop: invariant True",
                      "\twhen G1Clock <= worst_watering_interval sync waterG1 do {G1Clock := 0} goto loop;",
                      "\twhen G1Clock > worst_watering_interval goto unsafe;",
                      "",
                      "loc unsafe: invariant True",
                      "end (* G1 *)"]
        in
        show automata `shouldBe` (unlines expected)

  describe "Model" $ do
    it "show Model" $ do
      let clock = VariableWithV "G1Clock" "0"
          param = VariableWithC "worst_watering_interval" (Data.TimedAutomata.Imitator.GE "worst_watering_interval" "0")
          initialLoc = (Loc "initial" Data.TimedAutomata.Imitator.True [Transition Data.TimedAutomata.Imitator.True (Just "waterG1") [Update clock "0"] "loop"])
          waterG1  = "waterG1"
          initialToLoop = Transition Data.TimedAutomata.Imitator.True (Just waterG1) [Update clock "0"] "loop"
          initial  = Loc "initial" Data.TimedAutomata.Imitator.True [initialToLoop]
          loopTransition = Transition (Data.TimedAutomata.Imitator.LE "G1Clock" "worst_watering_interval") (Just waterG1) [Update clock "0"] "loop"
          loopToUnsafe = Transition (Data.TimedAutomata.Imitator.GT "G1Clock" "worst_watering_interval") Nothing [] "unsafe"
          loop     = Loc "loop" Data.TimedAutomata.Imitator.True [loopTransition, loopToUnsafe]
          unsafe   = Loc "unsafe" Data.TimedAutomata.Imitator.True []
          automata = Automata "G1" [waterG1] [initial, loop, unsafe] initial
          property = Unreachable [(automata, unsafe)]
          model = Model [clock] [] [param] [automata] property
          expected = ["var",
                      "",
                      "\tG1Clock,",
                      "\t\t: clock;",
                      "",
                      "\tworst_watering_interval,",
                      "\t\t: parameter;",
                      "",
                      "(************************************************************)",
                      "  automaton G1",
                      "(************************************************************)",
                      "synclabs: waterG1;",
                      "",
                      "loc initial: invariant True",
                      "\twhen True sync waterG1 do {G1Clock := 0} goto loop;",
                      "",
                      "loc loop: invariant True",
                      "\twhen G1Clock <= worst_watering_interval sync waterG1 do {G1Clock := 0} goto loop;",
                      "\twhen G1Clock > worst_watering_interval goto unsafe;",
                      "",
                      "loc unsafe: invariant True",
                      "end (* G1 *)",
                      "",
                      "(************************************************************)",
                      "(* Initial state *)",
                      "(************************************************************)",
                      "",
                      "init :=",
                      "\t(*------------------------------------------------------------*)",
                      "\t(* Initial location *)",
                      "\t(*------------------------------------------------------------*)",
                      "\t& loc[G1] = initial",
                      "",
                      "\t(*------------------------------------------------------------*)",
                      "\t(* Initial clock constraints *)",
                      "\t(*------------------------------------------------------------*)",
                      "\t& G1Clock = 0",
                      "",
                      "\t(*------------------------------------------------------------*)",
                      "\t(* Parameter constraints *)",
                      "\t(*------------------------------------------------------------*)",
                       "\t& worst_watering_interval >= 0",
                       ";",
                       "",
                       -- "(************************************************************)",
                       -- "(* Property specification *)",
                       -- "(************************************************************)",
                       -- "",
                       -- "property := unreachable",
                       -- "\tloc[G1] = unsafe",
                       -- ";",
                       -- "",
                       "(************************************************************)",
                       "(* The end *)",
                       "(************************************************************)",
                       "end"]
        in
        show model `shouldBe` (unlines expected)
    it "show Model without parameters" $ do
      let clock = VariableWithV "G1Clock" "0"
          initialLoc = (Loc "initial" Data.TimedAutomata.Imitator.True [Transition Data.TimedAutomata.Imitator.True (Just "waterG1") [Update clock "0"] "loop"])
          waterG1  = "waterG1"
          initialToLoop = Transition Data.TimedAutomata.Imitator.True (Just waterG1) [Update clock "0"] "loop"
          initial  = Loc "initial" Data.TimedAutomata.Imitator.True [initialToLoop]
          loopTransition = Transition (Data.TimedAutomata.Imitator.LE "G1Clock" "worst_watering_interval") (Just waterG1) [Update clock "0"] "loop"
          loopToUnsafe = Transition (Data.TimedAutomata.Imitator.GT "G1Clock" "worst_watering_interval") Nothing [] "unsafe"
          loop     = Loc "loop" Data.TimedAutomata.Imitator.True [loopTransition, loopToUnsafe]
          unsafe   = Loc "unsafe" Data.TimedAutomata.Imitator.True []
          automata = Automata "G1" [waterG1] [initial, loop, unsafe] initial
          property = Unreachable [(automata, unsafe)]
          model = Model [clock] [] [] [automata] property
          expected = ["var",
                      "",
                      "\tG1Clock,",
                      "\t\t: clock;",
                      "",
                      "(************************************************************)",
                      "  automaton G1",
                      "(************************************************************)",
                      "synclabs: waterG1;",
                      "",
                      "loc initial: invariant True",
                      "\twhen True sync waterG1 do {G1Clock := 0} goto loop;",
                      "",
                      "loc loop: invariant True",
                      "\twhen G1Clock <= worst_watering_interval sync waterG1 do {G1Clock := 0} goto loop;",
                      "\twhen G1Clock > worst_watering_interval goto unsafe;",
                      "",
                      "loc unsafe: invariant True",
                      "end (* G1 *)",
                      "",
                      "(************************************************************)",
                      "(* Initial state *)",
                      "(************************************************************)",
                      "",
                      "init :=",
                      "\t(*------------------------------------------------------------*)",
                      "\t(* Initial location *)",
                      "\t(*------------------------------------------------------------*)",
                      "\t& loc[G1] = initial",
                      "",
                      "\t(*------------------------------------------------------------*)",
                      "\t(* Initial clock constraints *)",
                      "\t(*------------------------------------------------------------*)",
                      "\t& G1Clock = 0",
                      "",
                      "\t(*------------------------------------------------------------*)",
                      "\t(* Parameter constraints *)",
                      "\t(*------------------------------------------------------------*)",
                       ";",
                       "",
                       -- "(************************************************************)",
                       -- "(* Property specification *)",
                       -- "(************************************************************)",
                       -- "",
                       -- "property := unreachable",
                       -- "\tloc[G1] = unsafe",
                       -- ";",
                       -- "",
                       "(************************************************************)",
                       "(* The end *)",
                       "(************************************************************)",
                       "end"]
        in
        show model `shouldBe` (unlines expected)
