{-# LANGUAGE OverloadedStrings #-}

module MNRLUtils where

import Control.Lens

import MNRLTypes

emptyMNRL :: MNRL
emptyMNRL = MNRL [] "__0__"

addComponent :: Component -> MNRL -> MNRL
addComponent c m = m & mnrlComponents %~ (c:)

addCounter :: Counter -> MNRL -> MNRL
addCounter = addComponent . CompCounter

addState :: State -> MNRL -> MNRL
addState = addComponent . CompState

addHState :: HState -> MNRL -> MNRL
addHState = addComponent . CompHState

addGate :: Gate -> MNRL -> MNRL
addGate = addComponent . CompGate
