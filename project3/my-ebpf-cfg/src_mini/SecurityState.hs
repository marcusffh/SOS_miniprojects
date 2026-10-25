
module SecurityState where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Ebpf.Asm (Reg)
import Security

data SecurityState = SecurityState
  { registerLevels    :: Map Reg SecurityLevel
  , memoryLevels      :: Map Int SecurityLevel
  , unknownMemoryLevel :: SecurityLevel
  } deriving (Eq, Show)

emptySecurityState :: SecurityState
emptySecurityState = SecurityState
  { registerLevels = Map.empty
  , memoryLevels = Map.empty
  , unknownMemoryLevel = Low
  }

setRegisterLevel
  :: Reg -> SecurityLevel -> SecurityState -> SecurityState
setRegisterLevel reg level state =
  state
    { registerLevels =
        Map.insert reg level (registerLevels state)
    }

setMemoryLevel
  :: Int -> SecurityLevel -> SecurityState -> SecurityState
setMemoryLevel address level state =
  state
    { memoryLevels =
        Map.insert address level (memoryLevels state)
    }

getRegisterLevel
  :: Reg -> SecurityState -> Maybe SecurityLevel
getRegisterLevel reg state =
  Map.lookup reg (registerLevels state)

getMemoryLevel
  :: Int -> SecurityState -> Maybe SecurityLevel
getMemoryLevel address state =
  Map.lookup address (memoryLevels state)

joinSecurityState :: SecurityState -> SecurityState -> SecurityState
joinSecurityState state1 state2 =
  SecurityState
    { registerLevels =
        joinMaps (registerLevels state1) (registerLevels state2)
    , memoryLevels =
        joinMaps (memoryLevels state1) (memoryLevels state2)
    , unknownMemoryLevel =
        joinLevel (unknownMemoryLevel state1) (unknownMemoryLevel state2)
    }

joinMaps
    :: Ord k
    => Map k SecurityLevel
    -> Map k SecurityLevel
    -> Map k SecurityLevel
joinMaps map1 map2 =
    Map.fromList
        [ (key, joinLevel level1 level2)
        | key <- Map.keys (Map.union map1 map2)
        , let level1 = Map.findWithDefault High key map1
        , let level2 = Map.findWithDefault High key map2
        ]
