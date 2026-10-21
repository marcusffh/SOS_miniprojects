
module SecurityState where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Ebpf.Asm (Reg)
import Security

-- registerLevels maps each register to its security level. For example r1 might be High and r2 might be low
-- memoryLevels maps each tracked memory location to its security level
-- SecurityLevel comes from the file we created in the previous step.
data SecurityState = SecurityState
  { registerLevels :: Map Reg SecurityLevel
  , memoryLevels   :: Map Int SecurityLevel
  } deriving (Eq, Show)


--Creates a state with no register or memory information recorded yet
emptySecurityState :: SecurityState
emptySecurityState = SecurityState
  { registerLevels = Map.empty
  , memoryLevels   = Map.empty
  }


-- Funcions update one entry while preserving everything else in the state.
-- For example, setting r1 to High won't change the level of r2 or any memory location
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


-- How states are combined. Suppose one path reaches an instruction with r1 = low
--, while another reaches it with r1 = High. At the merge point, we must treat r1 as High.
joinSecurityState :: SecurityState -> SecurityState -> SecurityState
joinSecurityState state1 state2 =
  SecurityState
    { registerLevels =
        joinMaps (registerLevels state1) (registerLevels state2)
    , memoryLevels =
        joinMaps (memoryLevels state1) (memoryLevels state2)
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
