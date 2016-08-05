

data GenHaxl u a = GenHaxl 
    { unHaxl :: Env u -> IORef (RequestStore u) -> IO (Result u a) }

data Env u = Env
  { cacheRef     :: IORef (DataCache ResultVar) -- cached data fetches
  , memoRef      :: IORef (DataCache (MemoVar u))   -- memoized computations
  , flags        :: Flags
  , userEnv      :: u
  , statsRef     :: IORef Stats
  , states       :: StateStore
  -- ^ Data sources and other components can store their state in
  -- here. Items in this store must be instances of 'StateKey'.
  }

data RequestStore = RequestStore 
    { 
