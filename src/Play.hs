module Play where

import Control.Lens (makeLenses)
import Control.Monad.RWS
import Control.Monad.Logger (LoggingT, runStderrLoggingT)
import RuleSet

data PlayState = PlayState {
    _play_number_steps :: Int,
    _play_prisoners_black :: Int,
    _play_prisoners_white :: Int
}
makeLenses ''PlayState

emptyState :: PlayState
emptyState = PlayState {
    _play_number_steps    = 0,
    _play_prisoners_black = 0,
    _play_prisoners_white = 0
}

data HyperParms = HyperParms {
    _parm_descend_threshold :: Int,
    _parm_number_descends :: Int,
    _parm_constant_puct :: Float,
    _parm_resign_threshold :: Float,
    _parm_temperature :: Float,
    _parm_epsilon :: Float
}
makeLenses ''HyperParms

data PlayConfig = PlayConfig { 
      _play_ruleset :: RuleSet,
      _play_allow_resign :: Bool,
      _play_hyper_parms :: HyperParms
    }
makeLenses ''PlayConfig

defaultConfig :: PlayConfig
defaultConfig = PlayConfig {
    _play_ruleset = chinese,
    _play_allow_resign  = True,
    _play_hyper_parms   = HyperParms {
        _parm_descend_threshold = 50,
        _parm_number_descends   = 100,
        _parm_constant_puct     = 1.0,
        _parm_resign_threshold  = 0.2,
        _parm_temperature       = 0.9,
        _parm_epsilon           = 0.1
    }
}

type Play = LoggingT (RWST PlayConfig () PlayState IO)

play :: PlayConfig -> Play a -> IO a
play c m = do
    (a,_) <- evalRWST (runStderrLoggingT m) c emptyState
    return a

io :: IO a -> Play a
io = liftIO