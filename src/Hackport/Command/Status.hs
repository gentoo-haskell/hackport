module Hackport.Command.Status
  ( statusAction
  ) where

import Status

import Hackport.Env
import Hackport.Util

statusAction :: Env StatusEnv ()
statusAction = withHackportContext (\_ -> runStatus)
