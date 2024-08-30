{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Type where

import Language.Haskell.TH.Quote (QuasiQuoter)
import TypedSession.TH (protocol)

data PingPongRole = Client | Server | Counter
  deriving (Show, Eq, Ord, Enum, Bounded)

data PingPongBranchSt = Continue | Finish | Check | Successed | Failed
  deriving (Show, Eq, Ord, Enum, Bounded)

pingpongProtocol :: QuasiQuoter
pingpongProtocol =
  protocol
    @PingPongRole
    @PingPongBranchSt
    "PingPong"
    ''PingPongRole
    ''PingPongBranchSt
