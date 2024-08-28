{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Type where

import Language.Haskell.TH.Quote (QuasiQuoter)
import TypedSession.TH (protocol)

data PingPongRole = Client | Server
  deriving (Show, Eq, Ord, Enum, Bounded)

data PingPongBranchSt = PingPongBranchSt
  deriving (Show, Eq, Ord, Enum, Bounded)

pingpongProtocol :: QuasiQuoter
pingpongProtocol =
  protocol
    @PingPongRole
    @PingPongBranchSt
    "PingPong"
    ''PingPongRole
    ''PingPongBranchSt
