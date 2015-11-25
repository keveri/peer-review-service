module PeerReview.DBConnection
    ( mkConnBuilder
    ) where

import qualified Database.PostgreSQL.Simple as PG
import           Web.Spock.Shared

import           PeerReview.Types

mkConnBuilder :: DBInfo -> ConnBuilder PG.Connection
mkConnBuilder dbInfo =
    ConnBuilder { cb_createConn = PG.connect $ mkConnInfo dbInfo
                , cb_destroyConn = PG.close
                , cb_poolConfiguration =
                    PoolCfg { pc_stripes = 1
                            , pc_resPerStripe = 5
                            , pc_keepOpenTime = 60
                            }
                }


mkConnInfo :: DBInfo -> PG.ConnectInfo
mkConnInfo (DBInfo h p u pass n) =
    PG.ConnectInfo h (fromIntegral p) u pass n