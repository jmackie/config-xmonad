module Language.Haskell.TH.Quote.Text (text) where

import Prelude

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))


text :: QuasiQuoter 
text = QuasiQuoter 
    { quoteExp = TH.litE . TH.stringL
    , quotePat = undefined 
    , quoteType = undefined 
    , quoteDec = undefined 
    }
