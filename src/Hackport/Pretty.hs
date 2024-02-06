module Hackport.Pretty
    ( renderDoc
    , prettyShow
    , module Prettyprinter
    ) where

import Prettyprinter hiding (cat)
import Prettyprinter.Render.String

renderDoc :: Doc ann -> String
renderDoc
    = renderString
    . layoutPretty defaultLayoutOptions { layoutPageWidth = Unbounded }

prettyShow :: Pretty a => a -> String
prettyShow = renderDoc . pretty
