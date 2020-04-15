{-# LANGUAGE OverloadedStrings #-}

module Frontend.Servant where

import Data.JSString (JSString)
import qualified Data.JSString.Internal.Type as JSString
import qualified GHCJS.Prim as Prim

-- When this is inlined, the bug goes away.
-- Also, when this is moved to Main (with NOINLINE), the bug goes away.
{-# NOINLINE joinStrings #-}
joinStrings :: String -> String -> String -> JSString
joinStrings a b c = toJSString ('1' : a) <> toJSString b <> toJSString c
  where toJSString = JSString.JSString . Prim.toJSString
