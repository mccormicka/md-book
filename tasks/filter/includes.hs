#!/usr/bin/env runhaskell
-- includes.hs
-- ghc --make tasks/filter/includes.hs
-- code taken from the pandoc scripting tutorial
-- http://johnmacfarlane.net/pandoc/scripting.html
import Text.Pandoc.JSON

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> return . (CodeBlock (id, classes, namevals)) =<< readFile f
       Nothing    -> return cb
doInclude x = return x

main :: IO ()
main = toJSONFilter doInclude