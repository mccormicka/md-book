#!/usr/bin/env runhaskell
-- includes.hs
import Text.Pandoc.JSON

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> do
         file <- readFile f
         let n = "```js\n" ++ file ++ "```\n"
         return (CodeBlock (id, classes, namevals) n)
       Nothing    -> return cb
doInclude x = return x

main :: IO ()
main = toJSONFilter doInclude

---- includes.hs
--import Text.Pandoc.JSON
--import Debug.Trace
--
--prefix :: String -> String
--prefix s = show "something"
--doInclude :: Block -> IO Block
----doInclude cb@(CodeBlock (id, classes, namevals) contents) =
----  case lookup "include" namevals of
----       Just f     -> return . trace (f) (CodeBlock (id, classes, namevals)) =<< readFile f
----       Nothing    -> return cb
----doInclude x = return x
--
--doInclude cb@(CodeBlock (id, classes, namevals) contents)
--  | Just f <- lookup "include" namevals =
--       return . trace (contents) (CodeBlock (id, classes, namevals)) =<< readFile f
--  | Just format <- lookup "format" namevals =
--    return . trace ("Reading format") (CodeBlock (id, classes, namevals)) =<< readFile format
--doInclude x = return x
--
--main :: IO ()
--main = toJSONFilter doInclude
----extractCode :: IORef (S.Set String) -> IORef (M.Map String [(String,String)]) ->
----               Block -> IO Block
----
----extractCode r m b@(CodeBlock (id,cls,attrs) code)
----  | Just fileName <- lookup "file" attrs =
----    do s <- readIORef r
----       if S.member fileName s
----         then appendFile fileName (code ++ "\n")
----         else do writeFile fileName (code ++ "\n")
----                 writeIORef r (S.insert fileName s)
----       return b
----  | Just template <- lookup "template" attrs,
----    Just var      <- lookup "var" attrs =
----      do t <- readIORef m
----         writeIORef m (M.insertWith (++) template [(var,code ++ "\n")] t)
----         return b
----extractCode r m b = return b