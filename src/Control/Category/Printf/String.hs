module Control.Category.Printf.String 
       (module Control.Category.Printf
       , printf
       , printfLn
       , hPrintf
       , hPrintfLn
       , padLeft
       , padRight
       ) where

import Control.Comonad
import Control.Category.Printf
import System.IO (Handle, hPutStr, hPutStrLn)

printf :: Format String (IO ()) b -> b
printf = printfWith putStr

printfLn :: Format String (IO ()) b -> b
printfLn = printfWith putStrLn

hPrintf :: Handle -> Format String (IO ()) b -> b
hPrintf h = printfWith (hPutStr h)

hPrintfLn :: Handle -> Format String (IO ()) b -> b
hPrintfLn h = printfWith (hPutStrLn h)

-- | padLeft s n f will transform the formatter f, ensuring that the output produced is at least length n
-- by appending sufficiently many copies of s on the left. The string s should have length at least 1,
-- otherwise this has no effect. In cases where s has length greater than 1, the last occurrence of s
-- will be truncated as necessary to fit the necessary width.
padLeft :: String -> Int -> Format String a b -> Format String a b
padLeft s _ f | length s < 1 = f
padLeft s n f = (`mapMonoid` f) $ \x ->
  let k = length x
      d = n - k
  in take d (cycle s) ++ x

-- | padRight s n f will transform the formatter f, ensuring that the output produced is at least length n
-- by appending sufficiently many copies of s on the right. The string s should have length at least 1,
-- otherwise this has no effect. In cases where s has length greater than 1, the last occurrence of s
-- will be truncated as necessary to fit the necessary width.
padRight :: String -> Int -> Format String a b -> Format String a b
padRight s _ f | length s < 1 = f
padRight s n f = (`mapMonoid` f) $ \x ->
  let k = length x
      d = n - k
  in x ++ take d (cycle s)