module Control.Category.Printf.ByteString.Lazy
       ( module Control.Category.Printf
       , printf
       , printfLn
       , hPrintf
       , hPrintfLn
       , padLeft
       , padRight
       ) where

import Prelude hiding (id, (.))

import Control.Category.Printf
import Control.Comonad
import Data.Monoid
import Data.Int (Int64)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

import System.IO (Handle)

printf :: Format ByteString (IO ()) b -> b
printf = printfWith B.putStr

printfLn :: Format ByteString (IO ()) b -> b
printfLn = printfWith BC.putStrLn 

hPrintf :: Handle -> Format ByteString (IO ()) b -> b
hPrintf h = printfWith (B.hPutStr h)

hPrintfLn :: Handle -> Format ByteString (IO ()) b -> b
hPrintfLn h = printfWith (BC.hPutStrLn h)

-- | padLeft s n f will transform the formatter f, ensuring that the output produced is at least length n
-- by appending sufficiently many copies of s on the left. The string s should have length at least 1,
-- otherwise this has no effect. In cases where s has length greater than 1, the last occurrence of s
-- will be truncated as necessary to fit the necessary width.
padLeft :: ByteString -> Int64 -> Format ByteString a b -> Format ByteString a b
padLeft s _ f | B.length s < 1 = f
padLeft s n f = (`mapMonoid` f) $ \x ->
  let k = B.length x
      d = n - k
  in B.take d (B.cycle s) <> x

-- | padRight s n f will transform the formatter f, ensuring that the output produced is at least length n
-- by appending sufficiently many copies of s on the right. The string s should have length at least 1,
-- otherwise this has no effect. In cases where s has length greater than 1, the last occurrence of s
-- will be truncated as necessary to fit the necessary width.
padRight :: ByteString -> Int64 -> Format ByteString a b -> Format ByteString a b
padRight s _ f | B.length s < 1 = f
padRight s n f = (`mapMonoid` f) $ \x ->
  let k = B.length x
      d = n - k
  in x <> B.take d (B.cycle s)