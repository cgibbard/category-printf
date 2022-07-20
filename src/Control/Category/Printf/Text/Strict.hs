module Control.Category.Printf.Text.Strict
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

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO (Handle)

printf :: Format Text (IO ()) b -> b
printf = printfWith T.putStr

printfLn :: Format Text (IO ()) b -> b
printfLn = printfWith T.putStrLn 

hPrintf :: Handle -> Format Text (IO ()) b -> b
hPrintf h = printfWith (T.hPutStr h)

hPrintfLn :: Handle -> Format Text (IO ()) b -> b
hPrintfLn h = printfWith (T.hPutStrLn h)

-- | padLeft s n f will transform the formatter f, ensuring that the output produced is at least length n
-- by appending sufficiently many copies of s on the left. The string s should have length at least 1,
-- otherwise this has no effect. In cases where s has length greater than 1, the last occurrence of s
-- will be truncated as necessary to fit the necessary width.
padLeft :: Text -> Int -> Format Text a b -> Format Text a b
padLeft s _ f | T.length s < 1 = f
padLeft s n f = (`mapMonoid` f) $ \x ->
  let k = T.length x
      l = T.length s
      (q,r) = (n - k) `divMod` l
  in mconcat (replicate q s) <> T.take r s <> x

-- | padRight s n f will transform the formatter f, ensuring that the output produced is at least length n
-- by appending sufficiently many copies of s on the right. The string s should have length at least 1,
-- otherwise this has no effect. In cases where s has length greater than 1, the last occurrence of s
-- will be truncated as necessary to fit the necessary width.
padRight :: Text -> Int -> Format Text a b -> Format Text a b
padRight s _ f | T.length s < 1 = f
padRight s n f = (`mapMonoid` f) $ \x ->
  let k = T.length x
      l = T.length s
      (q,r) = (n - k) `divMod` l
  in x <> mconcat (replicate q s) <> T.take r s