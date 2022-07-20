{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies, RankNTypes #-}

module Control.Category.Printf 
  ( 
  -- | Note that you'll probably want to import both this module and one of the other string-type specific modules depending on
  -- which sort of strings you'll be working with. You'll also probably want to turn on the OverloadedStrings extension so as to
  -- be able to use string literals as formatters which insert themselves.
    module Control.Category
  -- * Basics
  , Format
  , printfWith
  , sprintf
  , c
  , i
  , spliceWith
  , s
  , bind
  , mapMonoid
  , generalizeString
  -- * Numeric formatting
  , signedWith
  , intAtBase
  , hex
  , oct
  , eFloat
  , fFloat
  , gFloat
  -- * Argument stack manipulation
  , push
  , dup
  , swap
  , skip
  , apply
  , apply2
  ) where

import Prelude hiding (id, (.), length)
import Data.List (genericLength, genericReplicate)
import Control.Arrow
import Control.Category
import Data.Monoid
import Control.Comonad
import Numeric

import Data.String

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- | Handy type synonym for the things we're working with.
-- You should regard a value of type @Format m a b@ as something which explains how to write
-- some element of the monoid @m@ (a "string" for our purposes), and which will change the type
-- of printf from @a@ to @b@. For instance, something which adds a responsibility to provide an
-- additional argument of type @t@ might have type @Format m a (t -> a)@, while a formatter which
-- somehow absolves you of that responsibility would have type @Format m (t -> a) a@.
type Format m = Cokleisli ((->) m)

-- | We can apply this to something like putStrLn to get a function for formatted printing.
-- Typically you'll have @r = IO ()@, but that needn't be the case.
printfWith :: (m -> r) -> Format m r b -> b
printfWith printer f = runCokleisli f printer

-- | If you just want to build a string / element of your monoid, we have @sprintf = printfWith id@
sprintf :: Format m m b -> b
sprintf = printfWith id

instance (a ~ b, IsString s, Monoid s) => IsString (Cokleisli ((->) s) a b) where
  fromString = c . fromString

-- | Formatter for a constant string. Note that for string literals, this is implicit if you turn on
-- the OverloadedStrings extension.
--
-- >>> let x = "world" in printfLn (c "Hello, " . c x . c "!")
-- Hello, world!
c :: (Monoid m) => m -> Format m a a
c x = Cokleisli ($ x)

-- | Inclusion of a string as an argument.
-- 
-- >>> mapM_ (printfLn ("Hello, " . i . "!")) ["Anne", "Bob", "world"]
-- Hello, Anne!
-- Hello, Bob!
-- Hello, world!
i :: Format m a (m -> a)
i = Cokleisli id

-- | Given a way to turn a value of type t into a string, this builds a
-- formatter which demands an additional argument of type t and splices it in.
spliceWith :: (Monoid m) => (t -> m) -> Format m a (t -> a)
spliceWith f = Cokleisli (. f)

-- | Splice in anything showable.
-- 
-- >>> printfLn ("list: " . s . "  tuple: " . s . "  string: " . s) [1,2,3] ("hello", 'a') "there"
-- list: [1,2,3]  tuple: ("hello",'a')  string: "there"
s :: (Monoid s, IsString s, Show t) => Format s a (t -> a)
s = spliceWith (fromString . show)

-- | Select which formatter to apply based on an argument.
--
-- >>> printfLn (bind (\b -> if b then eFloat Nothing else fFloat Nothing)) False 5328
-- 5328.0
--
-- >>> printfLn (bind (\b -> if b then eFloat Nothing else fFloat Nothing)) True 5328
-- 5.328e3
--
-- >>> printfLn (bind (\f -> f Nothing)) eFloat 5328
-- 5.328e3
bind :: (t -> Format s a b) -> Format s a (t -> b)
bind h = Cokleisli (\k x -> runCokleisli (h x) k)

-- | Apply a function to the output of a formatter, possibly changing its type.
-- 
-- >>> printfLn (mapMonoid (map toUpper) hex) 0xcafe1234
-- CAFE1234
mapMonoid :: (m -> m') -> Format m a b -> Format m' a b
mapMonoid u f = Cokleisli (\k -> runCokleisli f (k . u))

-- | Generalizes the string type that a formatter uses by applying fromString internally.
generalizeString :: (IsString s, Monoid s) => Format String a b -> Format s a b
generalizeString = mapMonoid fromString

-- | Transform a numeric formatter which will be used to handle absolute values, applying the first given
-- function to the string to be spliced when the argument is negative, and the second given function otherwise.
--
-- >>> printfLn (signedWith (\v -> mconcat ["(",v,")"]) id (fFloat (Just 2))) (-pi)
-- (3.14)
--
-- >>> printfLn (signedWith ("-"<>) ("+"<>) (padLeft "0" 5 s)) (-439)
-- -00439
-- 
-- >>> printfLn (signedWith ("-"<>) ("+"<>) (padLeft "0" 5 s)) 1278
-- +01278
signedWith :: (Num t, Ord t, Monoid s) => (s -> s) -> (s -> s) -> Format s a (t -> b) -> Format s a (t -> b)
signedWith neg nonneg x = dup . bind (\n -> if n < 0 then mapMonoid neg (apply abs . x) else mapMonoid nonneg x)

-- | Show an integral value using the given base, and using the provided function to determine how
-- to display individual digits.
intAtBase :: (Real t, Integral t, Show t, Monoid s, IsString s)
          => t -> (Int -> Char) -> Format s a (t -> a)
intAtBase b showDigit = generalizeString $ spliceWith (\n -> showSigned (showIntAtBase b showDigit) 0 n "")

-- | Show an integral value in hexadecimal.
--
-- >>> printfLn (dup . s . " in hexadecimal is " . hex) 51966
-- 51966 in hexadecimal is cafe
hex :: (Integral t, Show t, Monoid s, IsString s) => Format s a (t -> a)
hex = generalizeString $ spliceWith (\n -> showSigned showHex 0 n "")

-- | Show an integral value in octal.
oct :: (Integral t, Show t, Monoid s, IsString s) => Format s a (t -> a)
oct = generalizeString $ spliceWith (\n -> showSigned showOct 0 n "")

-- | Show a floating point value in exponential format. (e.g. 2.45e2, -1.5e-3)
-- If `digs` is Nothing, the value is shown to full precision, if it is Just d then at most
-- d digits after the decimal point are shown.
eFloat :: (RealFloat t, Monoid s, IsString s) => Maybe Int -> Format s a (t -> a)
eFloat digs = generalizeString $ spliceWith (\n -> showEFloat digs n "")

-- | Show a floating point value in standard decimal format. (e.g. 245000, -0.0015)
-- If `digs` is Nothing, the value is shown to full precision, if it is Just d then at most
-- d digits after the decimal point are shown.
fFloat :: (RealFloat t, Monoid s, IsString s) => Maybe Int -> Format s a (t -> a)
fFloat digs = generalizeString $ spliceWith (\n -> showFFloat digs n "")

-- | Show a floating point value using standard decimal notation for arguments whose absolute
-- value lies between 0.1 and 9,999,999, and scientific notation otherwise. 
-- If `digs` is Nothing, the value is shown to full precision, if it is Just d then at most
-- d digits after the decimal point are shown.
gFloat :: (RealFloat t, Monoid s, IsString s) => Maybe Int -> Format s a (t -> a)
gFloat digs = generalizeString $ spliceWith (\n -> showGFloat digs n "")

-- | We can use `arr` from the Arrow instance for Cokleisli w to produce formatters
-- that manipulate the stack without printing. That is, we have
-- 
-- > arr :: (Monoid m) => (s -> s') -> Format m s s'

-- | Push an argument onto the stack to be consumed by subsequent formatters.
push :: Monoid m => t -> Format m (t -> a) a
push x = arr (\k -> k x)

-- | Duplicate an argument on the stack, making it available twice.
dup :: Monoid m => Format m (t -> t -> a) (t -> a)
dup = arr (\k x -> k x x)

-- | Swap the next two arguments on the stack.
swap :: Monoid m => Format m (t -> t' -> a) (t' -> t -> a)
swap = arr (\k x y -> k y x)

-- | Skip the next argument on the stack.
skip :: Monoid m => Format m a (t -> a)
skip = arr (\k x -> k)

-- | Apply a function to the argument on the top of the stack.
apply :: Monoid m => (u -> v) -> Format m (v -> a) (u -> a)
apply f = arr (\k x -> k (f x))

-- | Apply a binary function to the top two arguments on the stack.
--
-- >>> printfLn (dup . s . " plus " . swap . dup . s . " equals " . apply2 (+) . s) 4 6
-- 4 plus 6 equals 10
-- >>> printfLn (arr (\k x y -> k x y (x+y)) . s . " plus " . s . " equals " . s) 4 6
-- 4 plus 6 equals 10
apply2 :: Monoid m => (u -> v -> w) -> Format m (w -> a) (u -> v -> a)
apply2 f = arr (\k x y -> k (f x y))