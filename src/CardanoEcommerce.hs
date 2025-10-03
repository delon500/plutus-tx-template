{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Cardano.Ecommerce
-- Minimal, dependency-light utilities for an e-commerce flow that prices in
-- Lovelace (Integer) but lets you work in ADA when convenient.
-- Pure where possible; small, explicit IO where unavoidable (order IDs).
module Cardano.Ecommerce
  ( -- * Money types & conversions
    Lovelace(..)
  , adaToLovelace
  , lovelaceToAda
  , clampNonNegative

    -- * Catalog & cart
  , Product(..)
  , CartLine(..)
  , Cart
  , emptyCart
  , addItem
  , updateQty
  , removeItem
  , cartItems
  , subtotal

    -- * Pricing helpers
  , applyPercentDiscount
  , applyFixedDiscount
  , computeTax
  , shippingFlat
  , grandTotal

    -- * Cardano-ish utilities
  , isBech32AddressLike
  , feeLinear
  , defaultFeeBase
  , defaultFeePerByte

    -- * Orders
  , newOrderId
  ) where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Char       (isAsciiLower, isDigit)
import           Data.Time.Clock.POSIX (getPOSIXTime)

--------------------------------------------------------------------------------
-- Money
--------------------------------------------------------------------------------

-- | Lovelace is the smallest ADA unit. 1 ADA = 1_000_000 Lovelace.
newtype Lovelace = Lovelace { getLovelace :: Integer }
  deriving (Eq, Ord, Num, Read, Show)

-- | Convert ADA (as a 'Rational') to 'Lovelace', rounded to the nearest unit.
--   Example: adaToLovelace (3 % 2) == 1.5 ADA == 1_500_000 Lovelace.
adaToLovelace :: Rational -> Lovelace
adaToLovelace ada = Lovelace (round (ada * 1000000))

-- | Convert Lovelace to ADA as a 'Rational'.
lovelaceToAda :: Lovelace -> Rational
lovelaceToAda (Lovelace ll) = fromInteger ll / 1000000

-- | Ensure a Lovelace value is not negative.
clampNonNegative :: Lovelace -> Lovelace
clampNonNegative x | x < 0     = 0
                   | otherwise = x

--------------------------------------------------------------------------------
-- Catalog & cart
--------------------------------------------------------------------------------

type ProductId = String

data Product = Product
  { productId   :: ProductId
  , productName :: String
  , unitPrice   :: Lovelace
  } deriving (Eq, Show)

data CartLine = CartLine
  { lineName     :: String
  , lineUnit     :: Lovelace
  , lineQuantity :: Integer   -- non-negative; we clamp in helpers
  } deriving (Eq, Show)

-- | Cart is a map keyed by 'ProductId'.
type Cart = Map ProductId CartLine

emptyCart :: Cart
emptyCart = M.empty

-- | Add (or increase) an item by quantity >= 0.
addItem :: Product -> Integer -> Cart -> Cart
addItem Product{productId,p roductName=nm,unitPrice=ppu} q0 =
  let q = max 0 q0
  in M.insertWith
       (\new old -> old { lineQuantity = lineQuantity old + lineQuantity new })
       productId
       CartLine{ lineName = nm, lineUnit = ppu, lineQuantity = q }

-- | Set quantity (clamped >= 0). If quantity becomes 0, the line is removed.
updateQty :: ProductId -> Integer -> Cart -> Cart
updateQty pid q cart =
  let q' = max 0 q
  in if q' == 0
       then M.delete pid cart
       else M.adjust (\l -> l { lineQuantity = q' }) pid cart

-- | Remove a line entirely.
removeItem :: ProductId -> Cart -> Cart
removeItem = M.delete

-- | Extract cart lines as a list.
cartItems :: Cart -> [CartLine]
cartItems = M.elems

-- | Sum of (unit * quantity) across lines.
subtotal :: Cart -> Lovelace
subtotal =
  foldr (\CartLine{lineUnit,lineQuantity} acc ->
           acc + lineUnit * fromInteger (max 0 lineQuantity))
        0
  . cartItems

--------------------------------------------------------------------------------
-- Pricing helpers (order of operations is up to your app)
--------------------------------------------------------------------------------

-- | Apply a % discount (e.g. 0.10 == 10%). Result is clamped >= 0.
applyPercentDiscount :: Rational -> Lovelace -> Lovelace
applyPercentDiscount pct base =
  clampNonNegative $ Lovelace (round (fromIntegral (getLovelace base) * (1 - pct)))

-- | Subtract a fixed Lovelace discount. Clamped >= 0.
applyFixedDiscount :: Lovelace -> Lovelace -> Lovelace
applyFixedDiscount d base = clampNonNegative (base - d)

-- | Compute tax as a % of a base amount (e.g., 0.15 == 15% VAT).
computeTax :: Rational -> Lovelace -> Lovelace
computeTax rate base =
  Lovelace (round (fromIntegral (getLovelace base) * rate))

-- | Flat shipping fee (could be 0).
shippingFlat :: Lovelace -> Lovelace
shippingFlat = id

-- | Combine pieces into a final payable total.
--   Typically: total = clamp( (subtotal - discounts) + tax + shipping )
grandTotal
  :: Lovelace -- ^ discounted subtotal (or raw subtotal if you apply discounts here)
  -> Lovelace -- ^ tax
  -> Lovelace -- ^ shipping
  -> Lovelace
grandTotal s t sh = clampNonNegative (s + t + sh)

--------------------------------------------------------------------------------
-- Cardano-ish utilities
--------------------------------------------------------------------------------

-- | Extremely lightweight check for a Cardano Bech32 address shape.
--   This does NOT fully validate Bech32; it only catches obvious mistakes.
--   Accepts mainnet "addr1..." and testnet "addr_test1...".
isBech32AddressLike :: String -> Bool
isBech32AddressLike s =
  let prefixOk =
        take 5 s == "addr1" || take 10 s == "addr_test1"
      lenOk = length s >= 20 && length s <= 120
      charsetOk = all (\c -> isAsciiLower c || isDigit c || c == '_') s
   in prefixOk && lenOk && charsetOk

-- | A simple linear fee model:
--     fee = base + ceil(perByte * bytes)
--   Provide 'base' and 'perByte' in Lovelace; bytes is the estimated tx size.
feeLinear
  :: Lovelace -- ^ base fee in lovelace
  -> Rational -- ^ per-byte fee in lovelace (can be fractional)
  -> Integer  -- ^ estimated transaction size in bytes
  -> Lovelace
feeLinear (Lovelace base) perByte bytes =
  Lovelace $ base + ceiling (perByte * fromInteger bytes)

-- | Placeholder defaults (tune to your environment/tooling).
defaultFeeBase :: Lovelace
defaultFeeBase = 200_000  -- purely illustrative

defaultFeePerByte :: Rational
defaultFeePerByte = 44    -- purely illustrative (lovelace per byte)

--------------------------------------------------------------------------------
-- Orders
--------------------------------------------------------------------------------

-- | Create a timestamp-based order ID. For production, consider
--   adding entropy (e.g., a random suffix or hash).
newOrderId :: IO String
newOrderId = do
  t <- getPOSIXTime
  pure $ "order-" ++ show (floor (t * 1000) :: Integer)

--------------------------------------------------------------------------------
-- Notes / Example (doctest-style)
--------------------------------------------------------------------------------
{- $setup
>>> let p1 = Product "sku-ada-tshirt" "ADA T-Shirt" (Lovelace 2500000)   -- 2.5 ADA
>>> let p2 = Product "sku-hoodie"     "Hoodie"     (Lovelace 6000000)   -- 6 ADA
>>> let cart0 = emptyCart
>>> let cart1 = addItem p1 2 cart0
>>> let cart2 = addItem p2 1 cart1
-}

{- |
Subtotal:

>>> subtotal cart2
11000000

10% promo discount on subtotal:

>>> let s = subtotal cart2
>>> let discounted = applyPercentDiscount (10 % 100) s
>>> discounted
9900000

15% VAT on discounted:

>>> let vat = computeTax (15 % 100) discounted
>>> vat
1485000

Flat shipping 0.8 ADA:

>>> let ship = Lovelace 800000
>>> grandTotal discounted vat ship
12185000

Helpers:

>>> adaToLovelace (3 % 1)
3000000
>>> lovelaceToAda (Lovelace 1500000)
3 % 2

Bech32-ish check:

>>> isBech32AddressLike "addr1qxyz0123456789abcdefghijklmnopqrstuv"
True
-}
