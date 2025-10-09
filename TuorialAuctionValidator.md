# Tutorial: Auction Validator (Plutus V2)

> A guided, code‑aware walkthrough of the on‑chain validator that powers a simple single‑asset auction. You’ll learn the parameter/datum/redeemer model, the exact checks for bids and payout, and how the typed validator compiles to an untyped script.

---

## Table of Contents

1. [What this validator does](#1-what-this-validator-does)
2. [On-chain data model](#2-on-chain-data-model)

   * [AuctionParams](#21-auctionparams-static-parameters)
   * [Bid](#22-bid)
   * [AuctionDatum](#23-auctiondatum)
   * [AuctionRedeemer](#24-auctionredeemer)
3. [Validator shape (typed)](#3-validator-shape-typed)
4. [The checks in detail](#4-the-checks-in-detail)

   * [Sufficient bid](#41-sufficientbid)
   * [Valid bid time](#42-validbidtime)
   * [Refund previous highest bid](#43-refundsprevioushighestbid)
   * [Correct continuing output](#44-correctoutput-state-transition-on-newbid)
   * [Valid payout time](#45-validpayouttime)
   * [Seller gets highest bid](#46-sellergetshighestbid)
   * [Winner receives the asset](#47-highestbiddergetsasset)
5. [From typed to untyped & compilation](#5-from-typed-to-untyped--compilation)
6. [Worked scenarios](#6-worked-scenarios)
7. [Testing plan (suggested)](#7-testing-plan-suggested)
8. [Gotchas & best practices](#8-gotchas--best-practices)
9. [Minimal pseudo‑Haskell (decision tree)](#9-minimal-pseudo-haskell-decision-tree)
10. [Next steps](#10-next-steps)
11. [Glossary](#11-glossary)
12. [Appendix: Helper functions](#appendix-helper-functions-used)

---

## 1) What this validator does

The contract holds exactly **one NFT (or FT unit)** representing the auctioned asset plus some lovelace. While the auction is open, participants place bids. After the deadline, anyone can trigger **payout**:

* The **seller** receives the **highest bid** (if any).
* The **winner** (highest bidder) receives **exactly one** unit of the asset.
* If there are **no bids**, the asset returns to the **seller**.

---

## 2) On-chain data model

### 2.1 `AuctionParams` (static parameters)

* `apSeller :: PubKeyHash` — seller to pay on payout.
* `apCurrencySymbol :: CurrencySymbol` — asset’s currency symbol.
* `apTokenName :: TokenName` — asset’s token name.
* `apMinBid :: Lovelace` — minimum allowed first bid.
* `apEndTime :: POSIXTime` — bidding deadline (also earliest payout time).

**Why it matters:** These values never change during the contract lifetime; they’re compiled into the script or provided once when instantiating.

### 2.2 `Bid`

* `bPkh :: PubKeyHash` — bidder’s key hash.
* `bAmount :: Lovelace` — bid amount.

The code defines equality so that two bids are equal if **both** `bPkh` and `bAmount` match.

### 2.3 `AuctionDatum`

* `AuctionDatum { adHighestBid :: Maybe Bid }`

  * `Nothing` → no bids yet.
  * `Just bid` → current best bid (amount + bidder identity).

### 2.4 `AuctionRedeemer`

* `NewBid Bid` — attempt to set a new highest bid.
* `Payout` — settle the auction after the deadline.

---

## 3) Validator shape (typed)

```haskell
auctionTypedValidator :: AuctionParams -> AuctionDatum -> AuctionRedeemer -> ScriptContext -> Bool
```

At a high level, the validator builds a list of **guards** (booleans) depending on the redeemer, and requires **all** to be `True`:

* **`NewBid bid`** must satisfy:

  1. **`sufficientBid bid`** — bid is large enough.
  2. **`validBidTime`** — bid occurs **at or before** `apEndTime`.
  3. **`refundsPreviousHighestBid`** — prior highest bidder (if any) is refunded exactly.
  4. **`correctOutput bid`** — exactly one continuing output with the **updated datum** and the **correct value** (exact lovelace + exactly one unit of the asset).

* **`Payout`** must satisfy:

  1. **`validPayoutTime`** — payout occurs **at or after** `apEndTime`.
  2. **`sellerGetsHighestBid`** — seller receives exactly the highest bid (if any).
  3. **`highestBidderGetsAsset`** — winner (or seller if none) receives exactly **one** unit of the asset.

If any guard fails, the validator `traceError`s with a helpful message.

---

## 4) The checks in detail

### 4.1 `sufficientBid`

**Goal:** a new bid must strictly exceed the current highest amount; if there’s no bid yet, it must be **≥ `apMinBid`**.

**Why:** Prevents ties or non‑progressing bids; enforces the initial floor.

---

### 4.2 `validBidTime`

**Goal:** Bids are only valid **until** the end time.

**Mechanics:** The transaction’s validity range must be contained in `(-∞ .. apEndTime]`. In Plutus V2, the code checks this with `to apEndTime  
contains  txInfoValidRange` — meaning the range does not extend beyond the deadline.

**Common pitfall:** Using the wrong bound direction (`from` vs `to`) flips the meaning.

---

### 4.3 `refundsPreviousHighestBid`

**Goal:** When a new bid arrives and there was a previous highest bid, the previous bidder must be **fully refunded** in the same transaction.

**Mechanics:** The validator searches **all outputs** for one that:

* Pays exactly to the previous bidder’s `PubKeyHash`, and
* Contains **exactly** the previous bid amount in lovelace.

If not found → `traceError "Not found: refund output"`.

---

### 4.4 `correctOutput` (state transition on `NewBid`)

**Goal:** There must be **exactly one** continuing script output that:

1. Has an **inline datum** (`OutputDatum`) that decodes to `AuctionDatum (Just bid)` and equals the **expected** new bid.
2. Has a `Value` that contains:

   * `lovelace == bAmount bid` (the contract now escrows the new highest amount), and
   * `valueOf == 1` for the auctioned asset (`apCurrencySymbol`/`apTokenName`).

If there are zero or more than one continuing outputs → `traceError` with the actual count.

**Why inline datum?** The validator requires `OutputDatum` rather than `OutputDatumHash` to be explicit and decodable on‑chain.

---

### 4.5 `validPayoutTime`

**Goal:** Payout only after the deadline.

**Mechanics:** The tx validity range must be contained in `[apEndTime .. +∞)` — implemented via `from apEndTime  
contains  txInfoValidRange`.

---

### 4.6 `sellerGetsHighestBid`

**Goal:** On payout, the seller must receive **exactly** the highest bid amount (if one exists).

**Mechanics:** Search outputs for a payment to `apSeller` with `lovelace == bAmount highestBid`. If no bid exists, the check vacuously passes (there’s nothing to pay out).

---

### 4.7 `highestBidderGetsAsset`

**Goal:** Transfer exactly **one** unit of the auctioned asset to the winner (or back to seller if no bids).

**Mechanics:** Determine the recipient:

* If `Nothing` → recipient = `apSeller` (asset returns to seller).
* If `Just bid` → recipient = `bPkh bid`.

Then search outputs for a payment to that recipient with `valueOf == 1` for the asset.

---

## 5) From typed to untyped & compilation

Plutus validators are ultimately **untyped** (three `BuiltinData` arguments). This project:

* Implements a **typed** validator (with actual Haskell types) for safety and clarity.
* Wraps it in an **untyped** adapter using `unsafeFromBuiltinData`.
* Compiles/applies parameters with:

  * `PlutusTx.compile [|| auctionUntypedValidator ||]`
  * `liftCode` for `AuctionParams`
  * `unsafeApplyCode` to splice the parameters into the compiled code.

This yields `auctionValidatorScript :: AuctionParams -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> Unit)` that you can serialize for deployment.

---

## 6) Worked scenarios

1. **First bid succeeds**

   * Datum: `Nothing`.
   * Redeemer: `NewBid { bAmount = X }` with `X ≥ apMinBid`.
   * Tx range is before or at `apEndTime`.
   * Exactly one continuing output with inline datum `Just newBid` and value `(lovelace == X, asset == 1)`.

2. **Low bid rejected**

   * Datum: `Just bid'` with amount `Y`.
   * New bid `X ≤ Y` → `sufficientBid` fails.

3. **Late bid rejected**

   * Tx range strictly after `apEndTime` → `validBidTime` fails.

4. **Payout with winner**

   * Datum: `Just bid`.
   * Tx range after `apEndTime`.
   * One output pays seller exactly `bAmount bid` lovelace.
   * One output pays `bPkh bid` exactly one asset.

5. **Payout with no bids**

   * Datum: `Nothing`.
   * Asset returns to seller; no lovelace payment required.

---

## 7) Testing plan (suggested)

* **Unit tests** (emulator or off‑chain harness):

  * Accept first bid ≥ min.
  * Reject non‑increasing bid.
  * Enforce single continuing output on `NewBid`.
  * Enforce inline datum shape on `NewBid`.
  * Reject payout before end time.
  * Correct routing of funds/assets on payout (both with and without bids).

* **Property ideas:**

  * *Monotonicity:* Escrowed lovelace in script UTXO equals the recorded highest amount.
  * *Conservation:* Exactly one unit of the asset is always conserved between script and winner/seller.

---

## 8) Gotchas & best practices

* **Validity ranges:** remember `to apEndTime` (bidding) vs `from apEndTime` (payout).
* **Exactly one continuing output** during `NewBid`—helps preserve invariant and simplifies reasoning.
* **Inline datum required**: decoding failures intentionally `traceError`.
* **Exact amounts:** value checks are equality, not ≥. Don’t “overpay”/“underpay”.
* **Readable errors:** keep `traceError` strings meaningful; they show up in emulator logs.

---

## 9) Minimal pseudo‑Haskell (decision tree)

```haskell
case redeemer of
  NewBid bid -> and
    [ bidIsSufficient bid
    , within (-∞..end]
    , refundOldHighestIfAny
    , exactlyOneContinuingOutput
        && inlineDatumIs (Just bid)
        && outValue == (lovelace = bid.amount, asset = 1)
    ]

  Payout -> and
    [ within [end..+∞)
    , if highestBid then sellerPaidExactly(highest.amount) else True
    , recipient = highestBid ? highest.pkh : seller
      && paysExactly(recipient, asset = 1)
    ]
```

---

## 10) Next steps

* Write a matching **tutorial for the minting policy** (already present in your repo) and cross‑link both docs from the README.
* Add a **test scaffold** (`test/AuctionValidatorSpec.hs`) with at least one happy path and one failing case.
* (Optional) Add a minimal **GitHub Actions** workflow to build and run tests on PRs.

---

## 11) Glossary

**Auction** — A market process to sell one asset to the highest bidder by a deadline.

**AuctionParams** — Static configuration for the contract instance: seller, asset ID (currency symbol + token name), minimum bid, and end time.

**Datum** — On‑chain state attached to the script UTXO. Here, it records the current highest bid (`Maybe Bid`).

**Redeemer** — Action intent provided by the spending transaction. Here: `NewBid Bid` or `Payout`.

**Bid** — A pair of bidder identity (`PubKeyHash`) and amount (`Lovelace`).

**ScriptContext / TxInfo** — Read‑only transaction metadata visible to the validator (inputs, outputs, signatories, validity range, etc.).

**Validity range** — The POSIX time interval in which the transaction is valid. We check it against `to apEndTime` (bids) or `from apEndTime` (payout).

**Continuing output** — An output that sends funds back to the **same script address**, representing the next contract state.

**Inline datum** — Datum content stored directly in the output (as opposed to a datum hash). Required here so the validator can decode it on‑chain.

**Value / Lovelace** — Multi‑asset value carried by outputs. `lovelaceValueOf` extracts ADA amount; `valueOf` extracts quantity for a specific asset ID.

**CurrencySymbol / TokenName** — Pair that uniquely identifies a native asset on Cardano.

**Payout** — The settlement branch after the deadline: seller receives the highest bid; winner (or seller if no bids) receives the asset.

**Refund** — Returning the exact lovelace of the previous highest bid when a new higher bid arrives.

**Typed vs untyped validator** — A typed Haskell function `(params -> datum -> redeemer -> context -> Bool)` wrapped into an untyped `(BuiltinData -> BuiltinData -> BuiltinData -> ())` entrypoint for on‑chain execution.

**BuiltinData** — Plutus builtin type for serialized data passed to on‑chain scripts.

**`PlutusTx.compile` / `liftCode` / `unsafeApplyCode`** — Template‑Haskell helpers used to compile and parameterize the validator at compile time.

**`contains` / `from` / `to`** — Range operators used to check whether the transaction time range lies within a required interval.

---

### Appendix: Helper functions used

* `getContinuingOutputs` — collects outputs that continue the same script.
* `lovelaceValueOf` — extracts lovelace amount from a `Value`.
* `valueOf` — extracts an asset quantity by (CurrencySymbol, TokenName).
* `contains` with `to` / `from` — range checks for POSIX validity windows.
* `toPubKeyHash . txOutAddress` — decode a P2PKH recipient.

---

**End of tutorial.**
