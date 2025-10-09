# Tutorial: AuctionValidatorSpec (Plutus V2 Test Suite)

This guide explains how the `test/AuctionValidatorSpec.hs` suite is structured, how the **mock ScriptContext** works, and how to extend the tests to cover success and failure scenarios for the auction validator.

---

## Table of Contents

1. [What this test file is doing](#1-what-this-test-file-is-doing)
2. [Anatomy of the file](#2-anatomy-of-the-file)
3. [Why the sample test fails (by design)](#3-why-the-sample-test-fails-by-design)
4. [Running the tests](#4-running-the-tests)
5. [Extending the suite — practical scenarios](#5-extending-the-suite--practical-scenarios)

   * [First bid succeeds (happy path)](#51-first-bid-succeeds-happy-path)
   * [Non-increasing bid is rejected](#52-non-increasing-bid-is-rejected)
   * [Late bid is rejected](#53-late-bid-is-rejected)
   * [Payout before end time is rejected](#54-payout-before-end-time-is-rejected)
   * [Payout routes funds and asset correctly](#55-payout-routes-funds-and-asset-correctly)
   * [No bids → asset returns to seller on payout](#56-no-bids--asset-returns-to-seller-on-payout)
6. [Building TxInfo and outputs in tests](#6-building-txinfo-and-outputs-in-tests)
7. [Making assertions with Hspec](#7-making-assertions-with-hspec)
8. [Organizing your tests](#8-organizing-your-tests)
9. [Troubleshooting](#9-troubleshooting)
10. [Glossary](#10-glossary)
11. [Next steps](#11-next-steps)

---

## 1) What this test file is doing

* Uses **Hspec** to define behavior‑oriented tests for `auctionTypedValidator`.
* Builds a minimal **`ScriptContext`** (via `mockScriptContext`) to run the validator **purely**.
* Demonstrates a negative case: a `NewBid` is **rejected** when **no outputs** are present (i.e., the required continuing output isn’t found).

---

## 2) Anatomy of the file

* **Imports**

  * `Test.Hspec` — the testing framework for Haskell.
  * `AuctionValidator` — brings in `AuctionParams`, `Bid`, `AuctionDatum`, `AuctionRedeemer`, and `auctionTypedValidator`.
  * Plutus types for constructing the minimal `ScriptContext`/`TxInfo` (e.g., `Lovelace`, `CurrencySymbol`, `TokenName`, `ScriptContext`, `TxInfo`, `ScriptPurpose`).

* **`mockScriptContext`**

  * Provides an **empty** `TxInfo` (no inputs/outputs/mint/signatories; validity range = `always`).
  * `scriptContextPurpose = Spending <dummy utxo>` — matches how validators are invoked in spending transactions.
  * Intent: a **baseline** context you can copy and then **enrich** per test case.

* **`main` / spec**

  * `describe "auctionTypedValidator"` groups tests about the validator.
  * Example test: *“rejects a new bid when the context has no outputs”* asserts `False`.

---

## 3) Why the sample test fails (by design)

For a `NewBid` to pass, the validator typically requires (among other checks):

1. A **continuing script output** (exactly one),
2. Correct **inline datum** (`AuctionDatum (Just newBid)`), and
3. Correct **value** (lovelace == new bid amount + exactly one auctioned asset).

Because `mockScriptContext` contains **no outputs**, the validator cannot find the required continuing output and **returns `False`**.

---

## 4) Running the tests

From the project root (where cabal/stack is configured):

```bash
# Cabal
cabal test

# Or, Stack
stack test
```

If you want to run only this spec with cabal’s test framework, make sure the test suite’s `main-is` points to `AuctionValidatorSpec.hs` and then use your test runner’s filtering options (e.g., `--test-option=--match "auctionTypedValidator"`).

---

## 5) Extending the suite — practical scenarios

Below are practical test patterns you can add. Each requires constructing a suitable `ScriptContext` and, when needed, adjusting `AuctionDatum`/`AuctionParams`/`AuctionRedeemer`.

> Tip: Start from `mockScriptContext` and then replace specific fields inside `scriptContextTxInfo` (e.g., outputs, validity range) to model each scenario.

### 5.1 First bid succeeds (happy path)

**Given** no previous bid; **when** `NewBid` amount ≥ `apMinBid`, **then** validator is `True`.

Key steps:

* Datum: `AuctionDatum Nothing`.
* Redeemer: `NewBid newBid` with `bAmount >= apMinBid`.
* Validity: range within `(-∞ .. apEndTime]`.
* Outputs: include **exactly one** continuing script output with:

  * Inline datum `AuctionDatum (Just newBid)`.
  * Value containing `lovelace == bAmount` and the auctioned asset quantity == 1.

### 5.2 Non‑increasing bid is rejected

**Given** existing highest bid `Y`; **when** `NewBid X` with `X ≤ Y`, **then** `False`.

Key steps:

* Datum: `AuctionDatum (Just oldBid)`.
* Redeemer: `NewBid newBid` where `bAmount newBid <= bAmount oldBid`.
* Outputs: can be omitted or even provided; the `sufficientBid` guard should fail.

### 5.3 Late bid is rejected

**Given** `NewBid` after the deadline; **then** `False`.

Key steps:

* Validity range **after** `apEndTime` (e.g., `from apEndTime + 1`).

### 5.4 Payout before end time is rejected

**Given** `Payout` but validity is before `apEndTime`; **then** `False`.

Key steps:

* Redeemer: `Payout`.
* Validity: `to (apEndTime - 1)`.

### 5.5 Payout routes funds and asset correctly

**Given** a highest bid `B` and winner `W`; **when** `Payout` after deadline; **then** seller receives `B` lovelace and `W` receives exactly one unit of the asset.

Key steps:

* Outputs: one to seller with `lovelace == B`; one to `W` with asset quantity == 1.

### 5.6 No bids → asset returns to seller on payout

**Given** `AuctionDatum Nothing`; **when** `Payout` after deadline; **then** asset to seller, no lovelace transfer required.

---

## 6) Building `TxInfo` and outputs in tests

Constructing realistic `TxInfo` is the “heavy lift.” Common tools/patterns:

* **`TxOut` to a P2PKH address**: build an address for a `PubKeyHash` and set `txOutValue` with `lovelace`/`valueOf` composition.
* **Continuing output**: ensure the output’s address is the *same script address* and attach an **inline datum** (not a datum hash) encoding `AuctionDatum`.
* **Validity**: use `from` / `to` with `contains` semantics in your validator; in tests, set `txInfoValidRange` accordingly.

> If your project already exposes helpers to produce script addresses and inline datums for tests, reuse them to avoid boilerplate.

---

## 7) Making assertions with Hspec

* **Direct boolean**: `auctionTypedValidator params datum redeemer ctx
  	\`tshouldBe True`
* **Multiple conditions**: prefer multiple focused `it` blocks over one giant case.
* **Descriptive names**: make failing output obvious (include the reason in the `it` description).

---

## 8) Organizing your tests

* Group by **redeemer branch**:

  * `describe "NewBid"` → bid progression, refunds, continuing output shape, valid time.
  * `describe "Payout"` → time window, seller gets funds, winner gets asset, no‑bid case.
* Start with **small happy paths**; add failure edges one by one.
* Keep fixtures (e.g., params, seller pkh, asset IDs) at the top or in a separate `Test.Helpers` module.

---

## 9) Troubleshooting

* **Decoding datum fails**: ensure your continuing output uses an **inline** datum, and the encoding matches the on‑chain type.
* **Wrong address**: if the continuing output isn’t credited to the same script address, the validator won’t count it as “continuing.”
* **Off‑by‑one time**: confirm whether your validator uses inclusive `to end` vs inclusive `from end`; match the exact boundary.
* **Value mismatches**: the validator often checks **equality** (not ≥). Provide the exact lovelace and asset quantities expected.

---

## 10) Glossary

**AuctionParams** — Static configuration passed to the validator under test (seller, asset ID, min bid, end time). Tests often reuse a fixed instance across cases.

**Bid** — Test record capturing bidder identity (`PubKeyHash`) and amount (`Lovelace`). Used in datums and redeemers.

**AuctionDatum** — On-chain state for the script UTXO under test (`Nothing` for no bids, `Just Bid` for a highest bid). Tests encode this into an **inline datum** for continuing outputs.

**AuctionRedeemer** — Action being exercised in a test: `NewBid Bid` or `Payout`.

**ScriptContext** — The environment a validator sees. In tests, you construct this manually (inputs, outputs, validity range, purpose).

**TxInfo** — Part of the context containing inputs/outputs, mint, signatories, fees, and **`txInfoValidRange`**. Tests tweak these fields per scenario.

**Spending purpose** — `scriptContextPurpose = Spending <txOutRef>` indicates the validator is validating a spending transaction of the script UTXO.

**Continuing output** — An output that pays back to **the same script address**, representing the next state; required for `NewBid`.

**Inline datum** — Datum stored directly in the output (not a hash), enabling on-chain decoding during tests.

**Validity range** — POSIX interval a tx is valid in. Tests must set ranges that satisfy `to apEndTime` (bids) or `from apEndTime` (payout).

**Value checks** — Exact equality checks for lovelace and asset quantities; tests must construct outputs with precise values.

---

## 11) Next steps

* Add a **property test**: “escrowed lovelace equals the recorded highest bid amount.”
* Build small constructors/helpers for **outputs with inline datums** to avoid repetition.
* Add CI to run tests on every PR (Hspec output is friendly for reviewers).

---

**End of tutorial.**
