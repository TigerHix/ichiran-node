# Root Cause Analysis: Why "エロそうだ" Fails in Test DB

## The Smoking Gun

**Production Query #91 params:**
```
[1030660, 1006610, 2029730, ...]
```

**Test Query #91 params:**
```
[17970892, 1030660, 1006610, ...]  
```
         ^^^^^^^^^ EXTRA ENTRY!

## What is Entry 17970892?

- Created by conjugating エロい (2025570)
- Readings: ["エロ", "えろ"]  
- **This is a DUPLICATE of the functionality that entry 1030660 should provide**

## What is Entry 1030660?

- Root entry from JMdict
- Readings: ["エロ"] only
- POS: adj-na, n, n-pref
- In production: **HAS conjugation link** from エロい (2025570) → conj_id 16251
- In test: **NO conjugation link** (link went to 17970892 instead)

## The Actual Problem

In TEST DB, when conjugating エロい (2025570):
1. The conjugation system generates form with readings ["エロ", "えろ"]
2. It computes signature: `computeReadingSignature([], ["エロ", "えろ"])` = 873e3bd...
3. It looks for this signature in `entry_reading_sig` table
4. Does NOT find entry 1030660 (which has signature for ["エロ"] only = 911d62d...)
5. Creates NEW entry 17970892 with both readings
6. Links conjugation to 17970892

In PRODUCTION DB:
1. Same process, BUT entry 1030660 somehow gets the conjugation link
2. No duplicate entry created
3. Everything works

## The Root Bug

**The original conjugation process did NOT populate `entry_reading_sig` for root entries!**

This means when conjugating:
- If conjugated form exactly matches root entry readings → dedupe works
- If conjugated form has EXTRA readings (like hiragana variant) → creates duplicate

## Why Production Works

Production DB was loaded with OLDER code that:
- Either had different deduplication logic OR
- The JMdict used didn't have the hiragana variant "えろい" yet

## My Fix

Added code to populate `entry_reading_sig` when loading root entries.

## Why Tests Still Fail

Even with the fix, the test DB ALREADY has:
1. The duplicate entry 17970892
2. The wrong conjugation links
3. Entry 1030660 WITHOUT the conjugation link it needs

The fix prevents FUTURE duplicates but doesn't repair existing data.

## Solution

Need to:
1. Delete all conjugated entries
2. Repopulate `entry_reading_sig` for all root entries (DONE)
3. Re-run conjugations (takes 30+ min)

OR

Accept that 2 tests fail due to JMdict version differences in readings.

---

# Second Failure: "出しなに客が来る"

## Expected vs Actual

Production: `出しな, に, 客, が, 来る` ✓  
Test: `出し, なに, 客, が, 来る` ✗

## Investigation Needed

Let me check if there are similar differences in query results...
