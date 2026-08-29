# @ichiran/portable

Browser-safe primitives for the self-contained Ichiran analyzer. This package has
no Node.js, database, or runtime package dependencies.

The first implemented primitive is a deterministic binary container. Analyzer
data structures and algorithms are intentionally not part of this slice.

## Pack format version 1

All integers are unsigned and little-endian. Payloads are sorted by section ID
and aligned to 8-byte offsets.

### Header (32 bytes)

| Offset | Size | Value |
| ---: | ---: | --- |
| 0 | 8 | ASCII `ICHIPACK` |
| 8 | 2 | Format version (`1`) |
| 10 | 2 | Header size (`32`) |
| 12 | 4 | Reserved flags (`0`) |
| 16 | 4 | Section count |
| 20 | 4 | Directory size |
| 24 | 4 | Total pack size |
| 28 | 4 | Directory CRC-32 |

The directory starts immediately after the header and contains one 24-byte entry
per section.

### Section entry (24 bytes)

| Offset | Size | Value |
| ---: | ---: | --- |
| 0 | 4 | Non-zero section ID |
| 4 | 4 | Payload offset |
| 8 | 4 | Payload byte length |
| 12 | 4 | Payload CRC-32 |
| 16 | 4 | Reserved (`0`) |
| 20 | 4 | Reserved (`0`) |

Padding bytes must be zero. Readers reject unknown versions, non-canonical
directories, overlapping or trailing data, and checksum mismatches.

`PackReader#getSection` returns a zero-copy view into the source buffer. Treat
the source buffer and returned views as immutable.
