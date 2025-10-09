# Ichiran API Documentation

Japanese text segmentation and romanization API.

## Base URL

Local: `http://localhost:3000`
Deployed: `https://ichiran-node.your-subdomain.workers.dev`

## Endpoints

### Health Check

**GET** `/health`

Returns server status.

```bash
curl http://localhost:3000/health
```

Response:
```json
{
  "status": "ok",
  "timestamp": "2025-10-03T03:25:40.314Z"
}
```

---

### API Documentation

**GET** `/api`

Returns available endpoints and examples.

---

### Basic Romanization

**POST** `/api/romanize`

Converts Japanese text to romaji.

Request:
```json
{
  "text": "こんにちは"
}
```

Response:
```json
{
  "text": "こんにちは",
  "romanized": "konnichiwa"
}
```

Example:
```bash
curl -X POST http://localhost:3000/api/romanize \
  -H "Content-Type: application/json" \
  -d '{"text":"こんにちは"}'
```

---

### Romanization with Dictionary Info

**POST** `/api/romanize/info`

Returns romanization with word definitions and glosses.

Request:
```json
{
  "text": "今日は良い天気です"
}
```

Response:
```json
{
  "text": "今日は良い天気です",
  "romanized": "kyō wa yoitenki desu",
  "info": [
    ["desu", "です\n1. [aux-v,cop] be; is\n[ Conjugation: [cop] Non-past Affirmative Formal\n  だ :  ]"],
    ["yoitenki", "良い天気 【よいてんき】\n1. [n,exp] fine weather; fair weather"],
    ["wa", "は\n1. [prt] 《pronounced わ in modern Japanese》 indicates sentence topic"],
    ["kyō", "今日 【きょう】\n1. [n,adv] today; this day"]
  ]
}
```

Example:
```bash
curl -X POST http://localhost:3000/api/romanize/info \
  -H "Content-Type: application/json" \
  -d '{"text":"今日は良い天気です"}'
```

---

### Full Segmentation

**POST** `/api/segment`

Returns complete segmentation with alternatives, scores, and detailed word information.

Request:
```json
{
  "text": "ご注文はうさぎですか",
  "limit": 2
}
```

Parameters:
- `text` (string, required): Japanese text to segment
- `limit` (number, optional): Number of alternative segmentations to return (default: 1)

Response:
```json
{
  "text": "ご注文はうさぎですか",
  "segments": [
    [
      [
        [
          ["gochūmon", {...}, []],
          ["wa", {...}, []],
          ["usagi", {...}, []],
          ["desu", {...}, []],
          ["ka", {...}, []]
        ],
        518
      ]
    ]
  ],
  "limit": 2
}
```

Each word entry contains:
- Romanization
- Dictionary entry (reading, text, kana, score, seq, gloss, conjugation)
- Additional properties

Example:
```bash
curl -X POST http://localhost:3000/api/segment \
  -H "Content-Type: application/json" \
  -d '{"text":"ご注文はうさぎですか","limit":2}'
```

---

### Combined Grammar Analysis and Segmentation

**POST** `/api/analyze`

Performs both grammar pattern matching and text segmentation in a single efficient request. This endpoint combines the functionality of grammar analysis with tokenization, making only one database round-trip instead of two separate calls.

Request:
```json
{
  "text": "私は学生です",
  "limit": 5,
  "maxMatches": 10
}
```

Parameters:
- `text` (string, required): Japanese text to analyze
- `limit` (number, optional): Number of alternative segmentations to consider internally for better accuracy (default: 5)
- `maxMatches` (number, optional): Maximum number of grammar matches to return (default: unlimited)

**Note**: The `limit` parameter is used internally to test multiple segmentation alternatives and select the one that produces the best grammar matches. Only the best segmentation is returned in the response.

Response:
```json
{
  "segments": [
    [
      [
        [
          ["watashi", {...}, []],
          ["wa", {...}, []],
          ["gakusei", {...}, []],
          ["desu", {...}, []]
        ],
        480
      ]
    ]
  ],
  "grammars": {
    "n5.noun-wa": {
      "matchedSentences": [
        {
          "level": "n5",
          "description": "Used to mark the topic of a sentence",
          "captures": [
            {
              "label": "topic",
              "start": 0,
              "end": 1,
              "tokens": [...]
            },
            {
              "label": "predicate",
              "start": 2,
              "end": 4,
              "tokens": [...]
            }
          ],
          "segments": [
            {
              "type": "capture",
              "text": "私",
              "label": "topic"
            },
            {
              "type": "raw",
              "text": "は"
            },
            {
              "type": "capture",
              "text": "学生です",
              "label": "predicate"
            }
          ]
        }
      ],
      "grammarDetail": {
        "label": "話題の「は」",
        "formation": "Noun + は",
        "description": "Used to mark the topic of a sentence; 'as for', 'speaking of'",
        "explanation": "### Name\n- 話題の「は」\n\n### Meaning and function\n...",
        "examples": [
          {
            "jp": "今日は暑いですね。",
            "en": "As for today, it's hot, isn't it?"
          },
          {
            "jp": "この店は安くておいしい。",
            "en": "This shop is cheap and tasty."
          }
        ]
      }
    }
  }
}
```

The response includes:
- `segments`: Best segmentation result (same format as `/api/segment`, but only the single best alternative for each segment)
- `grammars`: Object keyed by grammarId, where each grammar contains:
  - `matchedSentences`: Array of all instances where this grammar pattern was matched
    - `level`: JLPT level (n1-n5)
    - `description`: Human-readable description of the pattern
    - `captures`: Named groups of tokens that match parts of the pattern (for internal use)
    - `segments`: **Easy-to-render structure** alternating between `raw` and `capture` segments
      - Each segment has `type` ('raw' or 'capture') and `text`
      - Capture segments also include `label` field
      - Segments can be directly mapped to UI elements (e.g., normal text vs highlighted text)
  - `grammarDetail`: Detailed information about the grammar pattern
    - `label`: Human-readable label of the pattern (e.g., "話題の「は」")
    - `formation`: Structure/template of the pattern (e.g., "Noun + は")
    - `description`: Brief description of the pattern's function
    - `explanation`: Comprehensive explanation including usage notes, constraints, and common mistakes
    - `examples`: Array of example sentences with Japanese text and English translations

Example:
```bash
curl -X POST http://localhost:3000/api/analyze \
  -H "Content-Type: application/json" \
  -d '{"text":"私は学生です","limit":5}'
```

---

## Error Responses

All endpoints return errors in this format:

```json
{
  "error": "Error message description"
}
```

Common status codes:
- `400` - Bad request (missing/invalid parameters)
- `500` - Internal server error

---

## CORS

All endpoints include CORS headers:
- `Access-Control-Allow-Origin: *`
- `Access-Control-Allow-Methods: GET, POST, OPTIONS`
- `Access-Control-Allow-Headers: Content-Type`
