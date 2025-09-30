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
