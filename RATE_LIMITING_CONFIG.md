# Rate Limiting Configuration Guide

## Current Issue
The system is hitting Groq API rate limits when processing multiple chunks in parallel. The error shows:
```
Rate limit reached for model `llama3-70b-8192` in organization `org_01j67phdt8ewgtw7hs3a68kew9` service tier `on_demand` on tokens per minute (TPM): Limit 30000, Used 28939, Requested 1281.
```

## Solution: Environment Configuration

Add these settings to your `.env` file to optimize for rate limiting:

```bash
# Groq Model Configuration
GROQ_MODEL=openai/gpt-oss-120b
GROQ_TEMPERATURE=0.1
GROQ_MAX_TOKENS=50000

# Rate Limiting Configuration
MAX_CONCURRENT_CHUNKS=2
DELAY_BETWEEN_CHUNKS=1.0
```

## Configuration Options

### Model Selection
- **`GROQ_MODEL`**: Choose from available Groq models
  - `openai/gpt-oss-120b` (current - higher rate limits)
  - `llama3-70b-8192` (default - lower rate limits)
  - `mixtral-8x7b-32768` (alternative option)

### Rate Limiting Settings
- **`MAX_CONCURRENT_CHUNKS`**: Number of chunks processed simultaneously
  - `1`: Sequential processing (slowest, safest)
  - `2`: Conservative parallel (recommended)
  - `3`: Aggressive parallel (may hit limits)

- **`DELAY_BETWEEN_CHUNKS`**: Delay in seconds between chunk processing
  - `0.5`: Minimal delay
  - `1.0`: Moderate delay (recommended)
  - `2.0`: Conservative delay

## Recommended Settings for Different Scenarios

### For Large Files (Many Chunks)
```bash
MAX_CONCURRENT_CHUNKS=1
DELAY_BETWEEN_CHUNKS=2.0
```

### For Medium Files (5-10 Chunks)
```bash
MAX_CONCURRENT_CHUNKS=2
DELAY_BETWEEN_CHUNKS=1.0
```

### For Small Files (1-3 Chunks)
```bash
MAX_CONCURRENT_CHUNKS=2
DELAY_BETWEEN_CHUNKS=0.5
```

## Monitoring

The system now shows:
- 🤖 Which model is being used
- 🌡️ Temperature setting
- 🔒 Rate limiting configuration
- ⚡ Parallel processing status

## Error Handling

The system now handles rate limit errors gracefully:
- Failed chunks get placeholder results
- Conversion continues with successful chunks
- Clear logging of which chunks failed and why
