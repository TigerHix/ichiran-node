export const NETWORK_INACTIVITY_TIMEOUT_MS = 30_000;

const MAX_JSON_BYTES = 64 * 1024;

async function cancelQuietly(
  reader: ReadableStreamDefaultReader<Uint8Array>,
  reason: unknown
): Promise<void> {
  try {
    await reader.cancel(reason);
  } catch {
    // The response may already have transitioned to errored or closed.
  }
}

export async function fetchBoundedJson(
  input: RequestInfo | URL,
  init: RequestInit,
  label: string,
  inactivityTimeoutMs = NETWORK_INACTIVITY_TIMEOUT_MS
): Promise<unknown> {
  const controller = new AbortController();
  let timedOut = false;
  let inactivityTimer: ReturnType<typeof setTimeout> | null = null;
  let reader: ReadableStreamDefaultReader<Uint8Array> | null = null;

  const stopTimer = (): void => {
    if (inactivityTimer !== null) clearTimeout(inactivityTimer);
    inactivityTimer = null;
  };
  const armTimer = (): void => {
    stopTimer();
    inactivityTimer = setTimeout(() => {
      timedOut = true;
      controller.abort();
    }, inactivityTimeoutMs);
  };

  try {
    armTimer();
    const response = await fetch(input, { ...init, signal: controller.signal });
    if (!response.ok) {
      await response.body?.cancel();
      throw new Error(`${label} failed with HTTP ${response.status}.`);
    }
    if (!response.headers.get('content-type')?.includes('application/json')) {
      await response.body?.cancel();
      throw new Error(`${label} is not JSON.`);
    }
    if (!response.body) throw new Error(`${label} has no response body.`);

    reader = response.body.getReader();
    const chunks: Uint8Array[] = [];
    let received = 0;
    while (true) {
      const { done, value } = await reader.read();
      if (done) {
        stopTimer();
        break;
      }
      armTimer();
      received += value.byteLength;
      if (received > MAX_JSON_BYTES) {
        throw new Error(`${label} exceeds ${MAX_JSON_BYTES} bytes.`);
      }
      chunks.push(value);
    }

    const bytes = new Uint8Array(received);
    let offset = 0;
    for (const chunk of chunks) {
      bytes.set(chunk, offset);
      offset += chunk.byteLength;
    }
    try {
      return JSON.parse(new TextDecoder().decode(bytes)) as unknown;
    } catch {
      throw new Error(`${label} contains invalid JSON.`);
    }
  } catch (error) {
    controller.abort();
    if (reader) await cancelQuietly(reader, error);
    if (timedOut) {
      throw new Error(
        `${label} received no data for ${inactivityTimeoutMs / 1_000} seconds. `
        + 'Check your connection and retry.'
      );
    }
    throw error;
  } finally {
    stopTimer();
  }
}
