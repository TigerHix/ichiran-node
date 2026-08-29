import { describe, expect, test } from 'bun:test';
import { Sha256 } from '../src/worker/sha256.js';

const encoder = new TextEncoder();

describe('streaming SHA-256', () => {
  test.each([
    ['', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'],
    ['abc', 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'],
    [
      'The quick brown fox jumps over the lazy dog',
      'd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592'
    ]
  ])('hashes %j', (input, expected) => {
    expect(new Sha256().update(encoder.encode(input)).digestHex()).toBe(expected);
  });

  test('is independent of chunk boundaries', () => {
    const bytes = new Uint8Array(10_000);
    for (let index = 0; index < bytes.length; index++) bytes[index] = index & 0xff;
    const whole = new Sha256().update(bytes).digestHex();
    const chunked = new Sha256();
    for (let offset = 0; offset < bytes.length; offset += 37) {
      chunked.update(bytes.subarray(offset, offset + 37));
    }
    expect(chunked.digestHex()).toBe(whole);
    expect(chunked.digestHex()).toBe(whole);
  });
});
