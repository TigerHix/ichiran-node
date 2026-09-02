import type { CanonicalEntry } from './model.js';

/** Owns the final canonical-root set while chronological source edits run. */
export class CanonicalLexicon {
  readonly #roots = new Map<number, CanonicalEntry>();

  constructor(entries: Iterable<CanonicalEntry> = []) {
    for (const entry of entries) this.add(entry);
  }

  add(entry: CanonicalEntry): void {
    if (this.#roots.has(entry.seq)) throw new RangeError(`Duplicate canonical root ${entry.seq}`);
    this.#roots.set(entry.seq, entry);
  }

  demoteRoot(seq: number): CanonicalEntry {
    const entry = this.#roots.get(seq);
    if (!entry) throw new RangeError(`Cannot demote missing canonical root ${seq}`);
    this.#roots.delete(seq);
    return entry;
  }

  entries(): CanonicalEntry[] {
    return [...this.#roots.values()].sort((left, right) => left.seq - right.seq);
  }
}
