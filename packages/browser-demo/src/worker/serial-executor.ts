export type SerialExecutor = <T>(operation: () => T | Promise<T>) => Promise<T>;

/** Runs Worker requests in arrival order and remains usable after a rejected request. */
export function createSerialExecutor(): SerialExecutor {
  let tail: Promise<void> = Promise.resolve();
  return <T>(operation: () => T | Promise<T>): Promise<T> => {
    const result = tail.then(operation);
    tail = result.then(() => undefined, () => undefined);
    return result;
  };
}
