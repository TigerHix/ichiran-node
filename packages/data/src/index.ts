/** PostgreSQL-free source compiler used by the canonical data package entry point. */
export { runSourceCompilerRelease } from './source-compiler/release.js';
export {
  assertSourceCompilerReleaseMode,
  verifySourceCompilerLock,
  type VerifiedSourceCompilerLock
} from './source-compiler/source-lock.js';
