#!/usr/bin/env bun

import { runSourceCompilerRelease } from './release.js';

await runSourceCompilerRelease(process.argv.slice(2));
