import react from '@vitejs/plugin-react';
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { gzipSync } from 'node:zlib';
import { defineConfig, type Plugin } from 'vite';

const OPAQUE_ANALYZER_ASSETS = new Map([
  ['/analyzer/hot.bin.gz', 'hot.bin.gz'],
  ['/analyzer/details.bin.gz', 'details.bin.gz']
]);

function serveOpaqueAnalyzerAssets(): Plugin {
  return {
    name: 'serve-opaque-analyzer-assets',
    configurePreviewServer(server) {
      server.middlewares.use((request, response, next) => {
        if (request.method !== 'GET' && request.method !== 'HEAD') return next();
        const url = new URL(request.url ?? '/', 'http://localhost');
        const pathname = url.pathname;
        const filename = OPAQUE_ANALYZER_ASSETS.get(pathname);
        if (!filename) return next();
        const path = resolve(server.config.root, server.config.build.outDir, 'analyzer', filename);
        void readFile(path).then(bytes => {
          response.statusCode = 200;
          response.setHeader('Content-Length', bytes.byteLength);
          response.setHeader('Content-Type', 'application/gzip');
          if (
            request.method === 'GET'
            && url.searchParams.get('__ichiran_e2e_partial') === '1'
            && bytes.byteLength > 1
          ) {
            // Playwright uses this preview-only hook to prove interruption
            // after body bytes arrive. Keep the declared full length and leave
            // the response open until closing the page cancels the request.
            response.flushHeaders();
            response.write(bytes.subarray(0, Math.min(64 * 1024, bytes.byteLength - 1)));
            const timeout = setTimeout(() => response.destroy(), 120_000);
            timeout.unref();
            response.once('close', () => clearTimeout(timeout));
          } else {
            response.end(request.method === 'HEAD' ? undefined : bytes);
          }
        }, (error: NodeJS.ErrnoException) => {
          if (error.code === 'ENOENT') next();
          else next(error);
        });
      });
    }
  };
}

function gzipRustKernelWasm(): Plugin {
  return {
    name: 'gzip-rust-kernel-wasm',
    enforce: 'post',
    generateBundle(_options, bundle) {
      const matches = Object.entries(bundle).filter(([, output]) =>
        output.type === 'asset'
        && output.fileName.startsWith('assets/ichiran_kernel_bg-')
        && output.fileName.endsWith('.wasm')
      );
      if (matches.length === 0 && process.env.ICHIRAN_TYPESCRIPT_ORACLE === '1') return;
      if (matches.length !== 1) {
        throw new Error(`Expected one Rust kernel WASM asset, found ${matches.length}`);
      }
      const [key, asset] = matches[0]!;
      if (asset.type !== 'asset') throw new Error('Rust kernel WASM output is not an asset');
      // Keep `.bin` last so static hosts do not advertise Content-Encoding and
      // transparently expand bytes that the Worker owns decompressing.
      const fileName = `${asset.fileName}.gz.bin`;
      delete bundle[key];
      bundle[fileName] = {
        ...asset,
        fileName,
        source: gzipSync(asset.source, { level: 9 })
      };
    }
  };
}

export default defineConfig({
  define: {
    __ICHIRAN_TYPESCRIPT_ORACLE__: JSON.stringify(
      process.env.ICHIRAN_TYPESCRIPT_ORACLE === '1'
    )
  },
  plugins: [react(), serveOpaqueAnalyzerAssets(), gzipRustKernelWasm()],
  worker: {
    format: 'es'
  }
});
