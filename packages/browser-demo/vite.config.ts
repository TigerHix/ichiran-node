import react from '@vitejs/plugin-react';
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
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

export default defineConfig({
  define: {
    __ICHIRAN_RUST_M1__: JSON.stringify(process.env.ICHIRAN_RUST_M1 === '1')
  },
  plugins: [react(), serveOpaqueAnalyzerAssets()],
  worker: {
    format: 'es'
  }
});
