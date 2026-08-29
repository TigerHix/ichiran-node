const CACHE = 'ichiran-shell-__CACHE_VERSION__';
const CORE = /*__PRECACHE__*/[];
const CORE_PATHS = new Set(CORE);

self.addEventListener('install', event => {
  event.waitUntil(caches.open(CACHE).then(async cache => {
    for (let index = 0; index < CORE.length; index += 12) {
      await cache.addAll(CORE.slice(index, index + 12));
    }
    await self.skipWaiting();
  }));
});

self.addEventListener('activate', event => {
  event.waitUntil(
    caches.keys()
      .then(keys => Promise.all(keys
        .filter(key => key.startsWith('ichiran-shell-') && key !== CACHE)
        .map(key => caches.delete(key))))
      .then(() => self.clients.claim())
  );
});

self.addEventListener('fetch', event => {
  const request = event.request;
  if (request.method !== 'GET') return;
  const url = new URL(request.url);
  if (url.origin !== self.location.origin) return;
  const navigation = request.mode === 'navigate'
    && (url.pathname === '/' || url.pathname === '/index.html');
  if (!navigation && !CORE_PATHS.has(url.pathname)) return;

  event.respondWith(
    caches.open(CACHE)
      .then(cache => cache.match(navigation ? '/index.html' : url.pathname))
      .then(cached => cached || fetch(request))
  );
});
