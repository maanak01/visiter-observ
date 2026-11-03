// 電波がなくてもアプリを開き直せるようにするファイル。
// 方針は「ネットワーク優先」：電波があれば最新のページを取りに行き、ついでに端末に控えを保存する。
// 電波がない（または3秒以内に返事がない）ときは、端末の控えを表示する。
// 記録データは扱わない（記録は各ページが端末に保存している）。
const CACHE = 'vobs-pages-v1';
const PAGES = ['./kiroku_index.html', './keikaku.html'];

self.addEventListener('install', e => {
  e.waitUntil((async () => {
    const c = await caches.open(CACHE);
    // 1つのページが見つからなくても、ほかのページの控えは作る
    await Promise.all(PAGES.map(p => c.add(p).catch(() => {})));
    await self.skipWaiting();
  })());
});

self.addEventListener('activate', e => {
  e.waitUntil((async () => {
    const keys = await caches.keys();
    await Promise.all(keys.filter(k => k !== CACHE).map(k => caches.delete(k)));
    await self.clients.claim();
  })());
});

self.addEventListener('fetch', e => {
  const req = e.request;
  if(req.method !== 'GET' || new URL(req.url).origin !== location.origin) return;
  e.respondWith(networkFirst(req));
});

async function networkFirst(req){
  const c = await caches.open(CACHE);
  try{
    const res = await withTimeout(fetch(req), 3000);
    if(res.ok) c.put(req, res.clone());
    return res;
  }catch(err){
    const hit = await c.match(req, {ignoreSearch:true});
    if(hit) return hit;
    throw err;
  }
}

function withTimeout(p, ms){
  return new Promise((resolve, reject) => {
    const t = setTimeout(() => reject(new Error('timeout')), ms);
    p.then(r => { clearTimeout(t); resolve(r); }, err => { clearTimeout(t); reject(err); });
  });
}
