import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

// Two build modes:
//   * default (`npm run build`): emits a same-origin SPA at base = '/'
//     intended to be served by rv-serve from frontend/dist/.
//   * pages (`npm run build:pages`, --mode pages): emits an SPA at
//     base = '/playground/' intended to be served by GitHub Pages at
//     https://rustviz.github.io/playground/, with API requests pointed
//     at the Fly origin via VITE_API_BASE in .env.pages.
//
// `npm run dev` proxies API + asset paths to a locally-running rv-serve
// at :8080 so you can iterate without CORS plumbing.
export default defineConfig(({ mode }) => ({
  base: mode === 'pages' ? '/playground/' : '/',
  plugins: [react()],
  server: {
    port: 3000,
    proxy: {
      '/submit-code': 'http://127.0.0.1:8080',
      '/ex-assets': 'http://127.0.0.1:8080',
    },
  },
  build: {
    outDir: 'dist',
    sourcemap: true,
  },
}));
