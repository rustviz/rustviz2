import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

// rv-serve runs on :8080 in dev. The Vite dev server proxies API + static
// asset paths to it so `npm run dev` and `cargo run` can be developed against
// each other without CORS plumbing.
export default defineConfig({
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
});
