import { defineConfig } from "vite";

export default defineConfig({
  root: "src",
  publicDir: "../public",
  clearScreen: false,
  server: {
    host: "127.0.0.1",
    port: 1420,
    strictPort: true,
    watch: {
      ignored: ["**/src-tauri/**"]
    }
  },
  build: {
    outDir: "../dist",
    emptyOutDir: true
  }
});
