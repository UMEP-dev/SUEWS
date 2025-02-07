import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import path from 'path'
import fs from 'fs'

// Copy schema file to build output
const schemaPath = path.resolve(__dirname, '../source/_static/suews-config-schema.json')
const outputDir = path.resolve(__dirname, '../source/_static/suews-config-ui')

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [
    react(),
    {
      name: 'copy-schema',
      buildEnd() {
        if (fs.existsSync(schemaPath)) {
          if (!fs.existsSync(outputDir)) {
            fs.mkdirSync(outputDir, { recursive: true })
          }
          fs.copyFileSync(
            schemaPath,
            path.join(outputDir, 'suews-config-schema.json')
          )
          console.log('Schema file copied to:', path.join(outputDir, 'suews-config-schema.json'))
        } else {
          console.warn('Schema file not found at:', schemaPath)
        }
      }
    }
  ],
  build: {
    outDir: outputDir,
    emptyOutDir: true,
    rollupOptions: {
      input: {
        main: path.resolve(__dirname, 'src/main.tsx')
      },
      output: {
        entryFileNames: 'main.js',
        chunkFileNames: '[name].js',
        assetFileNames: (assetInfo) => {
          const name = assetInfo.name || ''
          const info = name.split('.')
          const ext = info[info.length - 1]
          if (ext === 'css') {
            return 'main.css'
          }
          return '[name][ext]'
        }
      }
    }
  },
  base: './',
  server: {
    fs: {
      // Allow serving files from one level up to the project root
      allow: ['..']
    }
  }
})
