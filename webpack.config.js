import glob from 'glob'
import path from 'path'
import pkg from './package.json' assert { type: "json" }
import webpack from 'webpack'
import { createRequire } from 'node:module'

const require = createRequire(import.meta.url)

const config = {
  plugins: [
    // deal with the funky invokation of p-queue using "await import"
    new webpack.optimize.LimitChunkCountPlugin({
      maxChunks: 1
    }),
    // Webpack 5 no longer polyfills 'process'
    new webpack.ProvidePlugin({
      process: 'process/browser.js'
    }),
    // as per https://github.com/webpack/changelog-v5/issues/10
    new webpack.ProvidePlugin({
      Buffer: ['buffer', 'Buffer']
    })
  ],
  target: [ 'web' ],
  resolve: {
    fallback: {
      // BREAKING CHANGE: webpack < 5 used to include polyfills for
      // node.js core modules by default. This is no longer the
      // case.
      assert: false,
      buffer: require.resolve('buffer/'),
      fs: false,
      path: require.resolve('path-browserify'),
      stream: require.resolve('stream-browserify'),
      util: false,
      os: require.resolve('os-browserify')
    }
  }
}

// module.exports = [
export default [{
    ...config,
    mode: 'production',
    entry: './src/main.js',
    output: {
      path: path.resolve('dist'),
      filename: 'search-index-' + pkg.version + '.js',
      library: 'SearchIndex'
    }
  },
  {
    ...config,
    mode: 'production',
    entry: './src/main.js',
    experiments: {
      outputModule: true
    },
    output: {
      path: path.resolve('dist'),
      filename: 'search-index-esm-' + pkg.version + '.js',
      library: {
        type: 'module'
      }
    }
  },
  {
    ...config,
    // Use "mode: 'production" to keep bundle size low(ish- around 3mb)
    // possibly it would be good to have some kind of code splitting
    // instead
    mode: 'production',
    entry: glob.sync('./test/src/init-test.js'),
    output: {
      path: path.resolve('test/sandbox'),
      filename: 'browser-tests.js'
    }
  }
]
