import path from 'path'
import { version } from './package.json'
import glob from 'glob'

export default () => ([

  // this stanza creates the stand-alone version of search-index that
  // can be included in the <script> tag
  {
    mode: 'production',
    entry: './src/main.js',
    output: {
      path: path.resolve(__dirname, './dist'),
      filename: 'search-index.' + version + '.js',
      libraryTarget: 'umd',
      globalObject: 'this',
      library: 'searchIndex'
    },
    module: {
      rules: [
        {
          test: /\.(js)$/,
          exclude: /(node_modules|bower_components)/,
          use: 'babel-loader'
        }
      ]
    },
    node: {
      fs: 'empty'
    },
  },

  // this stanza make all of the tests run in the browser
  {
    mode: 'production',
    entry: glob.sync('./test/src/*-test.js'),
    output: {
      path: path.resolve(__dirname, './test/sandbox'),
      filename: 'bundle.js'
    },
    node: {
      fs: 'empty'
    }
  }
  
])

