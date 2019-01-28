import fs from 'fs'
import pkg from './package.json'

export default [
  // create common.js and es6 bundles
  {
    input: 'src/main.js',
    output: [
      { file: pkg.main, format: 'cjs' },
      { file: pkg.module, format: 'es' }
    ]
  },
  // compile tests to commonjs for running with `tape`
  ...fs.readdirSync('test/src').map(f => {
    return {
      input: 'test/src/' + f,
      output: [
        { file: 'test/cjs/' + f, format: 'cjs' }
      ]
    }
  })
]

