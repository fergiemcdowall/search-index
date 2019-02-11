import path from 'path'

export default () => (
  {
    mode: 'production',
    entry: 'test/src/*-test.js',
    output: {
      path: path.resolve(__dirname, './test/sandbox'),
      filename: 'bundle.js'
    },
    node: {
      fs: 'empty'
    }
  }
)
