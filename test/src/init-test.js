/* global si */
import si from '../../dist/search-index.esm.js'
import test from 'tape'

const data = [
  {
    _id: 'a',
    title: 'quite a cool document',
    body: {
      text: 'this document is really cool cool cool',
      metadata: 'coolness documentness'
    },
    importantNumber: 5000
  },
  {
    _id: 'b',
    title: 'quite a cool document',
    body: {
      text: 'this document is really cool bananas',
      metadata: 'coolness documentness'
    },
    importantNumber: 500
  },
  {
    _id: 'c',
    title: 'something different',
    body: {
      text: 'something totally different',
      metadata: 'coolness documentness'
    },
    importantNumber: 200
  }
]

test('can create a search index WITHOUT an options object', t => {
  t.plan(1)
  si().then(db => {
    global['db'] = db    
    t.pass('ok')
  })
})

test('can add some data', t => {
  t.plan(1)
  global['db'].PUT(data).then(() => {
    t.pass('ok')
  })
})

