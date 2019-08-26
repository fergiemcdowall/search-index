/* global si */
import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'

const data = [
  {
    _id: 'a',
    title: 'quite a cool document',
    band: ['john', 'paul', 'ringo', 'george'],
    body: {
      text: 'this document is really cool cool cool',
      metadata: 'coolness documentness'
    },
    importantNumber: 5000
  },
  {
    _id: 'b',
    title: 'quite a cool document',
    band: ['john', 'paul', 'ringo', 'george'],
    body: {
      text: 'this document is really cool bananas',
      metadata: 'coolness documentness'
    },
    importantNumber: 500
  },
  {
    _id: 'c',
    title: 'something different',
    band: ['john', 'paul', 'ringo', 'george'],
    body: {
      text: 'something totally different',
      metadata: 'coolness documentness'
    },
    importantNumber: 200
  }
]

test('create a search index', t => {
  t.plan(1)
  si({
    name: sandbox + 'indexing-test'
  }, (err, idx) => {
    global.idx = idx
    t.error(err)
  })
})

test('can add some data', t => {
  t.plan(1)
  idx.PUT(data).then(() => {
    t.pass('ok')
  })
})

test('check that arrays were properly indexed', t => {
  t.plan(4)
  var band = [
    { key: 'band.george:0.25', value: [ 'a', 'b', 'c' ] },
    { key: 'band.john:0.25', value: [ 'a', 'b', 'c' ] },
    { key: 'band.paul:0.25', value: [ 'a', 'b', 'c' ] },
    { key: 'band.ringo:0.25', value: [ 'a', 'b', 'c' ] }
  ]
  idx.INDEX.STORE.createReadStream({
    gte: 'band!',
    lte: 'band~'
  }).on('data', data => t.looseEqual(data, band.shift()))
})
