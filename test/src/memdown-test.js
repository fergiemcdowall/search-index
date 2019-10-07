import encode from 'encoding-down'
import fii from 'fergies-inverted-index'
import levelup from 'levelup'
import memdown from 'memdown'
import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'memdown-test'

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

let db

test('create a fii with memdown', t => {
  t.plan(3)
  levelup(encode(memdown(indexName), {
    valueEncoding: 'json'
  }), (err, store) => {
    t.error(err)
    si({
      fii: fii({ store: store })
    }).then(db => db.PUT(data).then(() => {
      t.pass('ok')
    }).then(() => {
      db.SEARCH(
        'body.text:cool', // use colon? eg "body.text:cool"
        'body.text:really',
        'body.text:bananas'
      ).then(res => {
        t.looseEqual(res, [
          {
            _id: 'b',
            // how about "match"
            _match: [
              'body.text.cool:0.17',
              'body.text.really:0.17',
              'body.text.bananas:0.17'
            ],
            _score: 0.71,
            _doc: data[1]
          }
        ])
      })
    }))
    
  })
})
