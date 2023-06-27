import { SearchIndex } from '../../src/main.js'

import tape from 'tape'

let db

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

tape('can create a search index WITHOUT an options object', async t => {
  t.plan(1)
  try {
    db = await new SearchIndex()
    t.ok(db)
  } catch (e) {
    t.error(e)
  }
})

tape('can add some data', t => {
  t.plan(1)
  db.PUT(data).then(t.pass)
})
