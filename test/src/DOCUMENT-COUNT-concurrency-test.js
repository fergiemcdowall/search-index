import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'DOCUMENT_COUNT-concurrency'

const data = [...Array(10)].map((_, n) => ({ _id: 'd' + n, body: 'document' }))

console.log(data)

const global = {}

test('create a search index', t => {
  t.plan(1)
  try {
    global[indexName] = new SearchIndex({ name: indexName })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add data', t => {
  t.plan(1)
  global[indexName].PUT(data).then(res => t.equals(res.length, 10))
})

test('DOCUMENT_COUNT is correct after a serial PUT', t => {
  t.plan(1)
  global[indexName].DOCUMENT_COUNT().then(count => t.equals(count, 10))
})

// DELETE does not go through the write queue (see the `TODO: should be queued`
// in write.js), so the read-modify-write in #incrementDocCount interleaves and
// all but one of these decrements is lost
test('concurrent DELETEs keep DOCUMENT_COUNT in step with the index', t => {
  t.plan(2)
  Promise.all(data.map(doc => global[indexName].DELETE(doc._id)))
    .then(() =>
      Promise.all([
        global[indexName].DOCUMENT_COUNT(),
        global[indexName].ALL_DOCUMENTS()
      ])
    )
    .then(([count, documents]) => {
      t.equals(documents.length, 0, 'every document was deleted')
      t.equals(count, 0, 'DOCUMENT_COUNT agrees with the number of documents')
    })
})
