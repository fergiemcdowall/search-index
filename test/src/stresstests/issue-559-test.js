import test from 'tape'
import { SearchIndex } from 'search-index'
import { eng } from 'stopword'
import { readFileSync, writeFileSync } from 'node:fs'
const sandbox = 'test/sandbox/'
const indexName = 'indexingStressTest'

const idx = new SearchIndex({
  name: sandbox + indexName,
  stopwords: eng
})

test('can add some data to an empty search-index', t => {
  t.plan(1)
  const movies = JSON.parse(readFileSync('test/data/movies.json'))
  const start = Date.now()
  idx.PUT(movies).then(() => {
    t.pass('Data added in ' + (Date.now() - start) + 'ms')
  })
})

test('can search', t => {
  t.plan(1)
  const start = Date.now()
  idx.SEARCH(['gremlins'], { DOCUMENTS: true }).then(res => {
    t.pass('Search took ' + (Date.now() - start) + 'ms')
    console.log(
      JSON.stringify(
        res.RESULT.map(item => item._doc.title),
        null,
        2
      )
    )
  })
})

test('can EXPORT', t => {
  t.plan(1)
  const start = Date.now()
  idx.EXPORT().then(dump => {
    t.pass('data EXPORTed in ' + (Date.now() - start) + 'ms')
    dump = JSON.stringify(dump)
    writeFileSync(sandbox + indexName + '-dump', dump)
  })
})

test('can IMPORT a serialized index into an empty search-index', t => {
  const idx2 = new SearchIndex({
    name: sandbox + indexName + '2',
    stopwords: eng
  })
  t.plan(2)
  const moviesDump = JSON.parse(readFileSync(sandbox + indexName + '-dump'))
  let start = Date.now()

  idx2
    .IMPORT(moviesDump)
    .then(() => t.pass('index IMPORTed in ' + (Date.now() - start) + 'ms'))
    .then(() => {
      console.log('closing yo')
      start = Date.now()
      idx2.INDEX.STORE.close().then(() =>
        t.pass('index closed in ' + (Date.now() - start) + 'ms')
      )
    })
})

test('can reopen index', t => {
  t.plan(2)
  const start = Date.now()
  const idx3 = new SearchIndex({
    name: sandbox + indexName + '2',
    stopwords: eng
  })
  idx3.EVENTS.on('ready', () => {
    t.pass('index reopened and ready in ' + (Date.now() - start) + 'ms')
    idx3.SEARCH(['gremlins'], { DOCUMENTS: true }).then(res => {
      t.pass(
        'Time from reopening to getting results from new index took ' +
          (Date.now() - start) +
          'ms'
      )
      console.log(
        JSON.stringify(
          res.RESULT.map(item => item._doc.title),
          null,
          2
        )
      )
    })
  })
})
