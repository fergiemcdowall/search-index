const sandbox = 'test/sandbox/'
const indexName = 'indexingStressTest'
import test from 'tape'
import { MemoryLevel } from 'memory-level'
import { SearchIndex } from 'search-index'
import { eng } from 'stopword'
import { readFileSync, writeFileSync } from 'node:fs'

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
    // console.log(JSON.stringify(res, null, 2))
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
  t.plan(1)
  const moviesDump = JSON.parse(readFileSync(sandbox + indexName + '-dump'))
  const start = Date.now()
  idx2
    .IMPORT(moviesDump)
    .then(() => t.pass('index IMPORTed in ' + (Date.now() - start) + 'ms'))
})
