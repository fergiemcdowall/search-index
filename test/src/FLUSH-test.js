import test from 'tape'
import { EntryStream } from 'level-read-stream'
import { SearchIndex } from 'search-index'
import { packageVersion } from '../../src/version.js'

const global = {}
const sandbox = 'test/sandbox/'
const indexName = sandbox + 'FLUSH'

const data = [
  {
    _id: 'a',
    title: 'quite a cool document',
    body: {
      text: 'this document is really cool cool cool',
      metadata: 'coolness documentness'
    },
    importantNumber: 5000
  }
]

test('create a search index', t => {
  t.plan(1)
  try {
    global[indexName] = new SearchIndex({
      name: indexName,
      storeVectors: true
    })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add some data', t => {
  t.plan(1)
  global[indexName].PUT(data).then(() => {
    t.pass('ok')
  })
})

test('verify index structure', t => {
  const expectedIndexStructure = [
    {
      key: ['CREATED_WITH'],
      value: 'search-index@' + packageVersion
    },
    {
      key: ['DOC', 'a'],
      value: {
        _id: 'a',
        title: [
          ['a', '1.00'],
          ['cool', '1.00'],
          ['document', '1.00'],
          ['quite', '1.00']
        ],
        body: {
          text: [
            ['cool', '1.00'],
            ['document', '0.33'],
            ['is', '0.33'],
            ['really', '0.33'],
            ['this', '0.33']
          ],
          metadata: [
            ['coolness', '1.00'],
            ['documentness', '1.00']
          ]
        },
        importantNumber: [5000, 5000]
      }
    },
    { key: ['DOCUMENT_COUNT'], value: 1 },
    {
      key: ['DOC_RAW', 'a'],
      value: {
        _id: 'a',
        title: 'quite a cool document',
        body: {
          text: 'this document is really cool cool cool',
          metadata: 'coolness documentness'
        },
        importantNumber: 5000
      }
    },
    { key: ['FIELD', 'body.metadata'], value: 'body.metadata' },
    { key: ['FIELD', 'body.text'], value: 'body.text' },
    { key: ['FIELD', 'importantnumber'], value: 'importantnumber' },
    { key: ['FIELD', 'title'], value: 'title' },
    {
      key: ['IDX', 'body.metadata', ['coolness', '1.00']],
      value: ['a']
    },
    {
      key: ['IDX', 'body.metadata', ['documentness', '1.00']],
      value: ['a']
    },
    { key: ['IDX', 'body.text', ['cool', '1.00']], value: ['a'] },
    { key: ['IDX', 'body.text', ['document', '0.33']], value: ['a'] },
    { key: ['IDX', 'body.text', ['is', '0.33']], value: ['a'] },
    { key: ['IDX', 'body.text', ['really', '0.33']], value: ['a'] },
    { key: ['IDX', 'body.text', ['this', '0.33']], value: ['a'] },
    {
      key: ['IDX', 'importantnumber', [5000, 5000]],
      value: ['a']
    },
    { key: ['IDX', 'title', ['a', '1.00']], value: ['a'] },
    { key: ['IDX', 'title', ['cool', '1.00']], value: ['a'] },
    { key: ['IDX', 'title', ['document', '1.00']], value: ['a'] },
    { key: ['IDX', 'title', ['quite', '1.00']], value: ['a'] }
  ]
  t.plan(expectedIndexStructure.length)
  new EntryStream(global[indexName].INDEX.STORE, {
    lt: ['~']
  }).on('data', d => {
    // console.log(d)
    t.deepEquals(d, expectedIndexStructure.shift())
  })
})

test('FLUSH index and verify', t => {
  t.plan(2)
  const expectedIndexStructure = [{ key: ['DOCUMENT_COUNT'], value: 0 }]
  global[indexName]
    .FLUSH()
    .then(() =>
      new EntryStream(global[indexName].INDEX.STORE, {
        lt: ['~']
      }).on('data', d => t.deepEquals(d, expectedIndexStructure.shift()))
    )
    .then(() => t.pass('index appears empty'))
})
