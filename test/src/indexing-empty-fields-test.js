import test from 'tape'
import { packageVersion } from '../../src/version.js'
import { EntryStream } from 'level-read-stream'

const { SearchIndex } = await import(
  '../../src/' + process.env.SI_TEST_ENTRYPOINT
)

const sandbox = 'test/sandbox/'
const dontIndexEmptyFields = sandbox + 'dontIndexEmptyFields'

const data = [
  {
    _id: '6',
    title: ' a volcano behind     waterfalls. [4472 × 5590] [OC]   '
  }
]

const global = {}

test('create a search index', async t => {
  t.plan(1)
  try {
    global[dontIndexEmptyFields] = await new SearchIndex({
      name: dontIndexEmptyFields
    })
    t.ok(global[dontIndexEmptyFields])
  } catch (e) {
    t.error(e)
  }
})

test('can add data', t => {
  t.plan(1)
  global[dontIndexEmptyFields].PUT(data).then(t.pass)
})

// TODO: '' now gives hits in everything: think about how this should work
test('should give no results for AND: [""]', t => {
  t.plan(1)
  global[dontIndexEmptyFields]
    .QUERY({
      AND: ['']
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: [],
        RESULT_LENGTH: 0
      })
    })
})

test('index looks good', t => {
  const expectedIndex = [
    {
      key: ['CREATED_WITH'],
      value: 'search-index@' + packageVersion
    },
    {
      key: ['DOC', '6'],
      value: {
        _id: '6',
        title: [
          ['4472', '1.00'],
          ['5590', '1.00'],
          ['a', '1.00'],
          ['behind', '1.00'],
          ['oc', '1.00'],
          ['volcano', '1.00'],
          ['waterfalls', '1.00']
        ]
      }
    },
    { key: ['DOCUMENT_COUNT'], value: 1 },
    {
      key: ['DOC_RAW', '6'],
      value: {
        _id: '6',
        title: ' a volcano behind     waterfalls. [4472 × 5590] [OC]   '
      }
    },
    { key: ['FIELD', 'title'], value: 'title' },
    { key: ['IDX', 'title', ['4472', '1.00']], value: ['6'] },
    { key: ['IDX', 'title', ['5590', '1.00']], value: ['6'] },
    { key: ['IDX', 'title', ['a', '1.00']], value: ['6'] },
    { key: ['IDX', 'title', ['behind', '1.00']], value: ['6'] },
    { key: ['IDX', 'title', ['oc', '1.00']], value: ['6'] },
    { key: ['IDX', 'title', ['volcano', '1.00']], value: ['6'] },
    { key: ['IDX', 'title', ['waterfalls', '1.00']], value: ['6'] }
  ]
  const actualIndex = []
  t.plan(1)
  new EntryStream(global[dontIndexEmptyFields].INDEX.STORE, {
    lt: ['~'],
    ...global[dontIndexEmptyFields].INDEX.LEVEL_OPTIONS
  })
    .on('data', d => actualIndex.push(d))
    .on('end', () => {
      t.deepEquals(actualIndex, expectedIndex)
    })
})
