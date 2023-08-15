import test from 'tape'
import { SearchIndex } from 'search-index'
import { packageVersion } from '../../src/version.js'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'version-stamp-test'

test('create a search index', async function (t) {
  t.plan(3)

  const si = new SearchIndex({
    name: indexName
  })
  t.ok(si.PUT)

  t.deepEquals(
    await si.PUT([
      {
        _id: '0',
        text: 'just a test'
      }
    ]),
    [{ _id: '0', status: 'CREATED', operation: 'PUT' }]
  )

  t.equals(
    await si.INDEX.STORE.get(['CREATED_WITH']),
    'search-index@' + packageVersion
  )

  // TODO: test rejections when trying to open with an incorrect
  // version of search-index
})
