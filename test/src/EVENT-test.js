import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'EVENTS'
const global = {}

test('create a search index and listen to the "ready" event', t => {
  t.plan(2)
  t.ok((global[indexName] = new SearchIndex({ name: indexName })), !undefined)
  global[indexName].EVENTS.on('ready', () => t.pass('ready event emitted'))
})
