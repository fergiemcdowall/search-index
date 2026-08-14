import test from 'tape'
import { SearchIndex } from 'search-index'
import { packageVersion } from '../../src/version.js'
import { validateVersion } from '../../src/util.js'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'version-stamp-test'
const mismatchIndexName = sandbox + 'version-stamp-mismatch-test'

test('create a search index', async function (t) {
  t.plan(3)

  const si = new SearchIndex({
    name: indexName
  })
  t.ok(await si.PUT)

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
})

// Write an index to disk and stamp it as the work of another version of
// search-index, then close it so that it can be reopened from scratch (a
// LevelDB cannot be opened twice concurrently)
const createIndexStampedWith = async (name, stamp) => {
  const si = new SearchIndex({ name })
  // the underlying index timestamps itself asynchronously from its constructor,
  // so wait for that to land before closing the store
  await new Promise(resolve => si.EVENTS.once('ready', resolve))
  await si.PUT([{ _id: '0', text: 'just a test' }])
  await si.INDEX.STORE.put(['CREATED_WITH'], stamp)
  await si.INDEX.STORE.close()
}

const createMismatchedIndex = name =>
  createIndexStampedWith(name, 'search-index@0.0.1')

// validateVersion is documented as throwing "a rejection if versions do not
// match", but it *returns* the Error instead of throwing it, so the promise
// fulfils and the mismatch is silently swallowed
test('validateVersion rejects when an index was created by another version', async function (t) {
  t.plan(1)

  const name = mismatchIndexName + '-validateVersion'
  await createMismatchedIndex(name)

  const si = new SearchIndex({ name })
  await validateVersion(si.INDEX).then(
    () => t.fail('validateVersion should not have resolved'),
    error =>
      t.equals(
        error.message,
        'This index was created with search-index@0.0.1, you are running search-index@' +
          packageVersion
      )
  )
})

// SearchIndex.js calls validateVersion but discards the promise it returns, so
// a mismatch is unobservable from the outside: the index opens, writes succeed,
// and nothing is reported on the event bus either
test('a version mismatch is observable when opening an index', async function (t) {
  t.plan(1)

  const name = mismatchIndexName + '-open'
  await createMismatchedIndex(name)

  const si = new SearchIndex({ name })
  const mismatch = await Promise.race([
    new Promise(resolve => si.EVENTS.on('error', resolve)),
    // if a mismatch is never surfaced, settle so the test can report that
    si
      .PUT([{ _id: '1', text: 'written to a mismatched index' }])
      .then(() => null)
  ])
  t.match(
    (mismatch || {}).message || 'no error was surfaced',
    /^This index was created with/
  )
})

// NOTE: if fields in the semver numbers ever actually reach 99 (has not
// happened so far) then these tests will go weird

test('an index created by another patch version of the same MAJOR fails', async function (t) {
  t.plan(1)

  const name = mismatchIndexName + '-same-major'
  const [major] = packageVersion.split('.')[0]
  await createIndexStampedWith(name, 'search-index@' + major + '.99.99')

  const si = new SearchIndex({ name })
  await si.VERSION_VALIDATED.then(
    () =>
      t.error('since minor version dod not match this test should have failed'),
    error => t.pass(error)
  )
})

// So 6.6.2 will work with 6.6.35. This means that code updates which change the
// the structure of the index need to be comitted with a MINOR semver bump
test('an index created by another patch version of the same MINOR passes', async function (t) {
  t.plan(1)

  const name = mismatchIndexName + '-same-minor'
  const minor = packageVersion.split('.').slice(0, 2).join('.')
  await createIndexStampedWith(name, 'search-index@' + minor + '.99.99')

  const si = new SearchIndex({ name })
  await si.VERSION_VALIDATED.then(
    () => t.pass('no version mismatch was reported'),
    error => t.error(error)
  )
})
