const lpad = require('left-pad')
const sandbox = 'test/sandbox/'
const test = require('tape')
const SearchIndex = require('../../../')
const wnum = require('written-number')
const Readable = require('stream').Readable

var si

test('initialize a search index', t => {
  t.plan(1)
  SearchIndex({
    indexPath: sandbox + '328-test'
  }, (err, newSi) => {
    t.error(err)
    si = newSi
  })
})

test('concurrently index docs using concurrentAdd', t => {
  t.plan(1)
  si.concurrentAdd({}, [{
    "id":"offering:21",
    "originalId":"21",
    "sortedId":"000000000021",
    "body":[
      "And Filter Operator",
      ""
    ],
    "userId":"b0946ffa20357194688e54b1cf143c9c",
    "productSpecification":"000000000017",
    "categoriesId":[
      "1",
      "5"
    ],
    "categoriesName":[
      "WireCloud Component",
      "WireCloud Operator"
    ],
    "href":"http://store.lab.fiware.org/DSProductCatalog/api/catalogManagement/v2/catalog/19/productOffering/21:(0.1)",
    "name":"And Filter Operator",
    "lifecycleStatus":"Launched",
    "isBundle":false,
    "catalog":"19"
  }], err => {
    t.error(err)
  })
})

test('search', t => {
  t.plan(2)
  var i = 0
  si.search({
    query: {
      AND: {
        lifecycleStatus: ["launched"],
        categoriesId: ["5"]
      }
    }
  })
    .on('data', hit => {
      t.equal(hit.id, 'offering:21')
      // t.equal(hit.score, 0.1505149978319906) <- gives variable results on travis- investigate
      i++
    })
    .on('end', () => {
      t.equal(i, 1)
    })
    .on('error', e => {
      t.fail(e)
    })
})

