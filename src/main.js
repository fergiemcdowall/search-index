const fii = require('fergies-inverted-index')

const Cache = require('./cache.js')
const reader = require('./read.js')
const writer = require('./write.js')

const makeASearchIndex = ops => {
  const cache = new Cache()
  cache.set('boom', 'diggy')
  cache.has('boom')
  console.log(cache.get('boom'))

  const w = writer(ops.fii, ops) // TODO: should be just ops?
  const r = reader(ops.fii, cache)
  return {
    // internal functions inherited from fergies-inverted-index
    _AND: ops.fii.AND,
    _BUCKET: ops.fii.BUCKET,
    _GET: ops.fii.GET,
    _NOT: ops.fii.SET_SUBTRACTION,
    _OR: ops.fii.OR,

    // search-index read
    _PAGE: r.PAGE,
    _SCORE: r.SCORE,
    _SEARCH: r.SEARCH,
    _SORT: r.SORT,

    // public API
    BUCKETS: ops.fii.BUCKETS,
    DELETE: w.DELETE,
    DICTIONARY: r.DICTIONARY,
    DISTINCT: r.DISTINCT,
    DOCUMENTS: r.DOCUMENTS,
    DOCUMENT_COUNT: r.DOCUMENT_COUNT,
    EXPORT: ops.fii.EXPORT,
    FACETS: r.FACETS,
    FIELDS: ops.fii.FIELDS,
    IMPORT: ops.fii.IMPORT,
    INDEX: ops.fii,
    MAX: ops.fii.MAX,
    MIN: ops.fii.MIN,
    PUT: w.PUT,
    PUT_RAW: w.PUT_RAW,
    QUERY: r.QUERY
  }
}

const initIndex = (ops = {}) => new Promise((resolve, reject) => {
  ops = Object.assign({
    tokenAppend: '#',
    caseSensitive: false,
    storeVectors: false,
    storeRawDocs: true
  }, ops)
  if (ops.fii) return resolve(ops)
  // else
  return fii(ops).then(
    aNewFii => resolve(Object.assign({ fii: aNewFii }, ops))
  )
})

module.exports = ops => initIndex(ops).then(makeASearchIndex)
