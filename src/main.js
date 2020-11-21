const fii = require('fergies-inverted-index')
const writer = require('./write.js')
const reader = require('./read.js')

const makeASearchIndex = ops => {
  const w = writer(ops.fii, ops)
  const r = reader(ops.fii)
  return {
    // internal functions inherited from fergies-inverted-index
    _AGGREGATE: ops.fii.AGGREGATE,
    _AND: ops.fii.AND,
    _BUCKET: ops.fii.BUCKET,
    _BUCKETS: ops.fii.BUCKETS,
    _FIELDS: ops.fii.FIELDS,
    _GET: ops.fii.GET,
    _NOT: ops.fii.SET_SUBTRACTION,
    _OR: ops.fii.OR,

    // search-index read
    _DISTINCT: r.DISTINCT,
    _DOCUMENT_COUNT: r.DOCUMENT_COUNT,
    _FACETS: r.FACETS,
    _PAGE: r.PAGE,
    _SCORE: r.SCORE,
    _SEARCH: r.SEARCH,
    _SORT: r.SORT,

    // search-index write
    _DELETE: w.DELETE,
    _PUT: w._PUT,
    _PUT_RAW: w._PUT_RAW,

    // public API
    DELETE: w.DELETE,
    DICTIONARY: r.DICTIONARY,
    DOCUMENTS: r.DOCUMENTS,
    DOCUMENT_COUNT: r.DOCUMENT_COUNT,
    EXPORT: ops.fii.EXPORT,
    FIELDS: r.FIELDS,
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
    storeVectors: false
  }, ops)
  if (ops.fii) return resolve(ops)
  // else
  return fii(ops).then(
    aNewFii => resolve(Object.assign({ fii: aNewFii }, ops))
  )
})

// export default ops => initIndex(ops).then(makeASearchIndex)
module.exports = ops => initIndex(ops).then(makeASearchIndex)
