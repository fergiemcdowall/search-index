const fii = require('fergies-inverted-index')
const writer = require('./write.js')
const reader = require('./read.js')

const makeASearchIndex = ops => {
  const w = writer(ops.fii, ops)
  const r = reader(ops.fii)
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
    DISTINCT: r.DISTINCT, // TODO- documention
    DOCUMENTS: r.DOCUMENTS, // TODO- documention, also, should return as {RESULT: [...]} so that sorting/paging (and maybe even scoring?) works
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
