const fii = require('fergies-inverted-index')

const Cache = require('./cache.js')
const reader = require('./read.js')
const writer = require('./write.js')

const makeASearchIndex = ops => {
  // ".flush" clears the cache ".cache" creates/promotes a cache entry
  const c = new Cache(ops.cacheLength)

  const w = writer(ops.fii, ops) // TODO: should be just ops?
  const r = reader(ops.fii)

  return {
    // internal functions inherited from fergies-inverted-index
    _AND: ops.fii.AND,
    _BUCKET: ops.fii.BUCKET,
    _CACHE: c,
    _GET: ops.fii.GET,
    _NOT: ops.fii.SET_SUBTRACTION,
    _OR: ops.fii.OR,

    // search-index read
    _PAGE: r.PAGE,
    _SCORE: r.SCORE,
    _SEARCH: r.SEARCH,
    _SORT: r.SORT,

    // public API
    ALL_DOCUMENTS: r.ALL_DOCUMENTS,
    BUCKETS: ops.fii.BUCKETS,
    DELETE: w.DELETE,
    DICTIONARY: token => c.cache({ DICTIONARY: token || null }, r.DICTIONARY(token)),
    DISTINCT: r.DISTINCT,
    DOCUMENTS: docs => c.cache({ DOCUMENTS: docs || null }, r.DOCUMENTS(docs)),
    DOCUMENT_COUNT: r.DOCUMENT_COUNT,
    EXPORT: ops.fii.EXPORT,
    FACETS: r.FACETS,
    FIELDS: ops.fii.FIELDS,
    IMPORT: idx => c.flush(ops.fii.IMPORT(idx)),
    INDEX: ops.fii,
    MAX: ops.fii.MAX,
    MIN: ops.fii.MIN,
    PUT: (docs, pops) => c.flush(w.PUT(docs, pops)),
    PUT_RAW: docs => c.flush(w.PUT_RAW(docs)),
    QUERY: (q, qops) => c.cache({ QUERY: [q, qops] }, r.QUERY(q, qops))
  }
}

const initIndex = (ops = {}) => new Promise((resolve, reject) => {
  ops = Object.assign({
    cacheLength: 1000,
    tokenAppend: '#',
    caseSensitive: false,
    storeVectors: false,
    storeRawDocs: true
  }, ops)
  //  if (ops.fii) return resolve(ops)
  // else
  return fii(ops).then(
    aNewFii => resolve(Object.assign({ fii: aNewFii }, ops))
  )
})

module.exports = ops => initIndex(ops).then(makeASearchIndex)
