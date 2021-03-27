const fii = require('fergies-inverted-index')

const Cache = require('./cache.js')
const reader = require('./read.js')
const writer = require('./write.js')
const tp = require('./tokenizerPipeline.js')
const packageJSON = require('../package.json')

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
    CREATED: ops.fii.CREATED,
    DELETE: (...ids) => c.flush().then(() => w.DELETE(ids)),
    DICTIONARY: token => c.cache({ DICTIONARY: token || null }, r.DICTIONARY(token)),
    DISTINCT: r.DISTINCT,
    DOCUMENTS: (...docs) => c.cache({
      DOCUMENTS: docs
    }, r.DOCUMENTS(...docs)),
    DOCUMENT_COUNT: r.DOCUMENT_COUNT,
    EXPORT: ops.fii.EXPORT,
    FACETS: r.FACETS,
    FIELDS: ops.fii.FIELDS,
    FLUSH: () => ops.fii.STORE.clear(),
    IMPORT: idx => c.flush().then(() => ops.fii.IMPORT(idx)),
    INDEX: ops.fii,
    LAST_UPDATED: ops.fii.LAST_UPDATED,
    MAX: ops.fii.MAX,
    MIN: ops.fii.MIN,
    PUT: (docs, pops) => c.flush().then(() => w.PUT(docs, pops)),
    PUT_RAW: docs => c.flush().then(() => w.PUT_RAW(docs)),
    QUERY: (q, qops) => c.cache({ QUERY: [q, qops] }, r.QUERY(q, qops)),

    // pipeline stages
    ...tp
  }
}

const initIndex = (ops = {}) => new Promise((resolve, reject) => {
  // TODO: dont pass tokenization ops through to fii. Use the
  // tokenization pipeline instead and always initialize fii with the
  // same ops
  ops = Object.assign({
    cacheLength: 1000,
    caseSensitive: false,
    docExistsSpace: 'DOC_RAW',
    doNotIndexField: [],
    ngrams: {},
    storeRawDocs: true,
    tokenizerPipeline: [
      tp.SPLIT,
      tp.DONT_INDEX_FIELD,
      tp.LOWCASE,
      tp.NGRAMS,
      tp.STOPWORDS,
      tp.SCORE_TFIDF
    ],
    stopwords: [],
    storeVectors: true, // TODO: make a test for this being false
    tokenAppend: '#'
  }, ops)

  return fii(ops).then(
    aNewFii => resolve(Object.assign({
      fii: aNewFii
    }, ops))
  )
})

const validateVersion = si => new Promise((resolve, reject) => {
  const key = '￮￮CREATED_WITH'
  const version = 'search-index@' + packageJSON.version
  return si.INDEX.STORE.get(key).then(v =>
    // throw a rejection if versions do not match
    (version === v)
      ? resolve()
      : reject(new Error(
        'This index was created with ' + v +
          ', you are running ' + version
      ))
  ).catch(e => si.INDEX.STORE.put(key, version).then(resolve))
})

module.exports = ops => initIndex(ops).then(makeASearchIndex).then(si =>
  validateVersion(si).then(() => si)
)
