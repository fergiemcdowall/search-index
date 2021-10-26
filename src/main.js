const fii = require('fergies-inverted-index')
const NewTokenisationPipeline = require('./NewTokenisationPipeline')
// const DocumentProcessor = require('./DocumentProcessor')

const LRU = require('lru-cache')
const reader = require('./read.js')
const writer = require('./write.js')
const tp = require('./tokenizationPipeline.js')
const packageJSON = require('../package.json')

// eslint-disable-next-line
const makeASearchIndex = ops =>
  // eslint-disable-next-line
  new Promise(async resolve => {
    // TODO: change to https://www.npmjs.com/package/lru-cache
    // ".flush" clears the cache ".cache" creates/promotes a cache entry
    // const cache = new Cache(ops.cacheLength)

    // TODO: the cache size should be an option
    const cache = new LRU(1000)

    // eslint-disable-next-line
    const queue = new (await import('p-queue')).default({ concurrency: 1 })

    // TODO: should be just ops?
    const w = writer(ops, cache, queue)
    const r = reader(ops, cache)

    // TODO: more caching
    return w._INCREMENT_DOC_COUNT(0).then(() =>
      resolve({
        // internal functions inherited from fergies-inverted-index
        _AND: ops.fii.AND,
        _BUCKET: ops.fii.BUCKET,
        _GET: ops.fii.GET,
        _NOT: ops.fii.NOT,
        _OR: ops.fii.OR,
        // TODO: should cache be at the fii level?
        _CACHE: cache,
        // search-index read
        _PAGE: r.PAGE,
        _SCORE: r.SCORE,
        _SEARCH: r.SEARCH,
        _SORT: r.SORT,

        // public API (write)
        DELETE: w.DELETE,
        FLUSH: w.FLUSH,
        IMPORT: w.IMPORT,
        PUT: w.PUT,
        PUT_RAW: w.PUT_RAW,
        TOKENIZATION_PIPELINE_STAGES: new NewTokenisationPipeline(),

        // public API (read)
        ALL_DOCUMENTS: r.ALL_DOCUMENTS,
        BUCKETS: ops.fii.BUCKETS,
        CREATED: ops.fii.CREATED,
        DICTIONARY: r.DICTIONARY,
        DISTINCT: r.DISTINCT,
        DOCUMENTS: r.DOCUMENTS,
        DOCUMENT_COUNT: r.DOCUMENT_COUNT,
        EXPORT: ops.fii.EXPORT,
        FACETS: r.FACETS,
        FIELDS: ops.fii.FIELDS,
        INDEX: ops.fii,
        LAST_UPDATED: ops.fii.LAST_UPDATED,
        MAX: ops.fii.MAX,
        MIN: ops.fii.MIN,
        QUERY: r.QUERY,
        SEARCH: r.SEARCH
      })
    )
  })

const initIndex = (ops = {}) =>
  new Promise((resolve, reject) => {
    // TODO: dont pass tokenization ops through to fii. Use the
    // tokenization pipeline instead and always initialize fii with the
    // same ops
    ops = Object.assign(
      {
        cacheLength: 1000,
        caseSensitive: false,
        docExistsSpace: 'DOC_RAW',
        skipFields: [],
        ngrams: {},
        replace: {},
        storeRawDocs: true,
        // TODO: processDocuments probably shouldn't be an option?

        tokenizer: new NewTokenisationPipeline().tokenizer,
        // tokenizationPipeline: [
        //   tp.SPLIT,
        //   tp.SKIP,
        //   tp.LOWCASE,
        //   tp.REPLACE,
        //   tp.NGRAMS,
        //   tp.STOPWORDS,
        //   tp.SCORE_TERM_FREQUENCY
        // ],
        stopwords: [],
        storeVectors: true, // TODO: make a test for this being false
        tokenAppend: '#'
      },
      ops
    )

    return fii(ops).then(aNewFii =>
      resolve(
        Object.assign(
          {
            fii: aNewFii
          },
          ops
        )
      )
    )
  })

const validateVersion = si =>
  new Promise((resolve, reject) => {
    const key = ['CREATED_WITH']
    const version = 'search-index@' + packageJSON.version
    return si.INDEX.STORE.get(key)
      .then(v =>
        // throw a rejection if versions do not match
        version === v
          ? resolve()
          : reject(
              new Error(
                'This index was created with ' +
                  v +
                  ', you are running ' +
                  version
              )
            )
      )
      .catch(e => si.INDEX.STORE.put(key, version).then(resolve))
  })

module.exports = ops =>
  initIndex(ops)
    .then(makeASearchIndex)
    .then(si => validateVersion(si).then(() => si))
