import PQueue from 'p-queue'
import Reader from './read.js'
import fii from 'fergies-inverted-index'
// the following makes standard.js/ESLint throw a wobbly that can't be
// resolved by setting an ignore rule
// import packageJSON from '../package.json' assert { type: 'json' }
// So do this instead:
import { packageVersion } from './version.js'
import { LRUCache } from 'lru-cache'
import { Writer } from './write.js'
import * as tokenization from './tokenisationPipeline.js'

// eslint-disable-next-line
const makeASearchIndex = ops =>
  // eslint-disable-next-line
  new Promise(async resolve => {
    // TODO: the cache size should be an option
    const cache = new LRUCache({
      max: 1000
    })

    // eslint-disable-next-line
    // const queue = new (await import('p-queue')).default({ concurrency: 1 })

    const queue = new PQueue({ concurrency: 1 })

    // TODO: should be just ops?
    const w = Writer(ops, cache, queue)
    const r = new Reader(ops, cache)

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
        TOKENIZATION_PIPELINE_STAGES: {
          SPLIT: tokenization.SPLIT,
          SKIP: tokenization.SKIP,
          LOWCASE: tokenization.LOWCASE,
          REPLACE: tokenization.REPLACE,
          NGRAMS: tokenization.NGRAMS,
          STOPWORDS: tokenization.STOPWORDS,
          SCORE_TERM_FREQUENCY: tokenization.SCORE_TERM_FREQUENCY
        },

        // public API (read)
        ALL_DOCUMENTS: r.ALL_DOCUMENTS,
        BUCKETS: ops.fii.BUCKETS,
        CREATED: ops.fii.CREATED,
        DICTIONARY: r.DICTIONARY,
        DISTINCT: r.DISTINCT,
        DOCUMENTS: r.DOCUMENTS,
        DOCUMENT_COUNT: r.DOCUMENT_COUNT,
        DOCUMENT_VECTORS: r.DOCUMENT_VECTORS,
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
    ops = {
      cacheLength: 1000,
      caseSensitive: false,
      docExistsSpace: 'DOC_RAW',
      idGenerator: (function * generateId () {
        let i = 0
        while (true) {
          yield Date.now() + '-' + i++
        }
      })(),
      isLeaf: node =>
        Array.isArray(node) &&
        node.length === 2 &&
        node.every(
          item =>
            typeof item === 'string' ||
            typeof item === 'number' ||
            item === null
        ),
      skipFields: [],
      ngrams: {},
      replace: {},
      storeRawDocs: true,
      stopwords: [],
      storeVectors: true, // TODO: make a test for this being false
      tokenAppend: '#',
      tokenSplitRegex: /[\p{L}\d]+/gu,
      tokenizer: tokenization.tokenizationPipeline,
      ...ops
    }

    return fii(ops).then(aNewFii =>
      resolve({
        fii: aNewFii,
        ...ops
      })
    )
  })

const validateVersion = si =>
  new Promise((resolve, reject) => {
    const key = ['CREATED_WITH']
    const version = 'search-index@' + packageVersion
    return si.INDEX.STORE.get(key, si.INDEX.LEVEL_OPTIONS)
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
      .catch(e =>
        si.INDEX.STORE.put(key, version, si.INDEX.LEVEL_OPTIONS).then(resolve)
      )
  })

export class SearchIndex {
  constructor (ops) {
    return initIndex(ops)
      .then(makeASearchIndex)
      .then(si => validateVersion(si).then(() => si))
  }
}
