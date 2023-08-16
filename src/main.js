import PQueue from 'p-queue'
import Reader from './read.js'
import { InvertedIndex } from 'fergies-inverted-index'
// the following makes standard.js/ESLint throw a wobbly that can't be
// resolved by setting an ignore rule
// import packageJSON from '../package.json' assert { type: 'json' }
// So do this instead:
import { packageVersion } from './version.js'
import { LRUCache } from 'lru-cache'
import { Writer } from './write.js'
import * as tokenization from './tokenisationPipeline.js'

// // eslint-disable-next-line
// const makeASearchIndex = ops =>
//   // eslint-disable-next-line
//   new Promise(async resolve => {
//     // TODO: the cache size should be an option
//     const cache = new LRUCache({
//       max: 1000
//     })

//     const queue = new PQueue({ concurrency: 1 })

//     // TODO: should be just ops?
//     const w = Writer(ops, cache, queue)
//     const r = new Reader(ops, cache)

//     // TODO: more caching

//     return w._INCREMENT_DOC_COUNT(0).then(() =>
//       resolve({
//         // internal functions inherited from fergies-inverted-index
//         _AND: ops.fii.AND,
//         _BUCKET: ops.fii.BUCKET,
//         _GET: ops.fii.GET,
//         _NOT: ops.fii.NOT,
//         _OR: ops.fii.OR,
//         // TODO: should cache be at the fii level?
//         _CACHE: cache,
//         // search-index read
//         _PAGE: r.PAGE,
//         _SCORE: r.SCORE,
//         _SEARCH: r.SEARCH,
//         _SORT: r.SORT,

//         // public API (write)
//         DELETE: w.DELETE,
//         FLUSH: w.FLUSH,
//         IMPORT: w.IMPORT,
//         PUT: w.PUT,
//         PUT_RAW: w.PUT_RAW,
//         TOKENIZATION_PIPELINE_STAGES: {
//           SPLIT: tokenization.SPLIT,
//           SKIP: tokenization.SKIP,
//           LOWCASE: tokenization.LOWCASE,
//           REPLACE: tokenization.REPLACE,
//           NGRAMS: tokenization.NGRAMS,
//           STOPWORDS: tokenization.STOPWORDS,
//           SCORE_TERM_FREQUENCY: tokenization.SCORE_TERM_FREQUENCY
//         },

//         // public API (read)
//         ALL_DOCUMENTS: r.ALL_DOCUMENTS,
//         BUCKETS: ops.fii.BUCKETS,
//         CREATED: ops.fii.CREATED,
//         DICTIONARY: r.DICTIONARY,
//         DISTINCT: r.DISTINCT,
//         DOCUMENTS: r.DOCUMENTS,
//         DOCUMENT_COUNT: r.DOCUMENT_COUNT,
//         DOCUMENT_VECTORS: r.DOCUMENT_VECTORS,
//         EXPORT: ops.fii.EXPORT,
//         FACETS: r.FACETS,
//         FIELDS: ops.fii.FIELDS,
//         INDEX: ops.fii,
//         LAST_UPDATED: ops.fii.LAST_UPDATED,
//         MAX: ops.fii.MAX,
//         MIN: ops.fii.MIN,
//         QUERY: r.QUERY,
//         SEARCH: r.SEARCH
//       })
//     )
//   })

export class Main {
  constructor(ops = {}) {
    ops = {
      cacheLength: 1000,
      caseSensitive: false,
      docExistsSpace: 'DOC_RAW',
      idGenerator: (function* generateId() {
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

    this.INDEX = new InvertedIndex(ops)

    // Now that constructor is not async- not sure where this should be called...
    this._validateVersion()

    this._CACHE = new LRUCache({
      max: 1000
    })

    this.w = new Writer(
      ops,
      this._CACHE,
      new PQueue({ concurrency: 1 }),
      this.INDEX
    )
    this.r = new Reader(ops, this._CACHE, this.INDEX)

    // TODO: this should be something more sensible like "countDocs"
    // async so this is "fire and forget"
    this.w._INCREMENT_DOC_COUNT(0)

    // // public API (write)
    // this.DELETE = w.DELETE
    // this.FLUSH = w.FLUSH
    // this.IMPORT = w.IMPORT
    // // this.PUT = w.PUT
    // this.PUT_RAW = w.PUT_RAW
    // this.TOKENIZATION_PIPELINE_STAGES = {
    //   SPLIT: tokenization.SPLIT,
    //   SKIP: tokenization.SKIP,
    //   LOWCASE: tokenization.LOWCASE,
    //   REPLACE: tokenization.REPLACE,
    //   NGRAMS: tokenization.NGRAMS,
    //   STOPWORDS: tokenization.STOPWORDS,
    //   SCORE_TERM_FREQUENCY: tokenization.SCORE_TERM_FREQUENCY
    // }

    // // public API (read)
    // this.ALL_DOCUMENTS = r.ALL_DOCUMENTS
    // this.BUCKETS = ii.BUCKETS
    // this.CREATED = ii.CREATED
    // this.DICTIONARY = r.DICTIONARY
    // this.DISTINCT = r.DISTINCT
    // this.DOCUMENTS = r.DOCUMENTS
    // this.DOCUMENT_COUNT = r.DOCUMENT_COUNT
    // this.DOCUMENT_VECTORS = r.DOCUMENT_VECTORS
    // this.EXPORT = ii.EXPORT
    // this.FACETS = r.FACETS
    // this.FIELDS = ii.FIELDS
    // this.INDEX = ii
    // this.LAST_UPDATED = ii.LAST_UPDATED
    // this.MAX = ii.MAX
    // this.MIN = ii.MIN
    // this.QUERY = r.QUERY
    // this.SEARCH = r.SEARCH

    // TODO: put validateVersion here somewhere

    // return initIndex(ops)
    //   .then(makeASearchIndex)
    //   .then(si => validateVersion(si).then(() => si))
  }

  // internal functions inherited from fergies-inverted-index
  _AND(tokens, pipeline) {
    return this.INDEX.AND(tokens, pipeline)
  }

  _BUCKET(token) {
    return this.INDEX.BUCKET(token)
  }

  _GET(tokens, pipeline) {
    return this.INDEX.GET(tokens, pipeline)
  }

  _NOT(include, exclude) {
    return this.INDEX.NOT(include, exclude)
  }

  _OR(tokens, pipeline) {
    return this.INDEX.OR(tokens, pipeline)
  }

  _PAGE(results, options) {
    return this.r.PAGE(results, options)
  }

  _SORT(results, options) {
    return this.r.SORT(results, options)
  }

  TOKENIZATION_PIPELINE_STAGES = {
    SPLIT: tokenization.SPLIT,
    SKIP: tokenization.SKIP,
    LOWCASE: tokenization.LOWCASE,
    REPLACE: tokenization.REPLACE,
    NGRAMS: tokenization.NGRAMS,
    STOPWORDS: tokenization.STOPWORDS,
    SCORE_TERM_FREQUENCY: tokenization.SCORE_TERM_FREQUENCY
  }

  // // TODO: should cache be at the fii level?
  // _CACHE = this.cache

  ALL_DOCUMENTS(limit) {
    return this.r.ALL_DOCUMENTS(limit)
  }

  BUCKETS(...token) {
    return this.INDEX.BUCKETS(...token)
  }

  CREATED() {
    return this.INDEX.CREATED()
  }

  EXPORT() {
    return this.INDEX.EXPORT()
  }

  IMPORT(index) {
    return this.INDEX.IMPORT(index)
  }

  DELETE(...id) {
    return this.w.DELETE(...id)
  }

  DISTINCT(...tokens) {
    return this.r.DISTINCT(...tokens)
  }

  DICTIONARY(token) {
    return this.r.DICTIONARY(token)
  }

  DOCUMENTS(...docs) {
    return this.r.DOCUMENTS(...docs)
  }

  DOCUMENT_COUNT() {
    return this.r.DOCUMENT_COUNT()
  }

  DOCUMENT_VECTORS(...requestedDocs) {
    return this.r.DOCUMENT_VECTORS(...requestedDocs)
  }

  FACETS(...token) {
    return this.r.FACETS(...token)
  }

  FIELDS() {
    return this.INDEX.FIELDS()
  }

  FLUSH() {
    return this.w.FLUSH()
  }

  LAST_UPDATED() {
    return this.INDEX.LAST_UPDATED()
  }

  MAX(token) {
    return this.INDEX.MAX(token)
  }

  MIN(token) {
    return this.INDEX.MIN(token)
  }

  PUT(docs, ops) {
    return this.w.PUT(docs, ops)
  }

  // TODO: is this a sensible API?
  PUT_RAW(docs, ids, dontStoreValue) {
    return this.w.PUT_RAW(docs, ids, dontStoreValue)
  }

  QUERY(q, ops) {
    return this.r.QUERY(q, ops)
  }

  SEARCH(q, ops) {
    return this.r.SEARCH(q, ops)
  }

  //TODO: put into own file
  _validateVersion() {
    return new Promise((resolve, reject) => {
      const key = ['CREATED_WITH']
      const version = 'search-index@' + packageVersion
      return this.INDEX.STORE.get(key)
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
        .catch(e => this.INDEX.STORE.put(key, version).then(resolve))
    })
  }
}
