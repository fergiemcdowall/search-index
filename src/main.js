import * as tokenization from './tokenisationPipeline.js'
import { InvertedIndex } from 'fergies-inverted-index'
import { LRUCache } from 'lru-cache'
import { Reader } from './read.js'
import { Writer } from './write.js'
import { validateVersion } from './util.js'

export class Main {
  constructor (ops = {}) {
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
      skipFields: [],
      ngrams: {},
      replace: {},
      storeRawDocs: true,
      stopwords: [],
      storeVectors: true, // TODO: make a test for this being false
      tokenSplitRegex: /[\p{L}\d]+/gu,
      tokenizer: tokenization.tokenizationPipeline,
      ...ops
    }
    this.INDEX = new InvertedIndex({
      ...ops,
      // isLeaf must be like so and is not a user defined option
      isLeaf: node =>
        Array.isArray(node) &&
        node.length === 2 &&
        node.every(
          item =>
            typeof item === 'string' ||
            typeof item === 'number' ||
            item === null
        )
    })
    // Now that constructor is not async- not sure where this should be called...
    this._CACHE = new LRUCache({ max: ops.cacheLength })
    this.r = new Reader(ops, this._CACHE, this.INDEX)
    this.w = new Writer(ops, this._CACHE, this.INDEX)
    validateVersion(this.INDEX)
  }

  // TODO: is this a sensible API?
  ALL_DOCUMENTS = limit => this.r.ALL_DOCUMENTS(limit)
  BUCKETS = (...token) => this.INDEX.BUCKETS(...token)
  CREATED = () => this.INDEX.CREATED()
  DELETE = (...id) => this.w.DELETE(...id)
  DELETE_RAW = (...id) => this.w.DELETE_RAW(...id)
  DICTIONARY = token => this.r.DICTIONARY(token)
  DISTINCT = (...token) => this.r.DISTINCT(...token)
  DOCUMENTS = (...docs) => this.r.DOCUMENTS(...docs)
  DOCUMENT_COUNT = () => this.r.DOCUMENT_COUNT()
  DOCUMENT_VECTORS = (...requestedDocs) =>
    this.r.DOCUMENT_VECTORS(...requestedDocs)

  EXPORT = () => this.INDEX.EXPORT()
  FACETS = (...token) => this.r.FACETS(...token)
  FIELDS = () => this.INDEX.FIELDS()
  FLUSH = () => this.w.FLUSH()
  IMPORT = index => this.INDEX.IMPORT(index)
  LAST_UPDATED = () => this.INDEX.LAST_UPDATED()
  MAX = token => this.INDEX.MAX(token)
  MIN = token => this.INDEX.MIN(token)
  PUT = (docs, ops) => this.w.PUT(docs, ops)
  PUT_RAW = (docs, ids, dontStoreValue) =>
    this.w.PUT_RAW(docs, ids, dontStoreValue)

  QUERY = (q, ops) => this.r.QUERY(q, ops)
  SEARCH = (q, ops) => this.r.SEARCH(q, ops)
  TOKENIZATION_PIPELINE_STAGES = {
    SPLIT: tokenization.SPLIT,
    SKIP: tokenization.SKIP,
    LOWCASE: tokenization.LOWCASE,
    REPLACE: tokenization.REPLACE,
    NGRAMS: tokenization.NGRAMS,
    STOPWORDS: tokenization.STOPWORDS,
    SCORE_TERM_FREQUENCY: tokenization.SCORE_TERM_FREQUENCY
  }

  // internal functions inherited from fergies-inverted-index
  _AND = (tokens, pipeline) => this.INDEX.AND(tokens, pipeline)
  _BUCKET = token => this.INDEX.BUCKET(token)
  _GET = (tokens, pipeline) => this.INDEX.GET(tokens, pipeline)
  _NOT = (include, exclude) => this.INDEX.NOT(include, exclude)
  _OR = (tokens, pipeline) => this.INDEX.OR(tokens, pipeline)
  _PAGE = (results, options) => this.r.PAGE(results, options)
  _SORT = (results, options) => this.r.SORT(results, options)
}
