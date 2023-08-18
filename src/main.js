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

  // internal functions inherited from fergies-inverted-index
  _AND (tokens, pipeline) {
    return this.INDEX.AND(tokens, pipeline)
  }

  _BUCKET (token) {
    return this.INDEX.BUCKET(token)
  }

  _GET (tokens, pipeline) {
    return this.INDEX.GET(tokens, pipeline)
  }

  _NOT (include, exclude) {
    return this.INDEX.NOT(include, exclude)
  }

  _OR (tokens, pipeline) {
    return this.INDEX.OR(tokens, pipeline)
  }

  _PAGE (results, options) {
    return this.r.PAGE(results, options)
  }

  _SORT (results, options) {
    return this.r.SORT(results, options)
  }

  ALL_DOCUMENTS (limit) {
    return this.r.ALL_DOCUMENTS(limit)
  }

  BUCKETS (...token) {
    return this.INDEX.BUCKETS(...token)
  }

  CREATED () {
    return this.INDEX.CREATED()
  }

  DELETE (...id) {
    return this.w.DELETE(...id)
  }

  DELETE_RAW (...id) {
    return this.w.DELETE_RAW(...id)
  }

  DISTINCT (...token) {
    return this.r.DISTINCT(...token)
  }

  DICTIONARY (token) {
    return this.r.DICTIONARY(token)
  }

  DOCUMENTS (...docs) {
    return this.r.DOCUMENTS(...docs)
  }

  DOCUMENT_COUNT () {
    return this.r.DOCUMENT_COUNT()
  }

  DOCUMENT_VECTORS (...requestedDocs) {
    return this.r.DOCUMENT_VECTORS(...requestedDocs)
  }

  EXPORT () {
    return this.INDEX.EXPORT()
  }

  FACETS (...token) {
    return this.r.FACETS(...token)
  }

  FLUSH () {
    return this.w.FLUSH()
  }

  FIELDS () {
    return this.INDEX.FIELDS()
  }

  IMPORT (index) {
    return this.INDEX.IMPORT(index)
  }

  LAST_UPDATED () {
    return this.INDEX.LAST_UPDATED()
  }

  MAX (token) {
    return this.INDEX.MAX(token)
  }

  MIN (token) {
    return this.INDEX.MIN(token)
  }

  PUT (docs, ops) {
    return this.w.PUT(docs, ops)
  }

  // TODO: is this a sensible API?
  PUT_RAW (docs, ids, dontStoreValue) {
    return this.w.PUT_RAW(docs, ids, dontStoreValue)
  }

  QUERY (q, ops) {
    return this.r.QUERY(q, ops)
  }

  SEARCH (q, ops) {
    return this.r.SEARCH(q, ops)
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
}
