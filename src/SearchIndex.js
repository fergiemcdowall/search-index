/// <reference path="typedefs.js" />

import {
  tokenizationPipelineStages,
  defaultPipeline
} from './tokenisationPipelineStages.js'
import { InvertedIndex } from 'fergies-inverted-index'
import { LRUCache } from 'lru-cache'
import { Reader } from './read.js'
import { Writer } from './write.js'
import { validateVersion } from './util.js'

/**
 * Class representing a search index.
 * @example
 *
 * import { SearchIndex } from 'search-index'
 *
 * // initialize an index
 * const { PUT, QUERY } = new SearchIndex(options)
 *
 * // add documents to the index
 * await PUT(documents)
 *
 * // read documents from the index
 * const results = await QUERY(query)
 */
export class SearchIndex {
  /**
   * Create a SearchIndex
   * @param {SIIndexOptions} [ops] - The initialisation options. Most can be overridden when reading or writing to the index.
   */
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
      name: 'index',
      ngrams: {},
      replace: {},
      storeRawDocs: true,
      stopwords: [],
      storeVectors: true, // TODO: make a test for this being false
      tokenSplitRegex: /[\p{L}\d]+/gu,
      tokenizer: defaultPipeline,
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

    // event bus
    this.EVENTS = this.INDEX.EVENTS

    // Now that constructor is not async- not sure where this should be called...
    this._CACHE = new LRUCache({ max: ops.cacheLength })
    this.r = new Reader(ops, this._CACHE, this.INDEX)
    this.w = new Writer(ops, this._CACHE, this.INDEX)
    validateVersion(this.INDEX)
  }

  /**
   * Get all the documents in the index. Limit if necessary.
   * @function ALL_DOCUMENTS
   * @param {number} [limit] - maximum documents to return
   * @returns {Promise<Array.<SIDocument>>} A promise returning an array of documents
   * @example
   * // Return all documents from index.
   * const documents = await ALL_DOCUMENTS(limit)
   * // "limit" is the maximum total of documents to be returned
   */
  ALL_DOCUMENTS = limit => this.r.ALL_DOCUMENTS(limit)

  /**
   * Return the IDs of documents for each given token filtered by the query result
   * @function BUCKETS
   * @param {...SIToken} [tokens] - Token spaces to create buckets on
   * @returns {Promise.<Array.<SIBucket>>} An array of the specified documents
   * @example
   * // Return the IDs of documents for each given token filtered by the
   * // query result
   * const buckets = await BUCKETS(token1, token2, tokenN)
   */
  BUCKETS = (...token) => this.INDEX.BUCKETS(...token)

  /**
   * Get index creation date
   * @function CREATED
   * @returns {Promise<string>} creation date in milliseconds as per Date.toISOString()
   * @example
   * // Find out when index was first created
   * const timestamp = await CREATED()
   */
  CREATED = () => this.INDEX.CREATED()

  /**
   * Delete documents from index
   * @function DELETE
   * @param {...(string|number)} [ids] - A spread array of ids of documents to be deleted
   * @returns {Promise<Array>} success status
   * @example
   * const result = await DELETE(id1, id2, idN)
   */
  DELETE = (...id) => this.w.DELETE(...id)

  /**
   * Deletes raw documents from index, but not their indexed values. These documents will therefore still be retrievable, but the index only contains a reference to them. The document itself must be stored elsewhere.
   * @function DELETE_RAW
   * @param {...(string|number)} [ids] - A spread array of ids of raw documents to be deleted. Document references will still be retrievable.
   * @returns {Promise<Array>} success status
   * @example
   * const result = await DELETE_RAW(id1, id2, idN)
   */
  DELETE_RAW = (...id) => this.w.DELETE_RAW(...id)

  /**
   * Returns the complete list of words stored in the index for the given Token. Returns for the whole index if no token is specified. DISTINCT differs from DICTIONARY in that it returns Tokens rather than strings. This means that one word that appears in many different fields will be returned per field in DISTINCT with the fieldname, but only once in DICTIONARY
   * @function DICTIONARY
   * @param {SIToken} [token] - Index space to search in
   * @returns {Promise<Array.<(string|number)>>} list of values
   * @example
   * // Return each available word for the given token space
   * const words = await DICTIONARY(token)
   *
   * // Return all words in the index
   * const allWords = await DICTIONARY()
   */
  DICTIONARY = (token, options) => this.r.DICTIONARY(token, options)

  /**
   * Returns the complete list of words stored in the index for the given Token. Returns for the whole index if no token is specified. DISTINCT differs from DICTIONARY in that it returns Tokens rather than strings. This means that one word that appears in many different fields will be returned per field in DISTINCT with the fieldname, but only once in DICTIONARY
   * @function DISTINCT
   * @param {...SIToken} [tokenSpace] - A spread array of index spaces to search in
   * @returns {Promise<Array.<SIToken>>} list of words
   * @example
   * // Return each available field/word combination for the given token space
   * const distinct = await DISTINCT(token1, token2, token3)
   */
  DISTINCT = (...token) => this.r.DISTINCT(...token)

  // TODO: shouldn't DOCUMENTS take IDs and return whole documents?
  /**
   * Return named documents from index.
   * @function DOCUMENTS
   * @param {...(number|string)} [ids] - A spread array of document ids. If no ids are specified, DOCUMENTS will return all documents in index.
   * @returns {Promise<Array.<SIDocument>>} Array of Documents
   * @example
   * const result = await DOCUMENTS()
   */
  DOCUMENTS = (...docs) => this.r.DOCUMENTS(...docs)

  /**
   * Return total number of documents in index
   * @function DOCUMENT_COUNT
   * @returns {Promise<number>} Total number of documents in index
   * @example
   * const totalDocs = await DOCUMENT_COUNT()
   */
  DOCUMENT_COUNT = () => this.r.DOCUMENT_COUNT()

  /**
   * Return named document vectors from index. Seeing the generated document vectors can be a useful way to debug indexing.
   * @function DOCUMENT_VECTORS
   * @returns {Promise<Array.<object>>} An array of document vectors
   * @param {...(number|string)} [ids] - A spread array of document ids
   * @example
   * const documentVectors = await DOCUMENT_VECTORS(id1, id2, id3)
   */
  DOCUMENT_VECTORS = (...ids) => this.r.DOCUMENT_VECTORS(...ids)

  /**
   * creates a backup/export of an index
   * @function EXPORT
   * @returns {Promise<object>} a backup/export of an index
   * @example
   * // Dump the index to file:
   * const indexExport = await EXPORT()
   *
   * // Create a new index from dump:
   * const si2 = new SearchIndex(ops)
   * await si2.IMPORT(indexExport)
   */
  EXPORT = () => this.INDEX.EXPORT()

  /**
   * Return document ids for each distinct field/value combination within the given token space.
   * @function FACETS
   * @param {...SIToken} [tokenSpace] - A spread array of token spaces to create facets on.
   * @returns {Promise<Array.<SIBucket>>} Array of Buckets
   * @example
   * const facets = await FACETS(token)
   */
  FACETS = (...token) => this.r.FACETS(...token)

  /**
   * Return every document fieldname that has been indexed
   * @function FIELDS
   * @returns {Promise<Array.<string>>} An array of fields
   * @example
   * const fields = await FIELDS()
   */
  FIELDS = () => this.INDEX.FIELDS()

  /**
   * Delete everything and start again (including creation metadata)
   * @function FLUSH
   * @example
   * @returns {Promise}
   * // I just want to watch the world burn 🔥
   * await FLUSH()
   */
  FLUSH = () => this.w.FLUSH()

  /**
   * creates an index from a backup/export
   * @function IMPORT
   * @param {object} [index] a serialised index dump as per the one generated by `EXPORT`
   * @returns {Promise}
   * @example
   * // having previously run:
   * const index = await anotherIndex.EXPORT()
   *
   * // you can them import like so:
   * await IMPORT(index)
   */
  IMPORT = index => this.w.IMPORT(index)

  /**
   * find out when index was last updated
   * @function LAST_UPDATED
   * @returns {Promise<number>} Timestamp as per Date.toISOString()
   * @example
   * const timestamp = await LAST_UPDATED()
   */
  LAST_UPDATED = () => this.INDEX.LAST_UPDATED()

  /**
   * Get the maxiumum/last value of the given token space
   * @function MAX
   * @returns {Promise<(number|string)>} The maxiumum/last value of the given token space
   * @param {Token} [token] - A token space
   * @example
   * const max = await MAX(token)
   */
  MAX = token => this.INDEX.MAX(token)

  /**
   * Get the miniumum/first value of the given token space
   * @function MIN
   * @returns {Promise<(number|string)>} The miniumum/first value of the given token space
   * @param {SIToken} [token] - A token space
   * @example
   * const min = await MIN(token)
   */
  MIN = token => this.INDEX.MIN(token)

  /**
   * Put documents into the index
   * @function PUT
   * @returns {Array.<object>} success or otherwise of the insertion
   * @param {Promise<Array.<SIDocument>>} [documents] - An array of documents to be indexed
   * @param {SIPutOptions} [options] - The options
   * @example
   * const result = await PUT(documents, options)
   */
  PUT = (documents, options) => this.w.PUT(documents, options)

  /**
   * Descriptioon here
   * @function PUT_RAW
   * @returns {Promise<Array>} success or otherwise of the insertion
   * @param {Array.<object>} [documents] An array of documents to be indexed where each document contains an _id field corresponding to an existing document in the index
   * @example
   * const result = await PUT_RAW(rawDocuments)
   */
  // TODO: this API should surely be better
  PUT_RAW = (docs, ids, dontStoreValue) =>
    this.w.PUT_RAW(docs, ids, dontStoreValue)

  /**
   * Query the index. See the documentation for instructions on how to compose queries.
   * @function QUERY
   * @returns {Promise<ResultSet>}
   * @param {Query} [query] - A query object
   * @param {SIQueryOptions} [options={SIQueryOptions}] - query options
   * @example
   * const results = await QUERY(query, options)
   */
  QUERY = (q, ops) => this.r.QUERY(q, ops)

  /**
   * Search the index (equivalent to QUERY(q, { SCORE: 'TFIDF', SORT: true} ))
   * @function SEARCH
   * @returns {Promise<ResultSet>}
   * @param  {Query} [query] - A query object
   * @example
   * const results = await SEARCH(q)
   */
  SEARCH = (query, ops) => this.r.SEARCH(query, ops)

  /**
   * @typedef {Object} TOKENIZATION_PIPELINE_STAGES
   * @memberOf SearchIndex
   */
  TOKENIZATION_PIPELINE_STAGES = tokenizationPipelineStages

  // internal functions inherited from fergies-inverted-index
  _AND = (tokens, pipeline) => this.INDEX.AND(tokens, pipeline)
  _BUCKET = token => this.INDEX.BUCKET(token)
  _GET = (tokens, pipeline) => this.INDEX.GET(tokens, pipeline)
  _NOT = (include, exclude) => this.INDEX.NOT(include, exclude)
  _OR = (tokens, pipeline) => this.INDEX.OR(tokens, pipeline)
  _PAGE = (results, options) => this.r.PAGE(results, options)
  _SORT = (results, options) => this.r.SORT(results, options)
}
