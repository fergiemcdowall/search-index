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
   * A lexicographical space in the index
   *
   * Tokens are used for lots<br> of things
   * @typedef Token
   * @memberof SearchIndex
   * @type {object}
   * @property {(string|string[])} FIELD - a field name, or array of names.
   * @property {(string|number|SearchIndex.Range)} VALUE - a value or range of values.
   */

  /**
   * @typedef Range
   * @memberof SearchIndex
   * @type {object}
   * @property {(string|number)} GTE - greater than or equal to
   * @property {(string|number)} LTE - less than or equal to
   */

  /**
   * @typedef Bucket
   * @memberof SearchIndex
   * @type {object}
   * @property {string} FIELD - The field name
   * @property {SearchIndex.Range} VALUE - The token range
   * @property {string[]} _id - An array of _ids of the documents that contain the specified token
   */

  /**
   * @typedef Document
   * @memberof SearchIndex
   * @type {object}
   * @property {(string|number)} _id
   * @property {object} _doc
   */

  /**
   * Options for writing documents to the index
   * @typedef {Object} PutOptions
   * @memberof SearchIndex
   * @property {boolean} [caseSensitive=false] - Case sensitivity. Default is `false`
   * @property {object} [ngrams={}] - Specify ngrams as per https://www.npmjs.com/package/ngraminator
   * @property {object} [replace={ { fields: [], values: {} } }] - replace tokens
   * @property {Array} [skipField=[]] - These fields will not be searchable, but they will still be stored
   * @property {Array} [stopwords=[]] - A list of words to be ignored
   * @property {boolean} [storeRawDocs=true] - Whether to store the raw document or not. In many cases it may be desirable to store it externally, or to skip storing when indexing if it is going to be updated directly later on
   * @property {boolean} [storeVectors=true] - Whether to store the raw document or not. In many cases it may be desirable to store it externally, or to skip storing when indexing if it is going to be updated directly later on
   * @property {Array} [tokenizationPipeline=[ SPLIT,  SKIP,  LOWCASE,  REPLACE,  NGRAMS,  STOPWORDS,  SCORE_TERM_FREQUENCY ]] - Tokenisation pipeline. Stages can be added and reordered
   * @property {RegExp} [tokenSplitRegex=/[\p{L}\d]+/gu] - The regular expression that splits strings into tokens
   */

  /**
   * Options for the index iteself
   * @typedef {Object} IndexOptions
   * @memberof SearchIndex
   * @property {number} [cacheLength=1000] - The length of the LRU cache. A higher value makes the index faster but uses more memory
   * @property {Level} [db=(BrowserLevel|ClassicLevel)] -  An abstract-level store. Defaults to BrowserLevel when compiled for web and ClassicLevel when compiled for node. You can also run `SearchIndex` on other key-value backends- see https://github.com/Level/abstract-level and https://github.com/Level/awesome for inspiration.
   */

  /**
   * Options for the index iteself
   * @typedef {Object} QueryOptions
   * @memberof SearchIndex
   * @property {Array} [BUCKETS=[]] - Aggregate on user defined buckets.
   * @property {boolean} [DOCUMENTS=false] - If `true` return entire document, if not `true` return reference to document
   * @property {Array} [FACETS=[]] - Aggregate on user-defined facets
   * @property {object} [PAGE={ NUMBER: 0, SIZE: 20 }] - Pagination
   * @property {Promise} [PIPELINE=token => new Promise(resolve => resolve(token))] - Query tokenization pipeline
   * @property {String} [SCORE='TFIDF'] - Calculate a relevancy value per document
   * @property {object} [SORT={ TYPE: 'NUMERIC', DIRECTION: 'DESCENDING', FIELD: '_score' }] - Sort documents
   * @property {object} [WEIGHT=[]] - Weight fields and/or values
   */

  /**
   * Options to initialise index with. Equivalent to `{...SearchIndex.IndexOptions, ...SearchIndex.PutOptions}`
   * @typedef {Object} Options
   * @memberof SearchIndex
   * @property {...SearchIndex.IndexOptions}
   * @property {...SearchIndex.PutOptions}
   */

  /**
   * Create a SearchIndex.
   * @param {SearchIndex.Options} [options] - The initialisation options. Most can be overridden when reading or writing to the index.
   */

  // @param {object} [options={IndexOptions...IndexOptions, ...PutOptions}] - The initialisation options. Most can be overridden with `PUT` or `QUERY`.
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
    // Now that constructor is not async- not sure where this should be called...
    this._CACHE = new LRUCache({ max: ops.cacheLength })
    this.r = new Reader(ops, this._CACHE, this.INDEX)
    this.w = new Writer(ops, this._CACHE, this.INDEX)
    validateVersion(this.INDEX)
  }

  /**
   * Get all the documents in the index. Limit if necessary.
   * @function ALL_DOCUMENTS
   * @memberOf SearchIndex
   * @instance
   * @param {number} [limit] - maximum documents to return
   * @returns {Promise<Array.<SearchIndex.Document>>} A promise returning an array of documents
   * @example
   * // Return all documents from index.
   * const documents = await ALL_DOCUMENTS(limit)
   * // "limit" is the maximum total of documents to be returned
   */
  ALL_DOCUMENTS = limit => this.r.ALL_DOCUMENTS(limit)

  /**
   * Return the IDs of documents for each given token filtered by the query result
   * @function BUCKETS
   * @memberOf SearchIndex
   * @instance
   * @param {...SearchIndex.Token} [tokens] - Token spaces to create buckets on
   * @returns {Promise.<Array.<SearchIndex.Bucket>>} An array of the specified documents
   * @example
   * // Return the IDs of documents for each given token filtered by the
   * // query result
   * const buckets = await BUCKETS(token1, token2, tokenN)
   */
  BUCKETS = (...token) => this.INDEX.BUCKETS(...token)

  /**
   * Get index creation date
   * @function CREATED
   * @memberOf SearchIndex
   * @instance
   * @returns {Promise<number>} creation date in milliseconds
   * @example
   * // Find out when index was first created
   * const timestamp = await CREATED()
   */
  CREATED = () => this.INDEX.CREATED()

  /**
   * Delete documents from index
   * @function DELETE
   * @memberOf SearchIndex
   * @instance
   * @param {...(string|number)} [ids] - A spread array of ids of documents to be deleted
   * @returns {Promise<Array>} success status
   * @example
   * const result = await DELETE(id1, id2, idN)
   */
  DELETE = (...id) => this.w.DELETE(...id)

  /**
   * Deletes raw documents from index, but not their indexed values. These documents will therefore still be retrievable, but the index only contains a reference to them. The document itself must be stored elsewhere.
   * @function DELETE_RAW
   * @memberOf SearchIndex
   * @instance
   * @param {...(string|number)} [ids] - A spread array of ids of raw documents to be deleted. Document references will still be retrievable.
   * @returns {Promise<Array>} success status
   * @example
   * const result = await DELETE_RAW(id1, id2, idN)
   */
  DELETE_RAW = (...id) => this.w.DELETE_RAW(...id)

  /**
   * Returns the complete list of words stored in the index for the given Token. Returns for the whole index if no token is specified. DISTINCT differs from DICTIONARY in that it returns Tokens rather than strings. This means that one word that appears in many different fields will be returned per field in DISTINCT with the fieldname, but only once in DICTIONARY
   * @function DICTIONARY
   * @memberOf SearchIndex
   * @instance
   * @param {SearchIndex.Token} [tokenSpace] - Index space to search in
   * @returns {Promise<Array.<string>>} list of words
   * @example
   * // Return each available word for the given token space
   * const words = await DICTIONARY(token)
   *
   * // Return all words in the index
   * const allWords = await DICTIONARY()
   */
  DICTIONARY = token => this.r.DICTIONARY(token)

  /**
   * Returns the complete list of words stored in the index for the given Token. Returns for the whole index if no token is specified. DISTINCT differs from DICTIONARY in that it returns Tokens rather than strings. This means that one word that appears in many different fields will be returned per field in DISTINCT with the fieldname, but only once in DICTIONARY
   * @function DISTINCT
   * @memberOf SearchIndex
   * @instance
   * @param {...SearchIndex.Token} [tokenSpace] - A spread array of index spaces to search in
   * @returns {Promise<Array.<SearchIndex.Token>>} list of words
   * @example
   * // Return each available field/word combination for the given token space
   * const distinct = await DISTINCT(token1, token2, token3)
   */
  DISTINCT = (...token) => this.r.DISTINCT(...token)

  // TODO: shouldn't DOCUMENTS take IDs and return whole documents?
  /**
   * Return named documents from index.
   * @function DOCUMENTS
   * @memberOf SearchIndex
   * @instance
   * @param {...(number | string)} [ids] - A spread array of document ids. If no ids are specified, DOCUMENTS will return all documents in index.
   * @returns {Promise<Array.<SearchIndex.Document>>} Array of Documents
   * @example
   * const result = await DOCUMENTS()
   */
  DOCUMENTS = (...docs) => this.r.DOCUMENTS(...docs)

  /**
   * Return total number of documents in index
   * @function DOCUMENT_COUNT
   * @memberOf SearchIndex
   * @instance
   * @returns {Promise<number>} Total number of documents in index
   * @example
   * const totalDocs = await DOCUMENT_COUNT()
   */
  DOCUMENT_COUNT = () => this.r.DOCUMENT_COUNT()

  /**
   * Return named document vectors from index. Seeing the generated document vectors can be a useful way to debug indexing.
   * @function DOCUMENT_VECTORS
   * @memberOf SearchIndex
   * @instance
   * @returns {Promise<Array.<object>>} An array of document vectors
   * @param {...(number | string)} [ids] - A spread array of document ids
   * @example
   * const documentVectors = await DOCUMENT_VECTORS(id1, id2, id3)
   */
  DOCUMENT_VECTORS = (...ids) => this.r.DOCUMENT_VECTORS(...ids)

  /**
   * creates a backup/export of an index
   * @function EXPORT
   * @memberOf SearchIndex
   * @instance
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
   * @memberOf SearchIndex
   * @instance
   * @param {...SearchIndex.Token} [tokenSpace] - A spread array of token spaces to create facets on.
   * @returns {Promise<Array.<SearchIndex.Bucket>>} Array of Buckets
   * @example
   * const facets = await FACETS(token)
   */
  FACETS = (...token) => this.r.FACETS(...token)

  /**
   * Return every document fieldname that has been indexed
   * @function FIELDS
   * @memberOf SearchIndex
   * @instance
   * @returns {Promise<Array>} An array of fields
   * @example
   * const fields = await FIELDS()
   */
  FIELDS = () => this.INDEX.FIELDS()

  /**
   * Delete everything and start again (including creation metadata)
   * @function FLUSH
   * @memberOf SearchIndex
   * @instance
   * @example
   * // I just want to watch the world burn 🔥
   * await FLUSH()
   */
  FLUSH = () => this.w.FLUSH()

  /**
   * creates an index from a backup/export
   * @function IMPORT
   * @memberOf SearchIndex
   * @instance
   * @param {object} [indexDump] a serialised index dump as per the one generated by `EXPORT`
   * @example
   * // having previously run:
   * const index = await anotherIndex.EXPORT()
   *
   * // you can them import like so:
   * await IMPORT(index)
   */
  IMPORT = index => this.INDEX.IMPORT(index)

  /**
   * find out when index was last updated
   * @function LAST_UPDATED
   * @memberOf SearchIndex
   * @instance
   * @returns {Promise<number>} Timestamp
   * @example
   * const timestamp = await LAST_UPDATED()
   */
  LAST_UPDATED = () => this.INDEX.LAST_UPDATED()

  /**
   * Get the maxiumum/last value of the given token space
   * @function MAX
   * @memberOf SearchIndex
   * @instance
   * @returns {(number|string)} The maxiumum/last value of the given token space
   * @param {SearchIndex.Token} [tokenSpace] - A token space
   * @example
   * const max = await MAX(token)
   */
  MAX = token => this.INDEX.MAX(token)

  /**
   * Get the miniumum/first value of the given token space
   * @function MIN
   * @memberOf SearchIndex
   * @instance
   * @returns {(number|string)} The miniumum/first value of the given token space
   * @param {SearchIndex.Token} [tokenSpace] - A token space
   * @example
   * const min = await MIN(token)
   */
  MIN = token => this.INDEX.MIN(token)

  /**
   * Put documents into the index
   * @function PUT
   * @memberOf SearchIndex
   * @instance
   * @returns {Array.<object>} success or otherwise of the insertion
   * @param {Array.<SearchIndex.Document>} [documents] - An array of documents to be indexed
   * @param {SearchIndex.PutOptions} [options] - The options
   * @example
   * const result = await PUT(documents, options)
   * // "result" shows the success or otherwise of the insertion
   * // "documents" is an Array of javascript Objects.
   * // "options" is an Object that contains indexing options
   * TODO TODO TODO
   */
  PUT = (docs, ops) => this.w.PUT(docs, ops)

  /**
   * Descriptioon here
   * @function PUT_RAW
   * @memberOf SearchIndex
   * @instance
   * @returns {} success or otherwise of the insertion
   * @param {Array.<object>} [documents] An array of documents to be indexed where each document contains an _id field corresponding to an existing document in the index
   * @example
   * const result = await PUT_RAW(rawDocuments)
   */
  // TODO: this API should surely be better
  PUT_RAW = (docs, ids, dontStoreValue) =>
    this.w.PUT_RAW(docs, ids, dontStoreValue)

  /**
   * Query the index
   * @function QUERY
   * @memberOf SearchIndex
   * @instance
   * @returns { ResultSet }
   * @param {Query} [query] - A query object
   * @param {QueryOptions} [options={QueryOptions}] - queryOptions
   * @example
   * const results = await QUERY(query, queryOptions)
   * // TODO TODO
   */
  QUERY = (q, ops) => this.r.QUERY(q, ops)

  /**
   * Search the index (equivalent to QUERY(q, { SCORE: 'TFIDF', SORT: true} ))
   * @function SEARCH
   * @memberOf SearchIndex
   * @instance
   * @returns {ResultSet}
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
