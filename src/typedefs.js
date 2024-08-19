/***************************************************************************
 * OPTIONS
 ***************************************************************************/

/**
 * Options for writing documents to the index
 *
 * @typedef SIPutOptions
 * @type {object}
 * @property {object} [ngrams={}] - Specify ngrams as per https://www.npmjs.com/package/ngraminator
 * @property {object} [replace={ { fields: [], values: {} } }] - replace tokens
 * @property {Array} [skipField=[]] - These fields will not be searchable, but they will still be stored
 * @property {boolean} [storeRawDocs=true] - Whether to store the raw document or not. In many cases it may be desirable to store it externally, or to skip storing when indexing if it is going to be updated directly later on
 * @property {boolean} [storeVectors=true] - Whether to store the raw document or not. In many cases it may be desirable to store it externally, or to skip storing when indexing if it is going to be updated directly later on
 * @property {Array} [tokenizationPipeline=[ SPLIT,  SKIP,  LOWCASE,  REPLACE,  NGRAMS,  STOPWORDS,  SCORE_TERM_FREQUENCY ]] - Tokenisation pipeline. Stages can be added and reordered
 * @property {RegExp} [tokenSplitRegex=/[\p{L}\d]+/gu] - The regular expression that splits strings into tokens
 */

/**
 * Options to initialise index with. Augments SIPutOptions, so all of those
 * options are also valid here
 *
 * @typedef {object} SIIndexOptions
 * @augments SIPutOptions
 * @property {number} [cacheLength=1000] - The length of the LRU cache. A higher value makes the index faster but uses more memory. Cache is emptied after each write.
 * @property {boolean} [caseSensitive=false] - Case sensitivity. Default is `false`. If true, `case` is preserved (so 'BaNaNa' != 'banana'), if `false`, text matching will not be case sensitive
 * @property {Level} [db=(BrowserLevel|ClassicLevel)] -  An abstract-level store. Defaults to BrowserLevel when compiled for web and ClassicLevel when compiled for node. You can also run `SearchIndex` on other key-value backends- see https://github.com/Level/abstract-level and https://github.com/Level/awesome for inspiration.
 * @property {string} [name=index] - Name of the index- will correspond to a physical folder on a filesystem (default for node) or a namespace in a database (default for web is indexedDB) depending on which backend you use
 * @property {Array} [stopwords=[]] - A list of words to be ignored when indexing and querying
 */

/**
 * Options for querying the index
 *
 * @typedef SIQueryOptions
 * @type {object}
 * @property {Array} [BUCKETS=[]] - Aggregate on user defined buckets.
 * @property {boolean} [DOCUMENTS=false] - If `true` return entire document, if not `true` return reference to document
 * @property {Array} [FACETS=[]] - Aggregate on user-defined facets
 * @property {object} [PAGE={ NUMBER: 0, SIZE: 20 }] - Pagination
 * @property {Promise} [PIPELINE=token => new Promise(resolve => resolve(token))] - Query tokenization pipeline
 * @property {String} [SCORE='TFIDF'] - Calculate a relevancy value per document
 * @property {object} [SORT={ TYPE: 'NUMERIC', DIRECTION: 'DESCENDING', FIELD: '_score' }] - Sort documents
 * @property {object} [WEIGHT=[]] - Weight fields and/or values
 */

/***************************************************************************
 * QUERIES
 ***************************************************************************/

/**
 * Specify a range of values.
 *
 * @typedef SIRange
 * @type {object}
 * @property {(string|number)} GTE - greater than or equal to
 * @property {(string|number)} LTE - less than or equal to
 */

/**
 * Represents a lexicographical space in the index.
 *
 * @typedef SIToken
 * @type {object}
 * @property {(string|string[])} FIELD - a field name, or array of names.
 * @property {(string|number|SIRange)} VALUE - a value or range of values.
 */

/***************************************************************************
 * RESULTS
 ***************************************************************************/

/**
 *
 * A set of document ids for a given token space.
 *
 * @typedef SIBucket
 * @type {object}
 * @property {string} FIELD - The field name
 * @property {SIRange} VALUE - The token range
 * @property {string[]} _id - An array of _ids of the documents that contain the specified token
 */

/**
 *
 * A document in the search index
 *
 * @typedef SIDocument
 * @type {object}
 * @property {(string|number)} _id
 * @property {object} _doc
 */
