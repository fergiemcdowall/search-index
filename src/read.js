export class Reader {
  #cache
  #docExistsSpace
  #ii

  constructor (ops, cache, ii) {
    this.#docExistsSpace = ops.docExistsSpace
    this.#ii = ii
    this.#cache = cache
  }

  // This function reads queries in a JSON format and then translates them to
  // Promises
  #parseJsonQuery = (q, options = {}) => {
    const runQuery = cmd => {
      // if string or object with only FIELD or VALUE, assume
      // that this is a GET
      if (typeof cmd === 'string' || typeof cmd === 'number') {
        return this.#ii.GET(cmd, options.PIPELINE)
      }
      if (cmd.FIELD) return this.#ii.GET(cmd)
      if (cmd.VALUE) return this.#ii.GET(cmd)

      // else:
      if (cmd.AND) {
        return this.#ii.AND(cmd.AND.map(runQuery), options.PIPELINE)
      }
      if (cmd.GET) return this.#ii.GET(cmd.GET, options.PIPELINE)
      if (cmd.NOT) {
        return this.#ii.NOT(
          runQuery(cmd.NOT.INCLUDE),
          runQuery(cmd.NOT.EXCLUDE)
        )
      }
      if (cmd.OR) return this.#ii.OR(cmd.OR.map(runQuery), options.PIPELINE)

      // TODO this should be ALL_DOCUMENTS, such that
      // ALL_DOCUMENTS=true returns everything (needs test)
      // It should be possible to combine ALL_DOCUMENTS with FACETS
      // and other aggregations
      if (cmd.ALL_DOCUMENTS) return this.ALL_DOCUMENTS(cmd.ALL_DOCUMENTS)
    }

    const formatResults = result =>
      result.RESULT
        ? Object.assign(result, {
          QUERY: { q, options },
          RESULT_LENGTH: result.RESULT.length
        })
        : {
            QUERY: q,
            OPTIONS: options,
            RESULT_LENGTH: result.length,
            RESULT: result
          }

    // APPEND DOCUMENTS IF SPECIFIED
    const appendDocuments = result =>
      options.DOCUMENTS
        ? this.DOCUMENTS(...result.RESULT.map(doc => doc._id)).then(documents =>
          Object.assign(result, {
            RESULT: result.RESULT.map((doc, i) =>
              Object.assign(doc, {
                _doc: documents[i]
              })
            )
          })
        )
        : result

    // SCORE IF SPECIFIED
    const score = result =>
      options.SCORE
        ? this.SCORE(result.RESULT, options.SCORE).then(scoredResult =>
          Object.assign(result, {
            RESULT: scoredResult
          })
        )
        : result

    // SORT IF SPECIFIED
    const sort = result =>
      Object.assign(
        result,
        options.SORT
          ? {
              // TODO: should this be async?
              RESULT: this.SORT(result.RESULT, options.SORT)
            }
          : {}
      )

    // BUCKETS IF SPECIFIED
    const buckets = result =>
      options.BUCKETS
        ? this.#ii.BUCKETS(...options.BUCKETS).then(bkts =>
          Object.assign(result, {
            BUCKETS: this.#ii.AGGREGATION_FILTER(bkts, result.RESULT, false)
          })
        )
        : result

    // FACETS IF SPECIFIED
    // TODO: FAST OPTION FOR WHEN ALL_DOCUMENTS IS SPECIFIED
    // TODO: This should be 3 cases: 1. needs filter, 2. no results,
    // 3. no need for filter
    const facets = result => {
      // no FACETS are specified
      if (!options.FACETS) return result

      // QUERY returned no results, and facets will therefore be empty
      if (!result.RESULT.length) {
        return Object.assign(result, {
          FACETS: [] // if empty result set then just return empty facets
        })
      }

      // ALL_DOCUMENTS so no need to filter the facets
      if (q.ALL_DOCUMENTS) {
        return this.FACETS(...options.FACETS).then(fcts =>
          Object.assign(result, {
            FACETS: fcts
          })
        )
      }

      // else
      return this.FACETS(...options.FACETS).then(fcts =>
        Object.assign(result, {
          FACETS: this.#ii.AGGREGATION_FILTER(fcts, result.RESULT)
        })
      )
    }

    // PAGE IF SPECIFIED
    const page = result => {
      let pageDetails
      return Object.assign(
        result,
        // TODO: surely options.PAGE is always set with default values?
        {
          RESULT: this.PAGE(result.RESULT, options.PAGE, pd => {
            pageDetails = pd
          })
        },
        { PAGING: pageDetails }
      )
    }
    // WEIGHT IF SPECIFIED
    const weight = result =>
      options.WEIGHT
        ? Object.assign(
          { RESULT: this.WEIGHT(result.RESULT, options.WEIGHT) },
          result
        )
        : result

    return runQuery(q)
      .then(formatResults)
      .then(buckets)
      .then(facets)
      .then(weight)
      .then(score)
      .then(sort)
      .then(page)
      .then(appendDocuments)
  }

  #DICTIONARY = (token, options = {}) =>
    this.DISTINCT(token).then(results => ({
      RESULT: Array.from(
        results.reduce((acc, cur) => acc.add(cur.VALUE), new Set())
      ).sort((a, b) =>
        // This should sort an array of strings and
        // numbers in an intuitive way (numbers numerically, strings
        // alphabetically)
        (a + '').localeCompare(b + '', undefined, {
          numeric: true,
          sensitivity: 'base'
        })
      ),
      OPTIONS: options
    }))

  #DOCUMENTS = (...requestedDocs) =>
    requestedDocs.length
      ? Promise.all(
        requestedDocs.map(_id =>
          this.#ii.STORE.get([this.#docExistsSpace, _id]).catch(e => null)
        )
      )
      : this.ALL_DOCUMENTS()

  // TODO: maybe add a default page size?
  #SEARCH = (q, qops) =>
    this.#parseJsonQuery(
      {
        AND: [...q]
      },
      // TODO: destructure instead of Object.assign
      Object.assign(
        {
          SCORE: {
            TYPE: 'TFIDF'
          },
          SORT: true
        },
        qops
      )
    )

  // TODO add aggregation to ALL_DOCUMENTS
  ALL_DOCUMENTS = limit =>
    this.#ii.STORE.iterator({
      gte: [this.#docExistsSpace, null],
      lte: [this.#docExistsSpace, undefined],
      limit
    })
      .all()
      .then(entries =>
        entries.map(([key, value]) => ({
          _id: value._id,
          _doc: value
        }))
      )

  cachePipeline = (func, funcLabel, ...params) => {
    const cacheKey = JSON.stringify({
      funcLabel,
      params
    })
    return this.#cache.has(cacheKey)
      ? Promise.resolve(this.#cache.get(cacheKey))
      : func(...params).then(res => {
        this.#cache.set(cacheKey, res)
        return res
      })
  }

  DICTIONARY = (token, dops) =>
    this.cachePipeline(this.#DICTIONARY, '#DICTIONARY', token, dops)

  DISTINCT = (...tokens) =>
    this.#ii.DISTINCT(...tokens).then(result =>
      [
        // Stringify Set entries so that Set can determine duplicates
        ...result.reduce(
          (acc, cur) =>
            acc.add(
              JSON.stringify(
                Object.assign(cur, {
                  VALUE: cur.VALUE
                })
              )
            ),
          new Set()
        )
      ].map(JSON.parse)
    ) // un-stringify

  DOCUMENTS = (...docs) =>
    this.cachePipeline(this.#DOCUMENTS, '#DOCUMENTS', ...docs)

  DOCUMENT_COUNT = () => this.#ii.STORE.get(['DOCUMENT_COUNT'])

  DOCUMENT_VECTORS = (...requestedDocs) =>
    Promise.all(
      requestedDocs.map(_id =>
        this.#ii.STORE.get(['DOC', _id]).catch(e => null)
      )
    )

  FACETS = (...tokens) =>
    this.#ii.FACETS(...tokens).then(result =>
      [
        // Stringify Set entries so that Set can determine duplicates
        ...result.reduce(
          (acc, cur) =>
            acc.add(
              JSON.stringify(
                Object.assign(cur, {
                  // VALUE: cur.VALUE.split('#')[0] // TODO: this is wrong
                  VALUE: cur.VALUE
                })
              )
            ),
          new Set()
        )
      ].map(JSON.parse)
    ) // un-stringify

  PAGE = (results, options = {}, callback = () => null) => {
    options = {
      // TODO: 'NUMBER' should probably be renamed 'PAGE_OFFSET'
      NUMBER: 0,
      SIZE: 20,
      ...options
    }

    options.TOTAL = Math.ceil(results.length / options.SIZE)
    options.DOC_OFFSET = options.NUMBER * options.SIZE

    callback(options)

    return results.slice(
      options.DOC_OFFSET,
      // (when paging from the end with a negative page number)
      // handle end index correctly when (start + size) == 0
      options.DOC_OFFSET + options.SIZE || undefined
    )
  }

  QUERY = (q, qops) =>
    this.cachePipeline(this.#parseJsonQuery, '#parseJsonQuery', q, qops)

  // score by tfidf by default
  // TODO: Total hits (length of _match)
  // TODO: better error handling: what if TYPE is 'XXXXX'
  // TODO: scoring precision (decimal places) should be an option
  SCORE = (results, scoreOps = {}) => {
    // TODO: test for defaulting to TFIDF
    scoreOps = Object.assign(
      {
        TYPE: 'TFIDF'
      },
      scoreOps
    )

    const filterFields = item =>
      !scoreOps.FIELDS ? true : scoreOps.FIELDS.includes(item.FIELD)

    const filterMatch = _match => (_match || []).filter(filterFields)

    return new Promise(resolve =>
      resolve(
        scoreOps.TYPE === 'TFIDF'
          ? this.DOCUMENT_COUNT().then(docCount =>
            results.map((result, _, resultSet) => {
              const idf = Math.log((docCount + 1) / resultSet.length)
              result._score = +(result._match || [])
                .filter(filterFields)
                .reduce((acc, cur) => acc + idf * +cur.SCORE, 0)
              // TODO: make precision an option
                .toFixed(2)
              return result
            })
          )
          : scoreOps.TYPE === 'PRODUCT'
            ? results.map(r => ({
              ...r,
              _score: +filterMatch(r._match)
                .reduce((acc, cur) => acc * +cur.SCORE, 1)
                .toFixed(2)
            }))
            : scoreOps.TYPE === 'CONCAT'
              ? results.map(r => ({
                ...r,
                _score: filterMatch(r._match).reduce(
                  (acc, cur) => acc + cur.SCORE,
                  ''
                )
              }))
              : scoreOps.TYPE === 'SUM'
                ? results.map(r => ({
                  ...r,
                  _score: +filterMatch(r._match)
                    .reduce((acc, cur) => acc + +cur.SCORE, 0)
                    .toFixed(2) // TODO: make precision an option
                }))
                : scoreOps.TYPE === 'VALUE'
                  ? results.map(r => ({
                    ...r,
                    _score: filterMatch(r._match).reduce(
                      (acc, cur) => acc + cur.VALUE,
                      ''
                    )
                  }))
                  : null
      )
    )
  }

  SEARCH = (q, qops) => this.cachePipeline(this.#SEARCH, '#SEARCH', q, qops)

  SORT = (results, options) => {
    options = Object.assign(
      {
        DIRECTION: 'DESCENDING',
        TYPE: 'NUMERIC'
      },
      options || {}
    )
    const sortFunction = {
      NUMERIC: {
        DESCENDING: (a, b) => +b._score - +a._score,
        ASCENDING: (a, b) => +a._score - +b._score
      },
      ALPHABETIC: {
        DESCENDING: (a, b) => {
          if (a._score < b._score) return 1
          if (a._score > b._score) return -1
          return 0
        },
        ASCENDING: (a, b) => {
          if (a._score < b._score) return -1
          if (a._score > b._score) return 1
          return 0
        }
      }
    }

    return results
      .sort((a, b) => {
        if (a._id < b._id) return -1
        if (a._id > b._id) return 1
        return 0
      })
      .sort(sortFunction[options.TYPE][options.DIRECTION])
  }

  WEIGHT = (results, weights) =>
    results.map(r => {
      r._match = r._match.map(m => {
        weights.forEach(w => {
          let doWeighting = false
          // TODO: possible bug / edge case- does this work when weighting a field with value 0?
          if (w.FIELD && w.VALUE) {
            if (w.FIELD === m.FIELD && w.VALUE === m.VALUE) {
              doWeighting = true
            }
          } else if (w.FIELD) {
            if (w.FIELD === m.FIELD) {
              doWeighting = true
            }
          } else if (w.VALUE) {
            if (w.VALUE === m.VALUE) {
              doWeighting = true
            }
          }
          if (doWeighting) m.SCORE = (w.WEIGHT * +m.SCORE).toFixed(2)
        })
        return m
      })
      return r
    })
}
