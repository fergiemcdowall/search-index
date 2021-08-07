// TODO: remove all '￮' and '#'

module.exports = (ops, cache) => {
  const ALL_DOCUMENTS = limit =>
    new Promise((resolve, reject) => {
      const result = []
      ops.fii.STORE.createReadStream({
        // gte: '￮DOC_RAW￮',
        // lte: '￮DOC_RAW￮￮',
        gte: ['DOC_RAW', null],
        lte: ['DOC_RAW', undefined],
        limit: limit
      })
        .on('data', d =>
          result.push({
            _id: d.value._id,
            _doc: d.value
          })
        )
        .on('end', () => resolve(result))
    })

  const DOCUMENTS = (...requestedDocs) => {
    return requestedDocs.length
      ? Promise.all(
          requestedDocs.map(_id =>
            ops.fii.STORE.get(['DOC_RAW', _id]).catch(e => null)
          )
        )
      : ALL_DOCUMENTS()
  }

  const DICTIONARY = token =>
    DISTINCT(token).then(results =>
      Array.from(
        results.reduce((acc, cur) => acc.add(cur.VALUE), new Set())
      ).sort((a, b) =>
        // This should sort an array of strings and
        // numbers in an intuitive way (numbers numerically, strings
        // alphabetically)
        (a + '').localeCompare(b + '', undefined, {
          numeric: true,
          sensitivity: 'base'
        })
      )
    )

  const DISTINCT = (...tokens) =>
    ops.fii.DISTINCT(...tokens).then(result => {
      return [
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
    }) // un-stringify

  const FACETS = (...tokens) =>
    ops.fii.FACETS(...tokens).then(result =>
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

  const PAGE = (results, options) => {
    options = Object.assign(
      {
        NUMBER: 0,
        SIZE: 20
      },
      options || {}
    )
    const start = options.NUMBER * options.SIZE
    // handle end index correctly when (start + size) == 0
    // (when paging from the end with a negative page number)
    const end = start + options.SIZE || undefined
    return results.slice(start, end)
  }

  // score by tfidf by default
  // TODO: Total hits (length of _match)
  // TODO: better error handling: what if TYPE is 'XXXXX'
  const SCORE = (results, scoreOps = {}) => {
    const filterFields = item => {
      if (!scoreOps.FIELDS) return true
      return scoreOps.FIELDS.includes(item.FIELD)
    }

    scoreOps = Object.assign(
      {
        TYPE: 'TFIDF'
      },
      scoreOps
    )

    if (scoreOps.TYPE === 'TFIDF') {
      return DOCUMENT_COUNT().then(docCount =>
        results.map((result, _, resultSet) => {
          const idf = Math.log((docCount + 1) / resultSet.length)
          result._score = +result._match
            .filter(filterFields)
            .reduce((acc, cur) => acc + idf * +cur.SCORE, 0)
            .toFixed(2) // TODO: make precision an option
          return result
        })
      )
    }
    if (scoreOps.TYPE === 'PRODUCT') {
      return new Promise(resolve =>
        resolve(
          results.map(r => {
            r._score = +r._match
              .filter(filterFields)
              .reduce((acc, cur) => acc * +cur.SCORE, 1)
              .toFixed(2)
            // TODO: make precision an option
            return r
          })
        )
      )
    }
    if (scoreOps.TYPE === 'CONCAT') {
      return new Promise(resolve =>
        resolve(
          results.map(r => {
            r._score = r._match
              .filter(filterFields)
              .reduce((acc, cur) => acc + cur.SCORE, '')
            return r
          })
        )
      )
    }
    if (scoreOps.TYPE === 'SUM') {
      return new Promise(resolve =>
        resolve(
          results.map(r => {
            r._score = +r._match
              .filter(filterFields)
              .reduce((acc, cur) => acc + +cur.SCORE, 0)
              .toFixed(2) // TODO: make precision an option
            return r
          })
        )
      )
    }
    if (scoreOps.TYPE === 'VALUE') {
      return new Promise(resolve =>
        resolve(
          results.map(r => {
            r._score = r._match
              .filter(filterFields)
              .reduce((acc, cur) => acc + cur.VALUE, '')
            return r
          })
        )
      )
    }
  }

  // TODO: maybe add a default page size?
  const SEARCH = (q, qops) => {
    return parseJsonQuery(
      {
        AND: [...q]
      },
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
  }

  const SORT = (results, options) => {
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

  const DOCUMENT_COUNT = () => ops.fii.STORE.get(['DOCUMENT_COUNT'])

  const WEIGHT = (results, weights) =>
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

  // This function reads queries in a JSON format and then translates them to
  // Promises
  const parseJsonQuery = (q, options = {}) => {
    const runQuery = cmd => {
      // if string or object with only FIELD or VALUE, assume
      // that this is a GET
      if (typeof cmd === 'string' || typeof cmd === 'number') { return ops.fii.GET(cmd, options.pipeline) }
      if (cmd.FIELD) return ops.fii.GET(cmd)
      if (cmd.VALUE) return ops.fii.GET(cmd)

      // TODO: Find a clever way to pass options to AND, GET, NOT and OR
      // Probably something along the lines of- if e.g. AND, and if
      // one condition is an object, then that condition is an options object

      // else:
      if (cmd.AND) return ops.fii.AND(cmd.AND.map(runQuery), options.pipeline)
      if (cmd.GET) return ops.fii.GET(cmd.GET, options.pipeline)
      if (cmd.NOT) { return ops.fii.NOT(runQuery(cmd.NOT.INCLUDE), runQuery(cmd.NOT.EXCLUDE)) }
      if (cmd.OR) return ops.fii.OR(cmd.OR.map(runQuery), options.pipeline)

      if (cmd.DOCUMENTS) return DOCUMENTS(...cmd.DOCUMENTS)
    }

    const formatResults = result =>
      result.RESULT
        ? Object.assign(result, {
            RESULT_LENGTH: result.RESULT.length
          })
        : {
            RESULT: result,
            RESULT_LENGTH: result.length
          }

    // APPEND DOCUMENTS IF SPECIFIED
    const appendDocuments = result =>
      options.DOCUMENTS
        ? DOCUMENTS(...result.RESULT.map(doc => doc._id)).then(documents =>
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
        ? SCORE(result.RESULT, options.SCORE).then(scoredResult =>
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
              RESULT: SORT(result.RESULT, options.SORT)
            }
          : {}
      )

    // BUCKETS IF SPECIFIED
    const buckets = result =>
      options.BUCKETS
        ? ops.fii.BUCKETS(...options.BUCKETS).then(bkts =>
            Object.assign(result, {
              BUCKETS: ops.fii.AGGREGATION_FILTER(bkts, result.RESULT)
            })
          )
        : result

    // FACETS IF SPECIFIED
    const facets = result =>
      options.FACETS
        ? result.RESULT.length
            ? FACETS(...options.FACETS).then(fcts =>
                Object.assign(result, {
                  FACETS: ops.fii.AGGREGATION_FILTER(fcts, result.RESULT)
                })
              )
            : Object.assign(result, {
              FACETS: [] // if empty result set then just return empty facets
            })
        : result

    // PAGE IF SPECIFIED
    const page = result =>
      Object.assign(
        result,
        options.PAGE ? { RESULT: PAGE(result.RESULT, options.PAGE) } : {}
      )

    // WEIGHT IF SPECIFIED
    const weight = result =>
      options.WEIGHT
        ? Object.assign(
            { RESULT: WEIGHT(result.RESULT, options.WEIGHT) },
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

  const tryCache = (q, cacheKey) =>
    new Promise(resolve => {
      cacheKey = JSON.stringify(cacheKey)
      return cache.has(cacheKey)
        ? resolve(cache.get(cacheKey))
        : q
          .then(res => cache.set(cacheKey, res))
          .then(() => resolve(cache.get(cacheKey)))
    })

  return {
    ALL_DOCUMENTS: ALL_DOCUMENTS,
    DICTIONARY: token =>
      tryCache(DICTIONARY(token), { DICTIONARY: token || null }),
    DISTINCT: DISTINCT,
    DOCUMENTS: (...docs) =>
      tryCache(DOCUMENTS(...docs), {
        DOCUMENTS: docs
      }),
    DOCUMENT_COUNT: DOCUMENT_COUNT,
    FACETS: FACETS,
    PAGE: PAGE,
    QUERY: (q, qops) => tryCache(parseJsonQuery(q, qops), { QUERY: [q, qops] }),
    SCORE: SCORE,
    SEARCH: (q, qops) => tryCache(SEARCH(q, qops), { SEARCH: [q, qops] }),
    SORT: SORT
  }
}
