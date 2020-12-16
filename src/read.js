module.exports = fii => {
  const DOCUMENTS = requestedDocs => {
    // Either return document per id
    return Array.isArray(requestedDocs)
      ? Promise.all(
          requestedDocs.map(
            doc => fii.STORE.get('￮DOC_RAW￮' + doc._id + '￮').catch(e => null)
          )
        ).then(returnedDocs => requestedDocs.map(
          (rd, i) => Object.assign(rd, { _doc: returnedDocs[i] })
        ))
      : new Promise((resolve, reject) => {
        const result = []
        fii.STORE.createReadStream({
          gte: '￮DOC_RAW￮',
          lte: '￮DOC_RAW￮￮'
        }).on('data', d => result.push({
          _id: d.value._id,
          _doc: d.value
        })).on('end', () => resolve(result))
      })
  }

  const DICTIONARY = token => DISTINCT(token).then(results =>
    Array.from(results.reduce(
      (acc, cur) => acc.add(cur.VALUE), new Set())
    ).sort()
  )

  const DISTINCT = (...tokens) => fii.DISTINCT(
    ...tokens
  ).then(result => [
    // Stringify Set entries so that Set can determine duplicates
    ...result.reduce((acc, cur) => acc.add(JSON.stringify(
      Object.assign(cur, {
        VALUE: cur.VALUE.split('#')[0]
      })
    )), new Set())
  ].map(JSON.parse)) // un-stringify

  const FACETS = (...tokens) => fii.FACETS(
    ...tokens
  ).then(result => [
    // Stringify Set entries so that Set can determine duplicates
    ...result.reduce((acc, cur) => acc.add(JSON.stringify(
      Object.assign(cur, {
        VALUE: cur.VALUE.split('#')[0]
      })
    )), new Set())
  ].map(JSON.parse)) // un-stringify

  const PAGE = (results, options) => {
    options = Object.assign({
      NUMBER: 0,
      SIZE: 20
    }, options || {})
    const start = options.NUMBER * options.SIZE
    // handle end index correctly when (start + size) == 0
    // (when paging from the end with a negative page number)
    const end = (start + options.SIZE) || undefined
    return results.slice(start, end)
  }

  // score by tfidf by default
  // TODO: Total hits (length of _match)
  const SCORE = (results, type = 'TFIDF') => {
    if (type === 'TFIDF') {
      return DOCUMENT_COUNT().then(
        docCount => results.map((x, _, resultSet) => {
          const idf = Math.log((docCount + 1) / resultSet.length)
          x._score = +x._match.reduce(
            (acc, cur) => acc + idf * +cur.split('#')[1], 0
          ).toFixed(2) // TODO: make precision an option
          return x
        })
      )
    }
    if (type === 'PRODUCT') {
      return new Promise(resolve => resolve(
        results.map(r => {
          r._score = +r._match.reduce(
            (acc, cur) => acc * +cur.split('#')[1], 1
          ).toFixed(2) // TODO: make precision an option
          return r
        })
      ))
    }
    if (type === 'CONCAT') {
      return new Promise(resolve => resolve(
        results.map(r => {
          r._score = r._match.reduce(
            (acc, cur) => acc + cur.split('#')[1], ''
          )
          return r
        })
      ))
    }
    if (type === 'SUM') {
      return new Promise(resolve => resolve(
        results.map(r => {
          r._score = +r._match.reduce(
            (acc, cur) => acc + +cur.split('#')[1], 0
          ).toFixed(2) // TODO: make precision an option
          return r
        })
      ))
    }
  }

  const SEARCH = (...q) => fii
    .AND(...q)
    .then(SCORE)
    .then(SORT)

  const SORT = (results, options) => {
    options = Object.assign({
      DIRECTION: 'DESCENDING',
      FIELD: '_score',
      TYPE: 'NUMERIC'
    }, options || {})
    const deepRef = obj => {
      const path = options.FIELD.split('.')
      // TODO: dont like doing it this way- there should probably be a
      // way to dump the literal field value into _score, and always
      // sort on _score
      //
      // That said, it should be possible to score without fetching
      // the whole document for each _id
      //
      // special case: sorting on _match so that you dont have to
      // fetch all the documents before doing a sort
      return (path[0] === '_match')
        ? (obj._match.find(
            _match => (path.slice(1).join('.') === _match.split(':')[0])
          // gracefully fail if field name not found
          ) || ':#').split(':')[1].split('#')[0]
        : path.reduce((o, i) => o[i], obj)
    }
    const sortFunction = {
      NUMERIC: {
        DESCENDING: (a, b) => +deepRef(b) - +deepRef(a),
        ASCENDING: (a, b) => +deepRef(a) - +deepRef(b)
      },
      ALPHABETIC: {
        DESCENDING: (a, b) => {
          if (deepRef(a) < deepRef(b)) return 1
          if (deepRef(a) > deepRef(b)) return -1
          return 0
        },
        ASCENDING: (a, b) => {
          if (deepRef(a) < deepRef(b)) return -1
          if (deepRef(a) > deepRef(b)) return 1
          return 0
        }
      }
    }
    return results.sort(sortFunction[options.TYPE][options.DIRECTION])
  }

  const DOCUMENT_COUNT = () => fii.STORE.get('￮DOCUMENT_COUNT￮')

  // This function reads queries in a JSON format and then translates them to
  // Promises
  const parseJsonQuery = (q, options = {}) => {
    const runQuery = cmd => {
      // if string or object with only FIELD or VALUE, assume
      // that this is a GET
      if (typeof cmd === 'string') return fii.GET(cmd)
      if (cmd.FIELD) return fii.GET(cmd)
      if (cmd.VALUE) return fii.GET(cmd)

      // else:
      if (cmd.AND) return fii.AND(...cmd.AND.map(runQuery))
      if (cmd.DOCUMENTS) return DOCUMENTS(cmd.DOCUMENTS)
      if (cmd.GET) return fii.GET(cmd.GET)
      if (cmd.NOT) {
        return fii.SET_SUBTRACTION(
          runQuery(cmd.NOT.INCLUDE),
          runQuery(cmd.NOT.EXCLUDE)
        )
      }
      if (cmd.OR) return fii.OR(...cmd.OR.map(runQuery))
      if (cmd.SEARCH) return SEARCH(...cmd.SEARCH.map(runQuery))
    }

    const formatResults = result => result.RESULT
      ? Object.assign(result, {
          RESULT_LENGTH: result.RESULT.length
        })
      : ({
          RESULT: result,
          RESULT_LENGTH: result.length
        })

    // APPEND DOCUMENTS IF SPECIFIED
    const appendDocuments = result => options.DOCUMENTS
      ? DOCUMENTS(result.RESULT).then(
          documentedResult => Object.assign(result, {
            RESULT: documentedResult
          }))
      : result

    // SCORE IF SPECIFIED
    const score = result => options.SCORE
      ? SCORE(result.RESULT, options.SCORE).then(
          scoredResult => Object.assign(result, {
            RESULT: scoredResult
          }))
      : result

    // SORT IF SPECIFIED
    const sort = result => Object.assign(
      result, options.SORT
        ? { RESULT: SORT(result.RESULT, options.SORT) }
        : {}
    )

    // BUCKETS IF SPECIFIED
    const buckets = result => options.BUCKETS
      ? fii.BUCKETS(...options.BUCKETS).then(bkts => Object.assign(
          result, {
            BUCKETS: fii.AGGREGATION_FILTER(bkts, result.RESULT)
          }))
      : result

    // FACETS IF SPECIFIED
    const facets = result => options.FACETS
      ? (result.RESULT.length)
          ? FACETS(...options.FACETS).then(fcts => Object.assign(
              result, {
                FACETS: fii.AGGREGATION_FILTER(fcts, result.RESULT)
              }))
    // if empty result set then just return empty facets
          : Object.assign(
            result, {
              FACETS: []
            })
      : result

    // PAGE IF SPECIFIED
    const page = result => Object.assign(
      result, options.PAGE
        ? { RESULT: PAGE(result.RESULT, options.PAGE) }
        : {}
    )

    return runQuery(q)
      .then(formatResults)
      .then(appendDocuments).then(score)
      .then(sort)
      .then(buckets)
      .then(facets)
      .then(page)
  }

  return {
    DICTIONARY: DICTIONARY,
    DISTINCT: DISTINCT,
    DOCUMENTS: DOCUMENTS,
    DOCUMENT_COUNT: DOCUMENT_COUNT,
    FACETS: FACETS,
    PAGE: PAGE,
    SCORE: SCORE,
    SEARCH: SEARCH,
    SORT: SORT,
    QUERY: parseJsonQuery
  }
}
