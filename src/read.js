export default function (fii) {
  const BUCKETS = (...buckets) => Promise.all(
    buckets.map(fii.BUCKET)
  )

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
        var result = []
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
  const SCORE = (results, type) => {
    type = type || 'TFIDF' // default
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
      'NUMERIC': {
        'DESCENDING': (a, b) => +deepRef(b) - +deepRef(a),
        'ASCENDING': (a, b) => +deepRef(a) - +deepRef(b)
      },
      'ALPHABETIC': {
        'DESCENDING': (a, b) => {
          if (deepRef(a) < deepRef(b)) return 1
          if (deepRef(a) > deepRef(b)) return -1
          return 0
        },
        'ASCENDING': (a, b) => {
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
  const parseJsonQuery = (q, options) => {
    options = options || {}

    var promisifyQuery = (command) => {
      // if string or object with only FIELD or VALUE, assume
      // that this is a GET
      if (typeof command === 'string') return fii.GET(command)
      if (command.FIELD) return fii.GET(command)
      if (command.VALUE) return fii.GET(command)

      // else:

      if (command.AGGREGATE) {
        return fii.AGGREGATE({
          BUCKETS: command.AGGREGATE.BUCKETS
            ? fii.BUCKETS(...command.AGGREGATE.BUCKETS)
            : [],
          FACETS: command.AGGREGATE.FACETS
            ? FACETS(command.AGGREGATE.FACETS)
            : [],
          QUERY: promisifyQuery(command.AGGREGATE.QUERY)
        })
      }
      if (command.AND) return fii.AND(...command.AND.map(promisifyQuery))
      if (command.BUCKETS) return BUCKETS(...command.BUCKETS)
      if (command.DISTINCT) return DISTINCT(...command.DISTINCT)
      // TODO: documents should be a QUERY option
      // feed in preceding results if present (ie if not first promise)
      if (command.DOCUMENTS) return DOCUMENTS(command.DOCUMENTS)
      if (command.FACETS) return FACETS(...command.FACETS)
      //      if (command.FIELDS) return fii.FIELDS() // TODO: own function
      if (command.GET) return fii.GET(command.GET)
      // if (command.MAX) return fii.MAX(command.MAX) // TODO: own function
      // if (command.MIN) return fii.MIN(command.MIN) // TODO: own function
      if (command.NOT) {
        return fii.SET_SUBTRACTION(
          promisifyQuery(command.NOT.INCLUDE),
          promisifyQuery(command.NOT.EXCLUDE)
        )
      }
      if (command.OR) return fii.OR(...command.OR.map(promisifyQuery))
      //      if (command.SCORE) return SCORE(resultFromPreceding, command.SCORE)
      if (command.SEARCH) return SEARCH(...command.SEARCH.map(promisifyQuery))
      // TODO: make into an option
      //      if (command.SORT) return SORT(resultFromPreceding, command.SORT)
    }

    return promisifyQuery(q).then(
      result => options.DOCUMENTS ? DOCUMENTS(result) : result
    ).then(
      result => options.SCORE ? SCORE(result, options.SCORE) : result
    ).then(
      result => options.SORT ? SORT(result, options.SORT) : result
    ).then(
      result => options.PAGE ? PAGE(result, options.PAGE) : result
    )
  }

  return {
    BUCKETS: BUCKETS,
    DICTIONARY: DICTIONARY,
    DISTINCT: DISTINCT,
    DOCUMENTS: DOCUMENTS,
    DOCUMENT_COUNT: DOCUMENT_COUNT,
    FACETS: FACETS,
    FIELDS: fii.FIELDS,
    PAGE: PAGE,
    SCORE: SCORE,
    SEARCH: SEARCH,
    SORT: SORT,
    QUERY: parseJsonQuery
  }
}
