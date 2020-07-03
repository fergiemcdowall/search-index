import { getAvailableFields, getRange, getDocCount } from './indexUtils.js'

export default function (fii) {

  // TODO: rewrite this to read in an array of DISTINCT queries
  // DICTIONARY: [{
  //   field: 'bla1'
  // }, {
  //   field: 'bla2'
  // }]

  // DICTIONARY: true // <- should return all tokens
                      // or maybe it should be possible to do a fieldless DISTINCT
  
  const DICTIONARY = q => new Promise((resolve) => {
    const flatten = arr => [].concat.apply([], arr)
    // if query is string convert to object
    // if no query, make empty query
    q = Object.assign(
      { gte: '', lte: '￮' },
      (typeof q === 'string') ? { gte: q, lte: q + '￮' } : q
    )

    // options, defaults
    q.options = Object.assign({
      withFieldName: false
    }, q.options || {})

    return resolve(
      new Promise(resolve => resolve(q.fields || getAvailableFields(fii)))
        .then(fields => Promise.all(
          fields.map(field => getRange(fii, {
            gte: field + ':' + q.gte,
            lte: field + ':' + q.lte + '￮'
          }))
        ))
        .then(flatten)
      //        .then(res => {console.log(res); return res})
        .then(tokens => tokens.map(t => (
          q.options.withFieldName
            ? t.split('#').shift()
            : t.split(':').pop().split('#').shift()
        )))
        .then(tokens => tokens.sort())
        .then(tokens => [...new Set(tokens)])
    )
  })

  const DOCUMENTS = requestedDocs => {
    // Either return document per id
    if (Array.isArray(requestedDocs)) {
      return Promise.all(
        requestedDocs.map(
          doc => fii.STORE.get('￮DOC_RAW￮' + doc._id + '￮')
            .catch(e => null)
        )
      ).then(returnedDocs => requestedDocs.map((rd, i) => {
        rd._doc = returnedDocs[i]
        return rd
      }))
    }
    // or just dump out all docs
    // TODO should share getRange in indexUtils.js?
    return new Promise((resolve, reject) => {
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

  const SEARCH = (...q) => fii
    .AND(...q)
    .then(SCORE)
    .then(SORT)

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
  // TODO: should also be an option to score by field
  // TODO: Total hits (length of _match)
  const SCORE = (results, type) => {
    type = type || 'TFIDF' // default
    if (type === 'TFIDF') {
      return getDocCount(fii).then(
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
      if (path[0] === '_match') {
        return obj._match.find(
          _match => (path.slice(1).join('.') === _match.split(':')[0])
        ).split(':')[1].split('#')[0]
      }
      return path.reduce((o, i) => o[i], obj)
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

  const DISTINCT = term => fii.DISTINCT(term).then(result => [
    ...result.reduce((acc, cur) => {
      cur.VALUE = cur.VALUE.split('#')[0]
      acc.add(JSON.stringify(cur))
      return acc
    }, new Set())
  ].map(JSON.parse))

  // This function reads queries in a JSON format and then translates them to
  // Promises
  const parseJsonQuery = (...q) => {
    // needs to be called with "command" and result from previous "thenable"
    var promisifyQuery = (command, resultFromPreceding) => {
      if (typeof command === 'string') return fii.GET(command)
      if (command.AND) return fii.AND(...command.AND.map(promisifyQuery))
      if (command.BUCKETFILTER) {
        if (command.BUCKETFILTER.BUCKETS.DISTINCT) {
          return fii.BUCKETFILTER(
            DISTINCT(command.BUCKETFILTER.BUCKETS.DISTINCT)
              .then(bkts => bkts.map(fii.BUCKET)),
            promisifyQuery(command.BUCKETFILTER.FILTER)
          )
        } else {
          return fii.BUCKETFILTER(
            command.BUCKETFILTER.BUCKETS.map(fii.BUCKET),
            promisifyQuery(command.BUCKETFILTER.FILTER)
          )
        }
      }
      // feed in preceding results if present (ie if not first promise)
      if (command.BUCKET) return fii.BUCKET(resultFromPreceding || command.BUCKET)
      if (command.DICTIONARY) return DICTIONARY(command.DICTIONARY)
      if (command.DISTINCT) return DISTINCT(command.DISTINCT)
      // feed in preceding results if present (ie if not first promise)
      if (command.DOCUMENTS) return DOCUMENTS(resultFromPreceding || command.DOCUMENTS)
      if (command.GET) return fii.GET(command.GET)
      if (command.NOT) {
        return fii.SET_SUBTRACTION(
          promisifyQuery(command.NOT.INCLUDE),
          promisifyQuery(command.NOT.EXCLUDE)
        )
      }
      if (command.OR) return fii.OR(...command.OR.map(promisifyQuery))
      if (command.PAGE) return PAGE(resultFromPreceding, command.PAGE)
      if (command.SCORE) return SCORE(resultFromPreceding, command.SCORE)
      if (command.SEARCH) return SEARCH(...command.SEARCH.map(promisifyQuery))
      if (command.SORT) return SORT(resultFromPreceding, command.SORT)
    }
    // Turn the array of commands into a chain of promises
    return q.reduce((acc, cur) => acc.then(
      result => promisifyQuery(cur, result)
    ), promisifyQuery(q.shift())) // <- Separate the first promise in the chain
    //                                  to be used as the start point in .reduce
  }

  return {
    AND: fii.AND,
    BUCKET: fii.BUCKET,
    BUCKETFILTER: fii.BUCKETFILTER,
    DICTIONARY: DICTIONARY,
    DISTINCT: DISTINCT,
    DOCUMENTS: DOCUMENTS,
    GET: fii.GET,
    OR: fii.OR,
    PAGE: PAGE,
    SCORE: SCORE,
    SEARCH: SEARCH,
    SET_SUBTRACTION: fii.SET_SUBTRACTION,
    SORT: SORT,
    parseJsonQuery: parseJsonQuery
  }
}
