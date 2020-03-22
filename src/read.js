import { TFIDF, numericField } from './scorers.js'
import { getAvailableFields, getRange } from './indexUtils.js'

export default function (fii) {
  const flatten = arr => [].concat.apply([], arr)

  const flattenMatch = result => result.map(x => {
    x._match = flatten(x._match) // flatten
    x._match = flatten(x._match) // flatten again
    return x
  })

  const DICTIONARY = q => new Promise((resolve) => {
    // if query is string convert to object
    // if no query, make empty query
    q = Object.assign(
      { gte: '', lte: '￮' },
      (typeof q === 'string') ? { gte: q, lte: q + '￮' } : q
    )
    
    // options, defaults
    q.options = Object.assign({
      withFieldName:false
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
          q.options.withFieldName ?
          t.split('#').shift() :
          t.split(':').pop().split('#').shift()
        )))
        .then(tokens => tokens.sort())
        .then(tokens => [...new Set(tokens)])
    )
  })

  const DOCUMENTS = requestedDocs => Promise.all(
    requestedDocs.map(doc => fii.STORE.get('￮DOC_RAW￮' + doc._id + '￮'))
  )
  
  const AND = (...keys) => fii.AND(
    ...keys.map(fii.GET)
  ).then(flattenMatch)

  const SEARCH = (...q) => AND(...q)
    .then(resultSet => TFIDF({
      fii: fii,
      resultSet: resultSet,
      offset: 0,
      limit: 10
    }))

  const OR = (...q) => fii.OR(
    ...flatten(q.map(fii.GET))
  ).then(flattenMatch)

  // NOT
  const SET_DIFFERENCE = (a, b) => Promise.all([
    (typeof a === 'string') ? fii.GET(a) : a,
    (typeof b === 'string') ? fii.GET(b) : b
  ]).then(([a, b]) => a.filter(
    aItem => b.map(bItem => bItem._id).indexOf(aItem._id) === -1)
  )

  const DISTINCT = term => fii.DISTINCT(term).then(result => [
    ...result.reduce((acc, cur) => {
      cur.value = cur.value.split('#')[0]
      acc.add(JSON.stringify(cur))
      return acc
    }, new Set())
  ].map(JSON.parse))

  // TODO: Tests for JSON nesting and JSON .then-ing
  // This function reads queries in a JSON format and then translates them to
  // Promises
  const parseJsonQuery = (...q) => {
    // needs to be called with "command" and result from previous "thenable"
    var promisifyQuery = (command, resultFromPreceding) => {
      if (typeof command === 'string') return fii.GET(command)
      if (command.ALL) return Promise.all(
        // TODO: why cant this be "command.ALL.map(promisifyQuery)"?
        command.ALL.map(item => promisifyQuery(item))
      )
      if (command.AND) return AND(...command.AND.map(promisifyQuery))
      if (command.BUCKETFILTER) {
        if (command.BUCKETFILTER.BUCKETS.DISTINCT) {
          return fii.BUCKETFILTER(
            DISTINCT(command.BUCKETFILTER.BUCKETS.DISTINCT)
              .then(bkts => bkts.map(fii.BUCKET)),
            promisifyQuery(command.BUCKETFILTER.FILTER)
          )
        }
        else {
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
      if (command.OR) return OR(...command.OR.map(promisifyQuery))
      if (command.NOT) return SET_DIFFERENCE(
        promisifyQuery(command.NOT.include),
        promisifyQuery(command.NOT.exclude)
      )
      if (command.SEARCH) return SEARCH(...command.SEARCH.map(promisifyQuery))
    }
    // Turn the array of commands into a chain of promises
    return q.reduce((acc, cur) => acc.then(
      result => promisifyQuery(cur, result)
    ), promisifyQuery(q.shift())) // <- Separate the first promise in the chain
    //                                  to be used as the start point in .reduce
  }

  return {
    AND: AND,
    BUCKET: fii.BUCKET,
    BUCKETFILTER: fii.BUCKETFILTER,
    DICTIONARY: DICTIONARY,
    DISTINCT: DISTINCT,
    DOCUMENTS: DOCUMENTS,
    GET: fii.GET,
    OR: OR,
    SCORENUMERIC: numericField,
    SCORETFIDF: TFIDF,
    SEARCH: SEARCH,
    SET_DIFFERENCE: SET_DIFFERENCE,
    parseJsonQuery: parseJsonQuery
  }
}
