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

    return resolve(
      new Promise(resolve => resolve(q.fields || getAvailableFields(fii)))
        .then(fields => Promise.all(
          fields.map(field => getRange(fii, {
            gte: field + '.' + q.gte,
            lte: field + '.' + q.lte + '￮'
          }))
        ))
        .then(flatten)
        .then(tokens => tokens.map(t => t.split(':')[0].split('.').pop()))
        .then(tokens => tokens.sort())
        .then(tokens => [...new Set(tokens)])
    )
  })

  const DOCUMENTS = requestedDocs => new Promise(
    resolve => fii.OBJECT(requestedDocs).then(
      retrievedDocs => resolve(requestedDocs.map((hit, i) => (Object.assign({
        _doc: retrievedDocs[i] ? retrievedDocs[i]['!doc'] : null
      }, requestedDocs[i]))))
    ))

  const AND = (...keys) => fii.AND(
    ...keys.map(GET)
  ).then(flattenMatch)

  const SEARCH = (...q) => AND(...q)
    .then(resultSet => TFIDF({
      resultSet: resultSet,
      offset: 0,
      limit: 10
    }))
    .then(resultSet => DOCUMENTS(resultSet))

  const OR = (...q) => fii.OR(
    ...flatten(q.map(GET))
  ).then(flattenMatch)

  // NOT
  const SET_DIFFERENCE = (a, b) => Promise.all([
    (typeof a === 'string') ? GET(a) : a,
    (typeof b === 'string') ? GET(b) : b
  ]).then(([a, b]) => a.filter(
    aItem => b.map(bItem => bItem._id).indexOf(aItem._id) === -1)
  )

  const GET = clause => {
    // could be a nested AND/OR/something else
    if (clause instanceof Promise) return clause
    // ELSE wildcard (*) search
    if (clause.slice(-2) === ':*') return fii.GET(clause.replace(':*', '.'))
    // ELSE a clause with a specified field ("<fieldpath>:clause")
    if (clause.indexOf(':') > -1) return fii.GET(clause.replace(':', '.') + ':')
    // ELSE a clause without specified field ("clause")
    return OR(...global.searchableFields.map(f => f + ':' + clause))
  }

  const DISTINCT = term => fii.DISTINCT(term).then(result => {
    return [...result.reduce((acc, cur) => {
      acc.add(cur.split(':')[0])
      return acc
    }, new Set())]
  })

  // TODO: Tests for JSON nesting and JSON .then-ing
  // This function reads queries in a JSON format and then translates them to
  // Promises
  const parseJsonQuery = (...q) => {
    // needs to be called with "command" and result from previous "thenable"
    var promisifyQuery = (command, resultFromPreceding) => {
      if (typeof command === 'string') return GET(command)
      if (command.AND) return AND(...command.AND.map(promisifyQuery))
      if (command.BUCKETFILTER) {
        return fii.BUCKETFILTER(
          Promise.all(command.BUCKETFILTER[0].map(promisifyQuery)),
          parseJsonQuery(command.BUCKETFILTER[1])
        )
      }
      // feed in preceding results if present (ie if not first promise)
      if (command.BUCKET) return fii.BUCKET(resultFromPreceding || command.BUCKET)
      if (command.DICTIONARY) return DICTIONARY(command.DICTIONARY)
      if (command.DISTINCT) return DISTINCT(command.DISTINCT)
      // feed in preceding results if present (ie if not first promise)
      if (command.DOCUMENTS) return DOCUMENTS(resultFromPreceding || command.DOCUMENTS)
      if (command.GET) return GET(command.GET)
      if (command.OR) return OR(...command.OR.map(promisifyQuery))
      if (command.NOT) {
        return SET_DIFFERENCE(
          promisifyQuery(command.NOT.include),
          promisifyQuery(command.NOT.exclude)
        )
      }
      if (command.SEARCH) return SEARCH(...command.SEARCH.map(promisifyQuery))
    }
    // Turn the array of commands into a chain of promises
    return q.reduce((acc, cur) => acc.then(
      result => promisifyQuery(cur, result)
    ), promisifyQuery(q.shift())) // <- Separate the first promise in the chain
    //    to be used as the start point in .reduce
  }

  return {
    AND: AND,
    BUCKET: fii.BUCKET,
    BUCKETFILTER: fii.BUCKETFILTER,
    DICTIONARY: DICTIONARY,
    DISTINCT: DISTINCT,
    DOCUMENTS: DOCUMENTS,
    GET: GET,
    OR: OR,
    SCORENUMERIC: numericField,
    SCORETFIDF: TFIDF,
    SEARCH: SEARCH,
    SET_DIFFERENCE: SET_DIFFERENCE,
    parseJsonQuery: parseJsonQuery
  }
}
