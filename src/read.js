import { TFIDF, numericField } from './scorers.js'

export default function (fii) {
  const flatten = arr => [].concat.apply([], arr)

  const flattenMatch = result => result.map(x => {
    x.match = flatten(x.match) // flatten
    x.match = flatten(x.match) // flatten again
    return x
  })

  const DICTIONARY = q => new Promise((resolve) => {
    const dict = new Set()
    // if query is string convert to object
    if (typeof q === 'string') q = { gte: q, lte: q + '￮' }
    // if no query, make empty query
    else q = Object.assign({ gte: '', lte: '￮' }, q)
    // append separator if not there already
    q.lte = (q.lte.substr(-1) === '￮') ? q.lte : q.lte + '￮'
    const ks = fii.STORE.createKeyStream(q)
    ks.on('data', d => dict.add(d.split(':')[0].split('.').pop()))
    ks.on('end', () => resolve(Array.from(dict).sort()))
  })

  const DOCUMENTS = hits => new Promise(
    (resolve) =>
      fii.OBJECT(hits).then(
        documents => resolve(hits.map((hit, i) => {
          hit.obj = documents[i]['!doc']
          return hit
        }))
      )
  )

  const AND = function (...keys) {
    return fii.AND(
      ...keys.map(GET)
    ).then(flattenMatch)
  }

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
  const SET_DIFFERENCE = (a, b) => {
    if (typeof a === 'string') a = GET(a)
    if (typeof b === 'string') b = GET(b)
    return Promise.all([a, b]).then(result => {
      let [ a, b ] = result
      b = b.map(item => item._id)
      return a.filter(item => b.indexOf(item._id))
    })
  }

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
  const parseJsonQuery = (...q) => {
    // Separate the first promise in the chain to be used as the start point in .reduce
    var start = q.shift()
    // needs to be called with "command" and result from previous "thenable"
    var promisifyQuery = (command, resultFromPreceding) => {
      if (typeof command === 'string') return GET(command)
      if (command.OR) return OR(...command.OR.map(promisifyQuery))
      if (command.AND) return AND(...command.AND.map(promisifyQuery))
      if (command.SEARCH) return SEARCH(...command.SEARCH.map(promisifyQuery))
      if (command.DICTIONARY) return DICTIONARY(command.DICTIONARY)
      if (command.DISTINCT) return DISTINCT(command.DISTINCT)
      if (command.GET) return GET(command.GET)
      if (command.BUCKETFILTER) {
        return fii.BUCKETFILTER(
          Promise.all(command.BUCKETFILTER[0].map(promisifyQuery)),
          parseJsonQuery(command.BUCKETFILTER[1])
        )
      }
      // feed in preceding results if present (ie if not first promise)
      if (command.DOCUMENTS) return DOCUMENTS(resultFromPreceding || command.DOCUMENTS)
      // feed in preceding results if present (ie if not first promise)
      if (command.BUCKET) return fii.BUCKET(resultFromPreceding || command.BUCKET)
    }
    // Turn the array of commands into a chain of promises
    return q.reduce((acc, cur) => acc.then(
      result => promisifyQuery(cur, result)
    ), promisifyQuery(start))
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
