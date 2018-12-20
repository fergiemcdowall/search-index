module.exports = fii => {
  const SCORE = require('./scorers.js')

  const flatten = arr => [].concat.apply([], arr)

  const flattenMatch = result => result.map(x => {
    x.match = flatten(x.match) // flatten
    x.match = flatten(x.match) // flatten again
    return x
  })

  const DICTIONARY = prefix => new Promise((resolve, reject) => {
    const dict = new Set()
    const ks = fii.STORE.createKeyStream({
      gte: prefix,
      lte: prefix + '￮'
    })
    ks.on('data', d => dict.add(d.split(':')[0].split('.').pop()))
    ks.on('end', () => resolve(Array.from(dict).sort()))
  })

  const DOCUMENTS = hits => new Promise(
    (resolve, reject) =>
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
    .then(resultSet => SCORE.TFIDF({
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
      var [ a, b ] = result
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

  return {
    AGGREGATE: fii.AGGREGATE,
    AND: AND,
    BUCKET: fii.BUCKET,
    DICTIONARY: DICTIONARY,
    DISTINCT: DISTINCT,
    DOCUMENTS: DOCUMENTS,
    GET: GET,
    SET_DIFFERENCE: SET_DIFFERENCE,
    OR: OR,
    SCORE: SCORE,
    SEARCH: SEARCH
  }
}
