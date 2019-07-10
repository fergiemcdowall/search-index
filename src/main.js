import fii from 'fergies-inverted-index'
import writer from './write.js'
import reader from './read.js'
import util from './util.js'

global.D = 0 // total docs in index
global.searchableFields = [] // fields that are available for searching

const makeASearchIndex = idx => {

  // takes an array of queries in JSON format and turns them into a
  // then-ed chain of Promises.
  const parseJsonQuery = (...q) => {
    // Separate the first promise in the chain to be used as the start point in .reduce
    var start = q.shift()

    // needs to be called with "command" and result from previous "thenable"
    var promisifyQuery = (command, resultFromPreceding) => {
      if (typeof command == 'string') return r.GET(command)

      if (command.OR) return r.OR(...command.OR.map(promisifyQuery))
      if (command.AND) return r.AND(...command.AND.map(promisifyQuery))
      if (command.SEARCH) return r.SEARCH(...command.SEARCH.map(promisifyQuery))
      // write tests for BUCKETFILTER
      if (command.BUCKETFILTER) return r.BUCKETFILTER(command.BUCKETFILTER)  
      if (command.DICTIONARY) return r.DICTIONARY(command.DICTIONARY)
      if (command.DISTINCT) return r.DISTINCT(command.DISTINCT)
      if (command.GET) return r.GET(command.GET)

      // feed in preceding results if present (ie if not first promise)
      if (command.DOCUMENTS) return r.DOCUMENTS(resultFromPreceding || command.DOCUMENTS)
      // feed in preceding results if present (ie if not first promise)
      if (command.BUCKET) return r.BUCKET(resultFromPreceding || command.BUCKET)
      
    }

    // Turn the array of commands into a chain of promises
    return q.reduce((acc, cur) => acc.then(
      result => promisifyQuery(cur, result)
    ), promisifyQuery(start))
  }



  const w = writer(idx)
  const r = reader(idx)
  return {
    AND: r.AND,
    BUCKET: r.BUCKET,
    BUCKETFILTER: r.BUCKETFILTER,
    DELETE: w.DELETE,
    DICTIONARY: r.DICTIONARY,
    DISTINCT: r.DISTINCT,
    DOCUMENTS: r.DOCUMENTS,
    GET: r.GET,
    INDEX: idx,
    NOT: r.SET_DIFFERENCE,
    OR: r.OR,
    PUT: w.PUT,
    SCORENUMERIC: r.SCORENUMERIC,
    SCORETFIDF: r.SCORETFIDF,
    SEARCH: r.SEARCH,
    read: parseJsonQuery
  }
}

export default function (ops, callback) {
  // if no callback then return lazy load
  if (!callback) {
    const idx = ops.fii || fii(ops)
    // lazy calibration
    util(idx).calibrate()
    return makeASearchIndex(idx)
  } else {
    fii(ops, (err, idx) => {
      util(idx).calibrate()
        .then(() => callback(err, makeASearchIndex(idx)))
    })
  }
}
