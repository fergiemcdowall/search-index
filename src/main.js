import fii from 'fergies-inverted-index'
import writer from './write.js'
import reader from './read.js'
import util from './util.js'

global.D = 0 // total docs in index
global.searchableFields = [] // fields that are available for searching

const makeASearchIndex = idx => {

  // const parseJsonQuery = q => {
  //   if (typeof q == 'string') return r.GET(q)
  //   return Object.entries(q).map(([key, value]) => {
  //     // TODO: allow only valid functions, throw a nice error
  //     return r[key](...value.map(parseJsonQuery))
  //   })[0] // only evaluate one (first) key-value pair in object
  // }

  const parseJsonQuery = (...q) => {
    console.log(q)
    var start = q.shift()

    // needs to be called with "command" and result from previous "thenable"
    var promisifyQuery = (command, resultFromPreceding) => {
      console.log(command)
      if (typeof command == 'string') return r.GET(command)
      if (command.DOCUMENTS) return r.DOCUMENTS(resultFromPreceding)
      if (command.OR) return r.OR(...command.OR.map(promisifyQuery))
      if (command.AND) return r.AND(...command.AND.map(promisifyQuery))
    }
    
    return q.reduce((acc, cur) => {
      console.log(cur)
      return acc.then(result => promisifyQuery(cur, result))
    }, promisifyQuery(start))
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
