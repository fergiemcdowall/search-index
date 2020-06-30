import fii from 'fergies-inverted-index'
import writer from './write.js'
import reader from './read.js'
import util from './util.js'

global.D = 0 // total docs in index
global.searchableFields = [] // fields that are available for searching

const makeASearchIndex = idx => {
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
    NOT: r.SET_SUBTRACTION,
    OR: r.OR,
    PAGE: r.PAGE,
    PUT: w.PUT,
    SCORE: r.SCORE,
    /* SCORENUMERIC: r.SCORENUMERIC,
     * SCORETFIDF: r.SCORETFIDF,*/
    SEARCH: r.SEARCH,
    SORT: r.SORT,
    QUERY: r.parseJsonQuery,
    UPDATE: w.parseJsonUpdate
  }
}

export default function (ops) {
  return new Promise((resolve, reject) => {
    ops = Object.assign(ops || {}, {
      tokenAppend: '#'
    })
    // if a fergies-inverted-index is passed as an option
    if (ops.fii) return resolve(makeASearchIndex(ops.fii))
    // else make a new fergies-inverted-index
    fii(ops, (err, idx) => {
      if (err) return reject(err)
      resolve(util(idx).calibrate()
        .then(() => {
          return makeASearchIndex(idx)
        }))
    })
  })
}
