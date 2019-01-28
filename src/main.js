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
    NOT: r.SET_DIFFERENCE,
    OR: r.OR,
    PUT: w.PUT,
    SCORENUMERIC: r.SCORENUMERIC,
    SCORETFIDF: r.SCORETFIDF,
    SEARCH: r.SEARCH
  }
}

export default function (ops, callback) {
  // if no callback then return lazy load
  if (!callback) {
    let idx = fii(ops)
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
