const fii = require('fergies-inverted-index')
const writer = require('./write.js')
const reader = require('./read.js')
const util = require('./util.js')

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
    SCORE: r.SCORE,
    SEARCH: r.SEARCH
  }
}

module.exports = (ops, callback) => {
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
