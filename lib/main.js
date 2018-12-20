const fii = require('fergies-inverted-index')
const writer = require('./write.js')
const reader = require('./read.js')
const util = require('./util.js')

global.D = 0 // total docs in index
global.searchableFields = [] // fields that are available for searching

const OPEN = ops => new Promise((resolve, reject) => {
  fii.OPEN(ops).then(
    idx => {
      const w = writer(idx)
      const r = reader(idx)
      const u = util(idx)
      u.prefetchSearchableFields() // read searchable fields
        .then(u.countDocs) // sum up docs
        .then(() => resolve({
          AGGREGATE: r.AGGREGATE,
          AND: r.AND,
          BUCKET: r.BUCKET,
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
        }))
    })
})

// open a new instance and create a reference from global
const INIT = async ops => {
  global[ops.name] = await OPEN(ops)
}

exports.INIT = INIT
exports.OPEN = OPEN
