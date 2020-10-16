import fii from 'fergies-inverted-index'
import writer from './write.js'
import reader from './read.js'

const makeASearchIndex = (idx, ops) => {
  const w = writer(idx, ops)
  const r = reader(idx)
  return {
    // internal functions inherited from fergies-inverted-index
    _AGGREGATE: idx.AGGREGATE,
    _AND: idx.AND,
    _BUCKET: idx.BUCKET,
    _BUCKETS: idx.BUCKETS,
    _FIELDS: idx.FIELDS,
    _GET: idx.GET,
    _NOT: idx.SET_SUBTRACTION,
    _OR: idx.OR,

    // search-index read
    _DISTINCT: r.DISTINCT,
    _DOCUMENT_COUNT: r.DOCUMENT_COUNT,
    _FACETS: r.FACETS,
    _PAGE: r.PAGE,
    _SCORE: r.SCORE,
    _SEARCH: r.SEARCH,
    _SORT: r.SORT,

    // search-index write
    _DELETE: w.DELETE,
    _PUT: w._PUT,
    _PUT_RAW: w._PUT_RAW,

    // public API
    DELETE: w.DELETE,
    DICTIONARY: r.DICTIONARY,
    DOCUMENTS: r.DOCUMENTS,
    DOCUMENT_COUNT: r.DOCUMENT_COUNT,
    EXPORT: idx.EXPORT,
    FIELDS: r.FIELDS,
    IMPORT: idx.IMPORT,
    INDEX: idx,
    MAX: idx.MAX,
    MIN: idx.MIN,
    PUT: w.PUT,
    PUT_RAW: w.PUT_RAW,
    QUERY: r.QUERY
  }
}

export default function (ops) {
  return new Promise((resolve, reject) => {
    ops = Object.assign({
      tokenAppend: '#',
      caseSensitive: false
    }, ops || {})
    // if a fergies-inverted-index is passed as an option
    if (ops.fii) return resolve(makeASearchIndex(ops.fii, ops))
    // else make a new fergies-inverted-index
    fii(ops, (err, idx) => {
      if (err) return reject(err)
      resolve(makeASearchIndex(idx, ops))
    })
  })
}
