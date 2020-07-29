import fii from 'fergies-inverted-index'
import writer from './write.js'
import reader from './read.js'

const makeASearchIndex = (idx, ops) => {
  const w = writer(idx, ops)
  const r = reader(idx)
  return {
    // inherited from fergies-inverted-index
    AND: idx.AND,
    BUCKET: idx.BUCKET,
    BUCKETFILTER: idx.BUCKETFILTER,
    FIELDS: idx.FIELDS,
    GET: idx.GET,
    INDEX: idx,
    MAX: idx.MAX,
    MIN: idx.MIN,
    NOT: idx.SET_SUBTRACTION,
    OR: idx.OR,
    // search-index read
    DICTIONARY: r.DICTIONARY,
    DISTINCT: r.DISTINCT,
    DOCUMENTS: r.DOCUMENTS,
    DOCUMENT_COUNT: r.DOCUMENT_COUNT,
    PAGE: r.PAGE,
    QUERY: r.QUERY,
    SCORE: r.SCORE,
    SEARCH: r.SEARCH,
    SORT: r.SORT,
    // search-index write
    DELETE: w.DELETE,
    PUT: w.PUT,
    UPDATE: w.UPDATE
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
