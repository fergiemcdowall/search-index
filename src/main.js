import fii from 'fergies-inverted-index'
import writer from './write.js'
import reader from './read.js'

const makeASearchIndex = (idx, ops) => {
  const w = writer(idx, ops)
  const r = reader(idx)
  return {
    AND: r.AND,
    BUCKET: r.BUCKET,
    BUCKETFILTER: r.BUCKETFILTER,
    DELETE: w.DELETE,
    DICTIONARY: r.DICTIONARY,
    DISTINCT: r.DISTINCT,
    DOCUMENTS: r.DOCUMENTS,
    DOCUMENT_COUNT: r.DOCUMENT_COUNT,
    FIELDS: r.FIELDS,
    GET: r.GET,
    INDEX: idx,
    NOT: r.SET_SUBTRACTION,
    OR: r.OR,
    PAGE: r.PAGE,
    PUT: w.PUT,
    QUERY: r.parseJsonQuery,
    SCORE: r.SCORE,
    SEARCH: r.SEARCH,
    SORT: r.SORT,
    UPDATE: w.parseJsonUpdate
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
