import util from './util.js'
//import trav from 'traverse'
import tv from 'term-vector'

const scoreArrayTFIDF = arr => {
  const v = tv(arr)
  const mostTokenOccurances = v.reduce((acc, cur) => Math.max(cur.positions.length, acc), 0)
  return v
    .map(item => item.term[0] + '#'
             + (((item.positions.length / mostTokenOccurances)).toFixed(2)))
}

// traverse object, tokenising all leaves (strings to array) and then
// scoring them
// `ops` is a collection of indexing pipeline options
const createDocumentVector = (obj, ops) => Object.entries(obj).reduce((acc, cur) => {
  ops = Object.assign({
    caseSensitive: false
  }, ops || {})
  if (cur[0] == '_id') {
    acc[cur[0]] = cur[1]  // return _id "as is"
  } else if (Array.isArray(cur[1])) {
    // split up cur[1] into an array or strings and an array of
    // other things. Then term-vectorize strings and recursively
    // process other things.
    const strings = scoreArrayTFIDF(
      cur[1].filter(item => typeof item === 'string')
    )
    const notStrings = cur[1].filter(
      item => typeof item != 'string'
    ).map(traverseObject)
    acc[cur[0]] = strings.concat(notStrings)
  }
  else if (typeof cur[1] === 'object') {
    acc[cur[0]] = traverseObject(cur[1])
  }
  else {
    let str = cur[1].toString().replace(/[^0-9a-z ]/gi, '')
    if (!ops.caseSensitive) str = str.toLowerCase()
    acc[cur[0]] = scoreArrayTFIDF(str.split(' '))
  }
  return acc
}, {})


export default function (fii) {

  const incrementDocCount = increment => fii.STORE.get(
    '￮DOCUMENT_COUNT￮'
  ).then(
    count => fii.STORE.put('￮DOCUMENT_COUNT￮', +count + increment)
  ).catch(
    // if not found assume value to be 0
    e => fii.STORE.put('￮DOCUMENT_COUNT￮', increment)
  )

  const PUT = (docs, ops) => fii.PUT(
    docs.map(doc => createDocumentVector(doc, ops))
  ).then(documentVector => Promise.all(
    docs.map(doc =>
      fii.STORE.put('￮DOC_RAW￮' + doc._id + '￮', doc)
    )).then(
      result => incrementDocCount(documentVector.length)
    )
  )

  return {
    // TODO: surely this can be DELETE: fii.DELETE?
    DELETE: (..._ids) => fii.DELETE(..._ids),
    PUT: PUT
  }
}


