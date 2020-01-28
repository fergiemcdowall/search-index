import util from './util.js'
//import trav from 'traverse'
import tv from 'term-vector'

export default function (fii) {

  const scoreArrayTFIDF = arr => {
    const v = tv(arr)
    const mostTokenOccurances = v.reduce((acc, cur) => Math.max(cur.positions.length, acc), 0)
    return v
      .map(item => item.term[0] + '#'
           + (((item.positions.length / mostTokenOccurances)).toFixed(2)))
  }

  // traverse object, tokenising all leaves (strings to array) and then
  // scoring them
  const traverseObject = obj => Object.entries(obj).reduce((acc, cur) => {  
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
      acc[cur[0]] = scoreArrayTFIDF(
        cur[1].toString().split(' ')
      )  
    }
    return acc
  }, {})

  const PUT = docs => fii.PUT(docs.map(traverseObject))

  return {
    // TODO: surely this can be DELETE: fii.DELETE?
    DELETE: (..._ids) => fii.DELETE(..._ids),
    PUT: PUT
  }
}


