import tv from 'term-vector'

const scoreArrayTFIDF = arr => {
  const v = tv(arr)
  const mostTokenOccurances = v.reduce((acc, cur) => Math.max(cur.positions.length, acc), 0)
  return v
    .map(item => item.term[0] + '#' +
             (((item.positions.length / mostTokenOccurances)).toFixed(2)))
}

// traverse object, tokenising all leaves (strings to array) and then
// scoring them
// `ops` is a collection of indexing pipeline options
const createDocumentVector = (obj, ops) => Object.entries(obj).reduce((acc, [
  fieldName, fieldValue
]) => {
  // if fieldname is undefined, ignore and procede to next
  if (fieldValue === undefined) return acc
  ops = Object.assign({
    caseSensitive: false
  }, ops || {})
  if (fieldName === '_id') {
    acc[fieldName] = fieldValue // return _id "as is"
  } else if (Array.isArray(fieldValue)) {
    // split up fieldValue into an array or strings and an array of
    // other things. Then term-vectorize strings and recursively
    // process other things.
    const strings = scoreArrayTFIDF(
      fieldValue
        .filter(item => typeof item === 'string')
        .map(str => str.toLowerCase())
    )
    const notStrings = fieldValue.filter(
      item => typeof item !== 'string'
    ).map(createDocumentVector)
    acc[fieldName] = strings.concat(notStrings).sort()
  } else if (typeof fieldValue === 'object') {
    acc[fieldName] = createDocumentVector(fieldValue)
  } else {
    let str = fieldValue.toString().replace(/[^0-9a-z ]/gi, '')
    if (!ops.caseSensitive) str = str.toLowerCase()
    acc[fieldName] = scoreArrayTFIDF(str.split(' ')).sort()
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

  const DELETE = _ids => fii.DELETE(_ids).then(
    result => Promise.all(
      result.map(
        r => fii.STORE.del('￮DOC_RAW￮' + r._id + '￮')
      )
    ).then(
      result => _ids.map(
        _id => ({
          _id: _id,
          operation: 'DELETE',
          status: 'OK'
        })
      )
    )
  )

  const parseJsonUpdate = update => {
    if (update.DELETE) return DELETE(update.DELETE)
  }

  return {
    // TODO: DELETE should be able to handle errors (_id not found etc.)
    DELETE: DELETE,
    PUT: PUT,
    parseJsonUpdate: parseJsonUpdate
  }
}
