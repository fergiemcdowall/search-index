// export default function (fii, ops) {
module.exports = (fii, ops) => {
  const createDocumentVector = (docs, putOptions) => {
    // tokenize
    // lowcase
    // ngrams
    // stopwords
    // synonyms
    // score
    const runTokenizationPipeline = (fieldName, str) =>
      putOptions.tokenizationPipeline.reduce(
        (acc, cur) => cur(acc, fieldName, putOptions), str
      )

    // traverse object, tokenising all leaves (strings to array) and then
    // scoring them
    // `ops` is a collection of indexing pipeline options
    const traverseObject = obj => Object.entries(obj).reduce((acc, [
      fieldName, fieldValue
    ]) => {
      // if fieldname is undefined, ignore and procede to next
      if (fieldValue === undefined) return acc

      // if fieldName is '_id', return _id "as is" and stringify
      if (fieldName === '_id') {
        acc[fieldName] = fieldValue + ''
        return acc
      }

      // If fieldValue is an Array
      if (Array.isArray(fieldValue)) {
        // split up fieldValue into an array of strings and an array of
        // other things. Then tokenize strings and recursively process
        // other things.
        const strings = fieldValue.filter(item => typeof item === 'string')
        const notStrings = fieldValue.filter(item => typeof item !== 'string')
        acc[fieldName] = [
          ...strings.map(str => runTokenizationPipeline(fieldName, str)),
          ...notStrings.map(traverseObject)
        ].sort()
        return acc
      }

      // If fieldValue is an Object
      if (typeof fieldValue === 'object') {
        acc[fieldName] = traverseObject(fieldValue)
        return acc
      }

      // else fieldvalue is Number or String
      acc[fieldName] = runTokenizationPipeline(fieldName, fieldValue.toString())
      return acc
    }, {})

    // remove fields who's value is []
    // (matching empty arrays in js is messier than you might think...)
    const removeEmptyFields = doc => {
      Object.keys(doc).forEach(fieldName => {
        if (Array.isArray(doc[fieldName])) {
          if (doc[fieldName].length === 0) { delete doc[fieldName] }
        }
      })
      return doc
    }

    return docs
      .map(traverseObject)
      .map(removeEmptyFields)
  }

  const incrementDocCount = increment => fii.STORE.get(
    '￮DOCUMENT_COUNT￮'
  ).then(
    count => fii.STORE.put('￮DOCUMENT_COUNT￮', +count + increment)
  ).catch(
    // if not found assume value to be 0
    e => fii.STORE.put('￮DOCUMENT_COUNT￮', increment)
  )

  const decrementDocCount = increment => fii.STORE.get(
    '￮DOCUMENT_COUNT￮'
  ).then(
    count => fii.STORE.put('￮DOCUMENT_COUNT￮', +count - increment)
  )

  const parseStringAsDoc = doc => (typeof doc === 'string')
    ? ({ body: doc })
    : doc

  // TODO: generated IDs are derived from timestamps. Possibly there
  // should be some sort of timer here that makes sure that this
  // function uses at least 1 millisecond in order to avoid id collisions
  const generateId = (doc, i) => (typeof doc._id === 'undefined')
    ? Object.assign(doc, {
        _id: Date.now() + '-' + i
      })
    : doc

  const _PUT = (docs, putOptions) => {
    putOptions = Object.assign(ops, putOptions)

    const rawDocs = docs
      .map(parseStringAsDoc)
      .map(generateId)

    return fii.PUT(
      createDocumentVector(
        rawDocs,
        putOptions),
      putOptions
    ).then(
      result => Promise.all([
        _PUT_RAW(rawDocs, !ops.storeRawDocs),
        incrementDocCount(result.filter(r => r.status === 'CREATED').length)
      ]).then(() => result)
    )
  }

  const _PUT_RAW = (docs, dontStoreValue) => Promise.all(
    docs.map(
      doc => fii.STORE.put('￮DOC_RAW￮' + doc._id + '￮', dontStoreValue ? {} : doc)
    )
  ).then(
    // TODO: make this actually deal with errors
    result => docs.map(
      doc => ({
        _id: doc._id,
        status: 'OK',
        operation: '_PUT_RAW'
      })
    )
  )

  const _DELETE = _ids => fii.DELETE(_ids).then(result => {
    const deleted = result.filter(d => d.status === 'DELETED')
    return Promise.all([
      Promise.all(deleted.map(r => fii.STORE.del('￮DOC_RAW￮' + r._id + '￮'))),
      decrementDocCount(deleted.length)
    ]).then(() => result)
  })

  return {
    // TODO: DELETE should be able to handle errors (_id not found etc.)
    DELETE: docIds => _DELETE(docIds), // for external use
    IMPORT: fii.IMPORT,
    PUT: _PUT,
    PUT_RAW: _PUT_RAW,
    _DELETE: _DELETE, // for internal use
    _PUT: _PUT,
    _PUT_RAW: _PUT_RAW
  }
}
