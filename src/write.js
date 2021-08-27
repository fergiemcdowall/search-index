// export default function (fii, ops) {
module.exports = (ops, cache, queue) => {
  const createDocumentVectors = (docs, putOptions) => {
    const runTokenizationPipeline = (fieldName, fieldValue) => {
      if (typeof fieldValue !== 'string') {
        return JSON.stringify([fieldValue, fieldValue])
      }
      return putOptions.tokenizationPipeline.reduce(
        (acc, cur) => cur(acc, fieldName, putOptions),
        fieldValue
      )
    }

    // traverse object, tokenising all leaves (strings to array) and then
    // scoring them
    // `ops` is a collection of indexing pipeline options
    const traverseObject = obj =>
      Object.entries(obj).reduce((acc, [fieldName, fieldValue]) => {
        const isObject = item =>
          typeof item === 'object' && item !== null && !Array.isArray(item)

        // if fieldname is undefined or null, ignore and procede to next
        if (fieldValue === undefined) return acc
        //        if (fieldValue === null) return acc

        // if fieldName is '_id', return _id "as is" and stringify
        if (fieldName === '_id') {
          // acc[fieldName] = fieldValue + ''
          acc[fieldName] = fieldValue
          return acc
        }

        // If fieldValue is an Object
        if (isObject(fieldValue)) {
          acc[fieldName] = traverseObject(fieldValue)
          return acc
        }

        // If fieldValue is an Array
        if (Array.isArray(fieldValue)) {
          // TODO: should be objects or not objects?

          // split up fieldValue into an array of objects and an array of
          // other things. Then tokenize other things and recursively process
          // objects.
          const notObjects = fieldValue.filter(item => !isObject(item))
          const objects = fieldValue.filter(isObject)
          acc[fieldName] = [
            ...notObjects.map(str => runTokenizationPipeline(fieldName, str)),
            ...objects.map(traverseObject)
          ].sort()

          return acc
        }

        acc[fieldName] = runTokenizationPipeline(fieldName, fieldValue)

        return acc
      }, {})

    // remove fields who's value is [] (empty array)
    // (matching empty arrays in js is messier than you might think...)
    const removeEmptyFields = doc => {
      Object.keys(doc).forEach(fieldName => {
        if (Array.isArray(doc[fieldName])) {
          if (doc[fieldName].length === 0) {
            delete doc[fieldName]
          }
        }
      })
      return doc
    }

    return docs.map(traverseObject).map(removeEmptyFields)
  }

  const incrementDocCount = increment =>
    ops.fii.STORE.get(['DOCUMENT_COUNT'])
      .then(count => ops.fii.STORE.put(['DOCUMENT_COUNT'], +count + increment))
      .catch(
        // if not found assume value to be 0
        e => ops.fii.STORE.put(['DOCUMENT_COUNT'], increment)
      )

  const decrementDocCount = increment =>
    ops.fii.STORE.get(['DOCUMENT_COUNT']).then(count =>
      ops.fii.STORE.put(['DOCUMENT_COUNT'], +count - increment)
    )

  const parseStringAsDoc = doc =>
    typeof doc === 'string' ? { body: doc } : doc

  let counter = 0
  const generateId = (doc, i) =>
    typeof doc._id === 'undefined'
      ? Object.assign(doc, {
          // counter is needed because if this function is called in quick
          // succession, Date.now() is not guaranteed to be unique. This could
          // still conflict if the DB is closed, clock is reset to the past, then
          // DB reopened. That's a bit of a corner case though.
          _id: `${Date.now()}-${i}-${counter++}`
        })
      : doc

  const _PUT = (docs, putOptions) => {
    cache.reset()

    putOptions = Object.assign(ops, putOptions)

    const rawDocs = docs.map(parseStringAsDoc).map(generateId)

    return ops.fii
      .PUT(createDocumentVectors(rawDocs, putOptions), putOptions)
      .then(result =>
        Promise.all([
          _PUT_RAW(rawDocs, !ops.storeRawDocs),
          incrementDocCount(result.filter(r => r.status === 'CREATED').length)
        ]).then(() => result)
      )
  }

  const _PUT_RAW = (docs, dontStoreValue) => {
    cache.reset()
    return Promise.all(
      docs.map(doc =>
        ops.fii.STORE.put(['DOC_RAW', doc._id], dontStoreValue ? {} : doc)
      )
    ).then(
      // TODO: make this actually deal with errors
      result =>
        docs.map(doc => ({
          _id: doc._id,
          status: 'OK',
          operation: '_PUT_RAW'
        }))
    )
  }

  const _DELETE = _ids =>
    ops.fii.DELETE(_ids).then(result => {
      cache.reset()
      const deleted = result.filter(d => d.status === 'DELETED')
      return Promise.all([
        Promise.all(deleted.map(r => ops.fii.STORE.del(['DOC_RAW', r._id]))),
        decrementDocCount(deleted.length)
      ]).then(() => result)
    })

  const _FLUSH = () =>
    ops.fii.STORE.clear()
      .then(() => {
        cache.reset()
        const timestamp = Date.now()
        return ops.fii.STORE.batch([
          { type: 'put', key: ['~CREATED'], value: timestamp },
          { type: 'put', key: ['~LAST_UPDATED'], value: timestamp },
          { type: 'put', key: ['DOCUMENT_COUNT'], value: 0 }
        ])
      })
      .then(() => true)

  return {
    DELETE: (...docIds) => _DELETE(docIds), // for external use
    FLUSH: _FLUSH,
    // TODO: IMPORT needs a test
    // IMPORT: () => cache.flush().then(ops.fii.IMPORT),
    IMPORT: index => {
      cache.reset()
      return Promise.resolve(ops.fii.IMPORT(index))
    },
    PUT: (docs, pops) => queue.add(() => _PUT(docs, pops)),
    PUT_RAW: _PUT_RAW,
    _INCREMENT_DOC_COUNT: incrementDocCount
  }
}
