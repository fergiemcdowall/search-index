const DocumentProcessor = require('./DocumentProcessor')

module.exports = (ops, cache, queue) => {
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

  const _PUT = (docs, putOptions) => {
    cache.clear()

    putOptions = Object.assign(ops, putOptions)

    return DocumentProcessor(ops)
      .processDocuments(docs)
      .then(vectors => {
        return ops.fii.PUT(vectors, putOptions).then(result => {
          return Promise.all([
            _PUT_RAW(
              docs,
              result.map(r => r._id),
              !ops.storeRawDocs
            ),
            incrementDocCount(result.filter(r => r.status === 'CREATED').length)
          ]).then(() => result)
        })
      })
  }

  const _PUT_RAW = (docs, ids, dontStoreValue) => {
    cache.clear()
    return Promise.all(
      docs.map((doc, i) =>
        ops.fii.STORE.put(['DOC_RAW', ids[i]], dontStoreValue ? {} : doc)
      )
    ).then(
      // TODO: make this actually deal with errors
      result =>
        docs.map((doc, i) => ({
          _id: ids[i],
          status: 'OK',
          operation: '_PUT_RAW'
        }))
    )
  }

  const _DELETE = _ids =>
    ops.fii.DELETE(_ids).then(result => {
      cache.clear()
      const deleted = result.filter(d => d.status === 'DELETED')
      return Promise.all([
        Promise.all(deleted.map(r => ops.fii.STORE.del(['DOC_RAW', r._id]))),
        decrementDocCount(deleted.length)
      ]).then(() => result)
    })

  const _FLUSH = () =>
    ops.fii.STORE.clear()
      .then(() => {
        cache.clear()
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
    IMPORT: index => {
      cache.clear()
      return Promise.resolve(ops.fii.IMPORT(index))
    },
    PUT: (docs, pops) => queue.add(() => _PUT(docs, pops)),
    PUT_RAW: _PUT_RAW,
    _INCREMENT_DOC_COUNT: incrementDocCount
  }
}
