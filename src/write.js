const DocumentProcessor = require('./DocumentProcessor')

module.exports = (ops, cache, queue) => {
  const incrementDocCount = increment =>
    ops.fii.STORE.get(['DOCUMENT_COUNT'], ops.fii.LEVEL_OPTIONS)
      .then(count => ops.fii.STORE.put(['DOCUMENT_COUNT'], +count + increment, ops.fii.LEVEL_OPTIONS))
      .catch(
        // if not found assume value to be 0
        e => ops.fii.STORE.put(['DOCUMENT_COUNT'], increment, ops.fii.LEVEL_OPTIONS)
      )

  const decrementDocCount = increment =>
    ops.fii.STORE.get(['DOCUMENT_COUNT'], ops.fii.LEVEL_OPTIONS).then(count =>
      ops.fii.STORE.put(['DOCUMENT_COUNT'], +count - increment, ops.fii.LEVEL_OPTIONS)
    )

  const _PUT = (docs, putOptions) => {
    cache.clear()

    putOptions = Object.assign(ops, putOptions)

    return DocumentProcessor(ops)
      .processDocuments(docs)
      .then(vectors => {
        return ops.fii.PUT(vectors, putOptions, ops.fii.LEVEL_OPTIONS).then(result => {
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
        ops.fii.STORE.put(['DOC_RAW', ids[i]], dontStoreValue ? {} : doc, ops.fii.LEVEL_OPTIONS)
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
    ops.fii.DELETE(_ids, ops.fii.LEVEL_OPTIONS).then(result => {
      cache.clear()
      const deleted = result.filter(d => d.status === 'DELETED')
      return Promise.all([
        Promise.all(deleted.map(r => ops.fii.STORE.del(['DOC_RAW', r._id], ops.fii.LEVEL_OPTIONS))),
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
        ], ops.fii.LEVEL_OPTIONS)
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
