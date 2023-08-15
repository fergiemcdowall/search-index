import { DocumentProcessor } from './DocumentProcessor.js'

export class Writer {
  constructor(ops, cache, queue, ii) {
    this.cache = cache
    this.ii = ii
    this.ops = ops
    this.queue = queue
  }

  incrementDocCount(increment) {
    return this.ii.STORE.get(['DOCUMENT_COUNT'])
      .then(count => this.ii.STORE.put(['DOCUMENT_COUNT'], +count + increment))
      .catch(
        // if not found assume value to be 0
        e => this.ii.STORE.put(['DOCUMENT_COUNT'], increment)
      )
  }

  #decrementDocCount(decrement) {
    return this.ii.STORE.get(['DOCUMENT_COUNT']).then(count =>
      this.ii.STORE.put(['DOCUMENT_COUNT'], +count - increment)
    )
  }

  #PUT(docs, putOptions) {
    this.cache.clear()

    const ops = {
      ...this.ops,
      ...putOptions
    }

    return DocumentProcessor(ops)
      .processDocuments(docs)
      .then(vectors => {
        return this.ii
          .PUT(vectors, putOptions, this.ii.LEVEL_OPTIONS)
          .then(result => {
            return Promise.all([
              this.PUT_RAW(
                docs,
                result.map(r => r._id),
                !ops.storeRawDocs
              ),
              this.incrementDocCount(
                result.filter(r => r.status === 'CREATED').length
              )
            ]).then(() => result)
          })
      })
  }

  #DELETE(_ids) {
    return this.ii.DELETE(_ids, this.ii.LEVEL_OPTIONS).then(result => {
      cache.clear()
      const deleted = result.filter(d => d.status === 'DELETED')
      return Promise.all([
        Promise.all(
          deleted.map(r =>
            this.ii.STORE.del(['DOC_RAW', r._id], this.ii.LEVEL_OPTIONS)
          )
        ),
        decrementDocCount(deleted.length)
      ]).then(() => result)
    })
  }

  DELETE(...docIds) {
    return this.#DELETE(docIds)
  }

  FLUSH() {
    return this.ii.STORE.clear()
      .then(() => {
        cache.clear()
        const timestamp = Date.now()
        return this.ii.STORE.batch(
          [
            { type: 'put', key: ['~CREATED'], value: timestamp },
            { type: 'put', key: ['~LAST_UPDATED'], value: timestamp },
            { type: 'put', key: ['DOCUMENT_COUNT'], value: 0 }
          ],
          this.ii.LEVEL_OPTIONS
        )
      })
      .then(() => true)
  }

  IMPORT(index) {
    this.cache.clear()
    return Promise.resolve(this.ii.IMPORT(index))
  }

  PUT(docs, pops) {
    return this.queue.add(() => this.#PUT(docs, pops))
  }

  PUT_RAW(docs, ids, dontStoreValue) {
    this.cache.clear()
    return Promise.all(
      docs.map((doc, i) =>
        this.ii.STORE.put(
          ['DOC_RAW', ids[i]],
          dontStoreValue ? {} : doc,
          this.ii.LEVEL_OPTIONS
        )
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

  // TODO: does this need to be exported?
  _INCREMENT_DOC_COUNT(increment) {
    return this.incrementDocCount(increment)
  }

  // return {
  //   DELETE: (...docIds) => _DELETE(docIds), // for external use
  //   FLUSH: _FLUSH,
  //   // TODO: IMPORT needs a test
  //   IMPORT: index => {
  //     cache.clear()
  //     return Promise.resolve(ii.IMPORT(index))
  //   },
  //   PUT: (docs, pops) => queue.add(() => _PUT(docs, pops)),
  //   PUT_RAW: _PUT_RAW,
  //   _INCREMENT_DOC_COUNT: incrementDocCount
  // }
}
