import { DocumentProcessor } from './DocumentProcessor.js'
import PQueue from 'p-queue'

export class Writer {
  #cache
  #ii
  #ops
  #queue

  constructor (ops, cache, ii) {
    this.#cache = cache
    this.#ii = ii
    this.#ops = ops
    this.#queue = new PQueue({ concurrency: 1 })
  }

  #incrementDocCount (increment) {
    return this.#ii.STORE.get(['DOCUMENT_COUNT'])
      .then(count => this.#ii.STORE.put(['DOCUMENT_COUNT'], +count + increment))
      .catch(
        // if not found assume value to be 0
        e => this.#ii.STORE.put(['DOCUMENT_COUNT'], increment)
      )
  }

  // TODO: should this be a separate function?
  #decrementDocCount (decrement) {
    return this.#ii.STORE.get(['DOCUMENT_COUNT']).then(count =>
      this.#ii.STORE.put(['DOCUMENT_COUNT'], +count - decrement)
    )
  }

  #PUT (docs, putOptions) {
    this.#cache.clear()
    const ops = {
      ...this.#ops,
      ...putOptions
    }
    return DocumentProcessor(ops)
      .processDocuments(docs)
      .then(vectors => this.#ii.PUT(vectors, putOptions))
      .then(result =>
        this.PUT_RAW(
          docs,
          result.map(r => r._id),
          !ops.storeRawDocs
        )
          .then(() =>
            this.#incrementDocCount(
              result.filter(r => r.status === 'CREATED').length
            )
          )
          .then(() => result)
      )
  }

  #DELETE (_ids) {
    return this.#ii.DELETE(_ids).then(result =>
      this.DELETE_RAW(..._ids)
        .then(() =>
          this.#decrementDocCount(
            result.filter(d => d.status === 'DELETED').length
          )
        )
        .then(() => this.#cache.clear())
        .then(() => result)
    )
  }

  DELETE (...docIds) {
    return this.#DELETE(docIds)
  }

  DELETE_RAW (...docIds) {
    return Promise.all(docIds.map(id => this.#ii.STORE.del(['DOC_RAW', id])))
  }

  FLUSH () {
    return this.#ii.STORE.clear()
      .then(() => {
        this.#cache.clear()
        const timestamp = Date.now()
        return this.#ii.STORE.batch([
          { type: 'put', key: ['~CREATED'], value: timestamp },
          { type: 'put', key: ['~LAST_UPDATED'], value: timestamp },
          { type: 'put', key: ['DOCUMENT_COUNT'], value: 0 }
        ])
      })
      .then(() => true)
  }

  IMPORT (index) {
    this.#cache.clear()
    return Promise.resolve(this.#ii.IMPORT(index))
  }

  PUT (docs, pops) {
    return this.#queue.add(() => this.#PUT(docs, pops))
  }

  PUT_RAW (docs, ids, dontStoreValue) {
    this.#cache.clear()
    return Promise.all(
      docs.map((doc, i) =>
        this.#ii.STORE.put(['DOC_RAW', ids[i]], dontStoreValue ? {} : doc)
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
  _INCREMENT_DOC_COUNT (increment) {
    return this.#incrementDocCount(increment)
  }
}
