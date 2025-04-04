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
    // Initialize document count (async fire and forget)
    this.#incrementDocCount(0)
  }

  // TODO: should be queued
  #incrementDocCount = (increment = 0) =>
    this.#ii.STORE.get(['DOCUMENT_COUNT']).then((count = 0) =>
      this.#ii.STORE.put(['DOCUMENT_COUNT'], +count + +increment)
    )

  #PUT = (docs, putOptions) => {
    this.#cache.clear()
    const ops = {
      ...this.#ops,
      ...putOptions
    }
    return DocumentProcessor(docs, ops)
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

  #DELETE = _ids =>
    this.#ii.DELETE(_ids).then(result =>
      this.DELETE_RAW(..._ids)
        .then(() =>
          this.#incrementDocCount(
            -result.filter(d => d.status === 'DELETED').length
          )
        )
        .then(() => this.#cache.clear())
        .then(() => result)
    )

  DELETE = (...docIds) => this.#DELETE(docIds)

  DELETE_RAW = (...docIds) =>
    Promise.all(
      docIds.map(id => this.#ii.STORE.del([this.#ops.docExistsSpace, id]))
    )

  FLUSH = () =>
    this.#ii.STORE.clear()
      .then(() => {
        this.#cache.clear()
        //        const timestamp = Date.now()
        const timestamp = new Date().toISOString()
        return this.#ii.STORE.batch([
          { type: 'put', key: ['~CREATED'], value: timestamp },
          { type: 'put', key: ['~LAST_UPDATED'], value: timestamp },
          { type: 'put', key: ['DOCUMENT_COUNT'], value: 0 }
        ])
      })
      .then(() => true)

  IMPORT = index => {
    this.#cache.clear()
    return Promise.resolve(this.#ii.IMPORT(index))
  }

  PUT = (docs, pops) => this.#queue.add(() => this.#PUT(docs, pops))

  PUT_RAW = (docs, ids, dontStoreValue) =>
    Promise.all(
      docs.map((doc, i) =>
        this.#ii.STORE.put(
          [this.#ops.docExistsSpace, ids[i]],
          dontStoreValue ? {} : doc
        )
      )
    ).then(
      // TODO: make this actually deal with errors
      result => {
        this.#cache.clear()
        return docs.map((doc, i) => ({
          _id: ids[i],
          status: 'OK',
          operation: '_PUT_RAW'
        }))
      }
    )
}
