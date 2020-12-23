// A simple LRU cache

module.exports = class Cache {
  constructor (limit = 1000) {
    this.limit = limit // max entries in cache
    this.LRUStore = new Map()
  }

  // check if entry exists for given key
  has (key) {
    return this.LRUStore.has(key)
  }

  // get cached entry, return and promote entry if found
  get (key) {
    const value = this.LRUStore.get(key)
    if (value) {
      // bump value to head of Map as most recently used
      this.set(key, value)
    }
    return value
  }

  // new entry, trim oldest entry if cache length exceeds limit
  set (key, value) {
    // if size limit reached, delete oldest entry
    if (this.LRUStore.size === this.limit) { this.LRUStore.delete(Array.from(this.LRUStore.keys()).shift()) }
    // remove key from current position in cache (if present)
    this.LRUStore.delete(key)
    this.LRUStore.set(key, value)
    return value
  }

  // check if query is cached if it is return/promote, if not then add
  cache (key, q) {
    key = JSON.stringify(key)
    return this.has(key)
      ? new Promise(resolve => resolve(this.get(key)))
      : q.then(
        result => this.set(key, result)
      )
  }

  // clear all entries in cache
  flush (operation) {
    this.LRUStore = new Map()
    return operation
  }
}
