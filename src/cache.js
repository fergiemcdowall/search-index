// A simple LRU cache

module.exports = class Cache {
  constructor (limit = 1000) {
    this.limit = limit
    this.LRUStore = new Map()
  }

  has (key) {
    return this.LRUStore.has(key)
  }

  get (key) {
    const value = this.LRUStore.get(key)
    if (value) {
      // bump value to head of Map as most recently used
      this.set(key, value)
    }
    return value
  }

  set (key, value) {
    // if size limit reached, delete oldest entry
    if (this.LRUStore.size === this.limit) { this.LRUStore.delete(Array.from(this.LRUStore.keys()).shift()) }
    // remove key from current position in cache (if present)
    this.LRUStore.delete(key)
    this.LRUStore.set(key, value)
    return value
  }

  cache (key, q) {
    key = JSON.stringify(key)
    return this.has(key)
      ? new Promise(resolve => resolve(this.get(key)))
      : q.then(
        result => this.set(key, result)
      )
  }

  flush (operation) {
    this.LRUStore = new Map()
    return operation
  }
}
