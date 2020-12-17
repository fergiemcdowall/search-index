// TODO: maintain a maximum size
// TODO: clear cache on write operations

module.exports = class Cache {
  constructor () {
    this.LRUStore = new Map()
  }

  has (key) {
    return this.LRUStore.has(key)
  }

  get (key) {
    console.log('check cache')
    const value = this.LRUStore.get(key)
    console.log('value is', value)
    if (value) {
      console.log('value found in cache')
      this.set(key, value)
    }
    return value
  }

  set (key, value) {
    console.log('setting cache...')
    console.log('deleting...')
    this.LRUStore.delete(key)
    console.log('inserting...')
    this.LRUStore.set(key, value) // bump to head
    console.log('completed.')
    return value
  }
}
