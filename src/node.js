const si = require('./main.js')
const { MemoryLevel } = require('memory-level')
module.exports = ops => {
  const defaultDb = new MemoryLevel({ valueEncoding: 'json' })
  return si(
    Object.assign(
      { db: defaultDb },
      ops
    )
  )
}
