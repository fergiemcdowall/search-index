const si = require('./main.js')
const { BrowserLevel } = require('browser-level')

module.exports = ops =>
  si(
    Object.assign(
      {
        db: BrowserLevel
      },
      ops
    )
  )
