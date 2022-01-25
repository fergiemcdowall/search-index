const si = require('./main.js')
const leveldown = require('leveldown')

module.exports = ops =>
  si(
    Object.assign(
      {
        db: leveldown
      },
      ops
    )
  )
