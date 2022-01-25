const si = require('./main.js')
const leveldown = require('level-js')

module.exports = ops =>
  si(
    Object.assign(
      {
        db: leveldown
      },
      ops
    )
  )
