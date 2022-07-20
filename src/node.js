const si = require('./main.js')
const { ClassicLevel } = require('classic-level')

module.exports = ops =>
  si(
    Object.assign(
      {
        db: ClassicLevel
      },
      ops
    )
  )
