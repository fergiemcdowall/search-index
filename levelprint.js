var levelup = require('levelup')
var db = levelup(process.argv[2])

db.createReadStream()
  .on('data', function (data) {
    console.log(data)
  })
