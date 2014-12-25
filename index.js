winston = require('winston');

//global <- is there a better way of doing this
searchIndexLogger = new (winston.Logger)({
  transports: [
  new (winston.transports.Console)({ level: 'error' }),
  //    new (winston.transports.File)({ filename: 'somefile.log' })
  ]
});

module.exports = require('./lib/search-index.js');
