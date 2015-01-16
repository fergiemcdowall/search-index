var winston = require('winston');

var searchIndexLogger = module.exports =  new (winston.Logger)({
  transports: [
    new (winston.transports.Console)({})
  ]
});
