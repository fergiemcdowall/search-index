var colors = require('colors');


function achieve() {console.log(('[achievement] ' + JSON.stringify(arguments[0])).cyan);}
function debug() {
  if (false)
    console.log(('[debug] ' + JSON.stringify(arguments[0])).white);
}
function error() {console.log(('[broken] ' + JSON.stringify(arguments[0])).red);}
function info() {console.log(('[information] ' + JSON.stringify(arguments[0])).green);}
function log() {console.log(('[log] ' + JSON.stringify(arguments[0])).white);}
function success() {console.log(('[success] ' + JSON.stringify(arguments[0])).blue);}
function warn() {console.log(('[warning] ' + JSON.stringify(arguments[0])).yellow);}

module.exports = {
  achieve: achieve,
  debug: debug,
  error: error,
  info: info,
  log: log,
  success: success,
  warn: warn
};
