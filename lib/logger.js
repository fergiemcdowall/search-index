var silent = false;

console.log('process.env.NODE_ENV', process.env.NODE_ENV);

if (process.env.NODE_ENV.toLowerCase() === 'test') {
  silent = true;
}

function log() {
  if (!silent) {
    console.log.apply(console, arguments);
  }
}

module.exports = {
  log: log
};
