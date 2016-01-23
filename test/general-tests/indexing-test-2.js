/* global it */
/* global describe */

var fs = require('fs')
var should = require('should'); // jshint ignore:line
var sandboxPath = 'test/sandbox'

var batch = [
  {
    id: 'a',
    title: 'The Beatles',
    content: 'The Beatles were an English rock band, formed in Liverpool in 1960. Beatles from Liverpool',
    year: ['1960', '1961', '1962']
  },
  {
    id: 'b',
    title: 'The Rolling Stones',
    content: 'The Rolling Stones are an English rock band formed in London in 1962.',
    year: ['1962', '1963', '1964']
  },
  {
    id: 'c',
    title: 'Pink Floyd',
    ignoreThis: 'bob loblaws law blog',
    content: 'Pink Floyd were an English rock band formed in London London London. Pink Floyd achieved international acclaim with their progressive and psychedelic music.',
    year: ['1963', '1964', '1965']
  }
]

describe('Indexing API', function (done) { // jshint ignore:line
  var si = require('../../')({
    indexPath: sandboxPath + '/indexing-test-2',
    logLevel: 'warn',
    fieldsToStore: ['id', 'title', 'content'],
    fieldOptions: [{fieldName: 'year', filter: true}]
  })
  it('should do some simple indexing', function (done) {
    si.add(batch, {}, function (err) {
      (err === null).should.be.exactly(true)

      si.snapShot(function (rs) {
        rs.pipe(fs.createWriteStream(sandboxPath + '/backup.gz'))
          .on('close', function () {
            (true).should.be.exactly(true)
            si.close(function (err) {
              if (err) false.should.eql(true)
              done()
            })
          })
          .on('error', function (err) {
            (err === null).should.be.exactly(true)
          })
      })
    })
  })
})
