/* global it */
/* global describe */

const Readable = require('stream').Readable
const SearchIndex = require('../../../')
const logLevel = process.env.NODE_ENV || 'error'
const should = require('should')

var s = new Readable({ objectMode: true })

describe('Matching epub: ', function () {
  var index

  it('should initialize the first search index', function (done) {
    SearchIndex({
      indexPath: 'test/sandbox/si-epub-matching-test',
      logLevel: logLevel
    }, function (err, thisIndex) {
      should.not.exist(err)
      index = thisIndex
      done()
    })
  })

  it('should index test data into the index', function (done) {
    s.push({
      id: 'doc101',
      title: 'Accessible EPUB 3',
      body: 'EPUB is great. epubxhighestsort',
      spineItemPath: 'epub_content/accessible_epub_3/EPUB/ch03s06.xhtml'
    })
    s.push({
      id: 'doc102',
      title: 'Even More Accessible EPUBulation 3',
      body: 'EPUB is epubtastic, epubxhighestsort',
      spineItemPath: 'epub_content/accessible_epub_3/EPUB/ch03s07.xhtml'
    })
    s.push({
      id: 'doc103',
      title: 'EPUB 3 FTW',
      body: 'EPUB is fantabulous, epubxhighestsort',
      spineItemPath: 'epub_content/accessible_epub_3/EPUB/ch03s08.xhtml'
    })
    s.push({
      id: 'doc104',
      title: '中文的标题',
      body: '中文的字符',
      spineItemPath: 'epub_content/accessible_epub_3/EPUB/ch03s09.xhtml'
    })
    s.push({
      id: 'doc105',
      title: 'another doc',
      body: 'make epubxhighestsort the most common TF term',
      spineItemPath: 'epub_content/accessible_epub_4/EPUB/ch03s09.xhtml'
    })
    s.push(null)
    s.pipe(index.defaultPipeline({
      batchName: 'epubdata',
      fieldOptions: {
        id: {
          searchable: false
        },
        spineItemPath: {
          searchable: false
        }
      }
    }))
      .pipe(index.add())
      .on('data', function (data) {
        // nowt
      })
      .on('end', function () {
        true.should.be.exactly(true)
        return done()
      })
  })

  it('should match on given field and get results, id not searchable', function (done) {
    var matches = [
      'epubxhighestsort',
      'epub',
      'epubtastic'
    ]
    index.match({
      beginsWith: 'epub',
      field: 'body'
    }).on('data', function (data) {
      data.should.be.exactly(matches.shift())
    }).on('end', function () {
      done()
    })
  })

  it('should match on all fields and get results, id not searchable', function (done) {
    var matches = [
      'epubxhighestsort',
      'epub',
      'epubtastic',
      'epubulation'
    ]
    index.match({
      beginsWith: 'epub'
    }).on('data', function (data) {
      data.should.be.exactly(matches.shift())
    }).on('end', function () {
      done()
    })
  })

  it('should match on all fields and return IDs', function (done) {
    var matches = [ [ 'epubxhighestsort', [ 'doc101', 'doc102', 'doc103', 'doc105' ] ],
      [ 'epub', [ 'doc101', 'doc102', 'doc103' ] ],
      [ 'epubtastic', [ 'doc102' ] ],
      [ 'epubulation', [ 'doc102' ] ] ]
    index.match({
      beginsWith: 'epub',
      type: 'ID'
    }).on('data', function (data) {
      data.should.eql(matches.shift())
    }).on('end', function () {
      done()
    })
  })

  it('should match on all fields and return IDs and counts', function (done) {
    var matches = [ [ 'epubxhighestsort', 4 ],
      [ 'epub', 3 ],
      [ 'epubtastic', 1 ],
      [ 'epubulation', 1 ] ]
    index.match({
      beginsWith: 'epub',
      type: 'count'
    }).on('data', function (data) {
      data.should.eql(matches.shift())
    }).on('end', function () {
      done()
    })
  })

  it('should match on all fields and get results, and set limit', function (done) {
    var matches = ['epubxhighestsort']
    var count = 0
    index.match({
      beginsWith: 'epub',
      limit: 1
    }).on('data', function (data) {
      ++count
      data.should.eql(matches.shift())
    }).on('end', function () {
      count.should.be.exactly(1)
      done()
    })
  })

  it('should match on body field and get results', function (done) {
    var matches = [
      'epubxhighestsort',
      'epub',
      'epubtastic'
    ]
    var count = 0
    index.match({
      beginsWith: 'epub',
      field: 'body'
    }).on('data', function (data) {
      ++count
      data.should.eql(matches.shift())
    }).on('end', function () {
      count.should.be.exactly(3)
      done()
    })
  })

  it('should match on title field and get results', function (done) {
    var matches = [
      'epub',
      'epubulation'
    ]
    var count = 0
    index.match({
      beginsWith: 'epub',
      field: 'title'
    }).on('data', function (data) {
      ++count
      data.should.eql(matches.shift())
    }).on('end', function () {
      count.should.be.exactly(2)
      done()
    })
  })

  it('should work for Unicode', function (done) {
    var matches = [
      '中文的字符',
      '中文的标题'
    ]
    var count = 0
    index.match({
      beginsWith: '中文的'
    }).on('data', function (data) {
      ++count
      data.should.eql(matches.shift())
    }).on('end', function () {
      count.should.be.exactly(2)
      done()
    })
  })

  it('Can reduce threshold', function (done) {
    var matches = [
      'epubxhighestsort',
      'epub',
      'epubtastic',
      'epubulation'
    ]
    var count = 0
    index.match({
      beginsWith: 'ep',
      threshold: 1
    }).on('data', function (data) {
      ++count
      data.should.eql(matches.shift())
    }).on('end', function () {
      count.should.be.exactly(4)
      done()
    })
  })

  it('handles match strings that are empty', function (done) {
    var count = 0
    index.match({
      beginsWith: ''
    }).on('data', function (data) {
      count++
    }).on('end', function () {
      count.should.be.exactly(0)
      done()
    })
  })

  it('handles malformed options object', function (done) {
    var str = 'this string should be an object'
    var count = 0
    index.match(
      str
    ).on('data', function (data) {
      count++
    }).on('end', function () {
      count.should.be.exactly(0)
      done()
    })
  })

  it('Handles string length below threshold', function (done) {
    index.match({
      beginsWith: 'ep'
    }).on('data', function (data) {
      console.log(data)
    }).on('error', function (err) {
      console.log(err)
    }).on('end', function () {
      done()
    })
  })
})
