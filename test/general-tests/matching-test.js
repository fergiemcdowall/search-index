/* global it */
/* global describe */

var logLevel = 'error';
if (process.env.NODE_ENV == 'TEST') logLevel = 'info';
var should = require('should');
var sandboxPath = 'test/sandbox';

describe('Matching epub: ', function () {
  var si = require('../../')({indexPath: sandboxPath + '/si-epub-matching-test',
                              logLevel: logLevel});

  it('should index test data into the index', function (done) {
    var data = [
      {
        id: 'doc101',
        title: 'Accessible EPUB 3',
        body: 'EPUB is great. epubxhighestsort',
        spineItemPath: 'epub_content/accessible_epub_3/EPUB/ch03s06.xhtml'
      },
      {
        id: 'doc102',
        title: 'Even More Accessible EPUBulation 3',
        body: 'EPUB is epubtastic, epubxhighestsort',
        spineItemPath: 'epub_content/accessible_epub_3/EPUB/ch03s07.xhtml'
      },
      {
        id: 'doc103',
        title: 'EPUB 3 FTW',
        body: 'EPUB is fantabulous, epubxhighestsort',
        spineItemPath: 'epub_content/accessible_epub_3/EPUB/ch03s08.xhtml'
      },
      {
        id: 'doc104',
        title: '中文的标题',
        body: '中文的字符',
        spineItemPath: 'epub_content/accessible_epub_3/EPUB/ch03s09.xhtml'
      },
      {
        id: 'doc105',
        title: 'another doc',
        body: 'make epubxhighestsort the most common TF term',
        spineItemPath: 'epub_content/accessible_epub_4/EPUB/ch03s09.xhtml'
      }
    ];
    si.add(
      data,
      {
        batchName: 'epubdata',
        fieldOptions: [{
          fieldName: 'id',
          searchable: false
        }, {
          fieldName: 'spineItemPath',
          searchable: false
        }]
      }, function (err) {
        (err === null).should.be.exactly(true);
        done();
      });
  });

  it('should match on all fields and get results', function (done) {
    var str = 'epub';
    si.match({beginsWith: str}, function (err, matches) {
      should.exist(matches);
      (err === null).should.be.exactly(true);
      matches.length.should.be.exactly(4);
      matches[0].should.be.exactly('epubxhighestsort');
      matches[1].should.be.exactly('epub');
      matches[2].should.be.exactly('epubtastic');
      matches[3].should.be.exactly('epubulation');
      done();
    });
  });

  it('should match on all fields and return IDs', function (done) {
    var str = 'epub';
    si.match({beginsWith: str, type: 'ID'}, function (err, matches) {
      should.exist(matches);
      (err === null).should.be.exactly(true);
      matches.should.eql(
        [ [ 'epubxhighestsort', [ 'doc101', 'doc102', 'doc103', 'doc105' ] ],
          [ 'epub', [ 'doc101', 'doc102', 'doc103' ] ],
          [ 'epubtastic', [ 'doc102' ] ],
          [ 'epubulation', [ 'doc102' ] ] ]
      );
      done();
    });
  });

  it('should match on all fields and return IDs', function (done) {
    var str = 'epub';
    si.match({beginsWith: str, type: 'count'}, function (err, matches) {
      should.exist(matches);
      (err === null).should.be.exactly(true);
      matches.should.eql(
        [ [ 'epubxhighestsort', 4 ],
          [ 'epub', 3 ],
          [ 'epubtastic', 1 ],
          [ 'epubulation', 1 ] ]
      );
      done();
    });
  });

  it('should match on all fields and get results, and set limit', function (done) {
    var str = 'epub';
    si.match({beginsWith: str, limit: 1}, function (err, matches) {
      should.exist(matches);
      (err === null).should.be.exactly(true);
      matches.length.should.be.exactly(1);
      matches[0].should.be.exactly('epubxhighestsort');
      done();
    });
  });

  it('should match on body field and get results', function (done) {
    var str = 'epub';
    si.match({beginsWith: str, field: 'body'}, function (err, matches) {
      should.exist(matches);
      (err === null).should.be.exactly(true);
      matches.length.should.be.exactly(3);
      matches[0].should.be.exactly('epubxhighestsort');
      matches[1].should.be.exactly('epub');
      matches[2].should.be.exactly('epubtastic');
      done();
    });
  });

  it('should match on title field and get results', function (done) {
    var str = 'epub';
    si.match({beginsWith: str, field: 'title'}, function (err, matches) {
      should.exist(matches);
      (err === null).should.be.exactly(true);
      matches.length.should.be.exactly(2);
      matches[0].should.be.exactly('epub');
      matches[1].should.be.exactly('epubulation');
      done();
    });
  });

  it('should work for Unicode', function (done) {
    var str = '中文的';
    si.match({beginsWith: str}, function (err, matches) {
      should.exist(matches);
      (err === null).should.be.exactly(true);
      matches.length.should.be.exactly(2);
      matches.should.containEql('中文的标题');
      matches.should.containEql('中文的字符');
      done();
    });
  });

  it('handles match strings that are empty', function (done) {
    var str = '';
    si.match({beginsWith: str}, function (err, matches) {
      should.exist(matches);
      matches.length.should.be.exactly(0);
      (err instanceof Error).should.be.exactly(true);
      err.toString().should.be.exactly('Error: match string can not be empty');
      done();
    });
  });

  it('handles malformed options object', function (done) {
    var str = 'this string should be an object';
    si.match(str, function (err, matches) {
      should.exist(matches);
      matches.length.should.be.exactly(0);
      (err instanceof Error).should.be.exactly(true);
      err.toString().should.be.exactly('Error: Options should be an object');
      done();
    });
  });

  it('Throws error if below threshold', function (done) {
    var str = 'ep';
    si.match({beginsWith: str}, function (err, matches) {
      should.exist(matches);
      matches.length.should.be.exactly(0);
      (err instanceof Error).should.be.exactly(true);
      err.toString().should.be.exactly('Error: match string must be longer than threshold (3)');
      done();
    });
  });

  it('Can reduce threshold', function (done) {
    var str = 'ep';
    si.match({beginsWith: str, threshold: 1}, function (err, matches) {
      should.exist(matches);
      matches.length.should.be.exactly(4);
      (err instanceof Error).should.be.exactly(false);
      matches.should.eql([ 'epubxhighestsort', 'epub', 'epubtastic', 'epubulation' ]);
      done();
    });
  });

});
