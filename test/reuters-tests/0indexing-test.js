/* global it */
/* global describe */

var assert = require('assert');
var should = require('should');
var fs = require('fs');

describe('Indexing Reuters reuters-000.json: ', function () {
  var data = [];
  var sandboxPath = 'test/sandbox';

  it('should find the data and set up a sandbox', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/full/reuters-000.json'));
    assert.equal(data.length, 1000);
    assert.equal(data[0].id, '1');
    try {
      var stats = fs.lstatSync(sandboxPath);
      assert(stats.isDirectory());
    }
    catch (e) {
      console.log(e);
      assert(false);
    }
    si.close(function (err) {
      if (err) false.should.eql(true);done();
    });
  });

  it('should index the data', function (done) {
    this.timeout(120000);
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    var opt = {};
    opt.batchName = 'reuters';
    opt.fieldOptions = [
      {fieldName: 'places', filter: true},
      {fieldName: 'topics', filter: true}
    ];
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should verify indexing', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.tellMeAboutMySearchIndex(function (info) {
      should.exist(info);
      (info.totalDocs).should.be.exactly(1000);
      info.options.indexPath.should.be.exactly('test/sandbox/si-reuters');
      info.options.logLevel.should.be.exactly('error');
      info.options.deletable.should.be.exactly(true);
      info.options.fieldedSearch.should.be.exactly(true);
      info.options.nGramLength.should.be.exactly(1);
      info.options.fieldsToStore.should.be.exactly('all');
      info.options.stopwords.should.eql(
        [ '$',
          '0',
          '1',
          '2',
          '3',
          '4',
          '5',
          '6',
          '7',
          '8',
          '9',
          '_',
          'a',
          'about',
          'after',
          'all',
          'also',
          'am',
          'an',
          'and',
          'another',
          'any',
          'are',
          'as',
          'at',
          'b',
          'be',
          'because',
          'been',
          'before',
          'being',
          'between',
          'both',
          'but',
          'by',
          'c',
          'came',
          'can',
          'come',
          'could',
          'd',
          'did',
          'do',
          'e',
          'each',
          'f',
          'for',
          'from',
          'g',
          'get',
          'got',
          'h',
          'had',
          'has',
          'have',
          'he',
          'her',
          'here',
          'him',
          'himself',
          'his',
          'how',
          'i',
          'if',
          'in',
          'into',
          'is',
          'it',
          'j',
          'k',
          'l',
          'like',
          'm',
          'make',
          'many',
          'me',
          'might',
          'more',
          'most',
          'much',
          'must',
          'my',
          'n',
          'never',
          'now',
          'o',
          'of',
          'on',
          'only',
          'or',
          'other',
          'our',
          'out',
          'over',
          'p',
          'q',
          'r',
          's',
          'said',
          'same',
          'see',
          'should',
          'since',
          'some',
          'still',
          'such',
          't',
          'take',
          'than',
          'that',
          'the',
          'their',
          'them',
          'then',
          'there',
          'these',
          'they',
          'this',
          'those',
          'through',
          'to',
          'too',
          'u',
          'under',
          'up',
          'v',
          'very',
          'w',
          'was',
          'way',
          'we',
          'well',
          'were',
          'what',
          'where',
          'which',
          'while',
          'who',
          'with',
          'would',
          'x',
          'y',
          'you',
          'your',
          'z' ]);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

});
