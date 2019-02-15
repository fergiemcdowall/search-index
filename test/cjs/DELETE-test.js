'use strict';

function _interopDefault (ex) { return (ex && (typeof ex === 'object') && 'default' in ex) ? ex['default'] : ex; }

var fii = _interopDefault(require('fergies-inverted-index'));
var trav = _interopDefault(require('traverse'));
var tv = _interopDefault(require('term-vector'));
var test = _interopDefault(require('tape'));
require('world-bank-dataset');

function util (fii$$1) {
  const prefetchSearchableFields = () => {
    const tmp = [];
    return new Promise((resolve, reject) => {
      fii$$1.STORE.createKeyStream({
        gte: '￮FIELD!',
        lte: '￮FIELD￮￮'
      }).on('data', d => tmp.push(d.split('￮')[2]))
        .on('end', () => resolve(global.searchableFields = tmp));
    })
  };

  const countDocs = () => {
    var i = 0;
    return new Promise((resolve, reject) => {
      fii$$1.STORE.createKeyStream({
        gte: '￮DOC￮!',
        lte: '￮DOC￮￮'
      }).on('data', d => i++)
        .on('end', () => resolve(global.D = i));
    })
  };

  const calibrate = () => {
    // can handle lazy opening
    if (fii$$1.STORE.isOpen()) {
      return prefetchSearchableFields().then(countDocs)
    } else setTimeout(calibrate, 1000); // will rerun function every 1000ms until fii.STORE.isOpen()
  };

  return {
    countDocs: countDocs,
    prefetchSearchableFields: prefetchSearchableFields,
    calibrate: calibrate
  }
}

function writer (fii$$1) {
  const invertDoc = function (obj) {
    var invertedDoc = {};
    // take a plain old JSON object and parse out all of the leaf-nodes
    trav(obj).forEach(function (node) {
      if (typeof node === 'undefined') return
      var that = this;
      var searchable = true;
      this.path.forEach(item => {
        // denotes that a field is indexable
        if (item === '_id') searchable = false;
        if (item.substring(0, 1) === '!') searchable = false;
      });
      if (searchable && this.isLeaf) {
        invertedDoc[that.path.filter(item => {  // eslint-disable-line
          return isNaN(item)
        }).join('.')] = (that.node + '').split(' ');
      }
    });
    return invertedDoc
  };

  // calculate term frequency on flattened object
  const calculateTermFrequency = invertedDoc => Object.keys(invertedDoc).reduce(
    (newInvertedDoc, key) => {
      // for fields that are length 1 with a number return number
      // (trying to find a sensible way to deal with numbers)
      if ((invertedDoc[key].length === 1) && (!isNaN(invertedDoc[key][0]))) {
        // invertedDoc[key] = {'_SELF': invertedDoc[key][0]}
        const val = invertedDoc[key][0];
        newInvertedDoc[key] = {};
        newInvertedDoc[key][val] = val;
        return newInvertedDoc
      }
      // for all other fields generate term frequency
      newInvertedDoc[key] = tv(invertedDoc[key]).reduce((acc, cur, i, arr) => {
        // TODO: make scoring precision an option
        acc[cur.term] = (cur.positions.length / arr.length).toFixed(2);
        return acc
      }, {});
      return newInvertedDoc
    }, {});

  const addSearchableFields = iDocs => new Promise((resolve, reject) => {
    const fields = new Set([].concat.apply([], iDocs.map(Object.keys)));
    fields.delete('_id'); // id not searchable
    fields.delete('!doc'); // !doc not searchable
    fii$$1.STORE.batch(Array.from(fields).map(f => {
      return {
        type: 'put',
        key: '￮FIELD￮' + f + '￮',
        value: true
      }
    }), err => {
      if (err) console.log(err);
      util(fii$$1).calibrate().then(resolve);
    });
  });

  const PUT = docs => fii$$1.PUT(docs
    .map(invertDoc)
    .map(calculateTermFrequency)
    .map(
      (doc, i) => {
        doc._id = docs[i]._id;
        doc['!doc'] = docs[i];
        return doc
      }
    )).then(addSearchableFields);

  return {
    DELETE: (..._ids) => fii$$1.DELETE(..._ids),
    PUT: PUT
  }
}

// TODO: put in some defaults
function TFIDF (ops) {
  const calculateScore = (x, i, resultset) => {
    const idf = Math.log((global.D + 1) / resultset.length);
    x.score = +x.match.reduce(
      (acc, cur) => acc + idf * +cur.split(':')[1], 0
    ).toFixed(2); // TODO: make precision an option
    return x
  };
  return ops
    .resultSet
    .map(calculateScore)
  // sort by score descending
    .sort((a, b) => b.score - a.score)
  // limit to n hits
    .slice(ops.offset, ops.limit)
}

// TODO: put in some defaults
function numericField (ops) {
  const calculateScore = (x, i, resultset) => {
    x.score = +x.match.filter(
      item => item.startsWith(ops.fieldName)
    )[0].split(':')[1];
    return x
  };
  return ops
    .resultSet
    .map(calculateScore)
  // sort by score descending
    .sort(ops.sort)
  // limit to n hits
    .slice(ops.offset, ops.limit)
}

function reader (fii$$1) {
//  const SCORE = require('./scorers.js')

  const flatten = arr => [].concat.apply([], arr);

  const flattenMatch = result => result.map(x => {
    x.match = flatten(x.match); // flatten
    x.match = flatten(x.match); // flatten again
    return x
  });

  const DICTIONARY = prefix => new Promise((resolve, reject) => {
    const dict = new Set();
    const ks = fii$$1.STORE.createKeyStream({
      gte: prefix || '',
      lte: (prefix || '') + '￮'
    });
    ks.on('data', d => dict.add(d.split(':')[0].split('.').pop()));
    ks.on('end', () => resolve(Array.from(dict).sort()));
  });

  const DOCUMENTS = hits => new Promise(
    (resolve, reject) =>
      fii$$1.OBJECT(hits).then(
        documents => resolve(hits.map((hit, i) => {
          hit.obj = documents[i]['!doc'];
          return hit
        }))
      )
  );

  const AND = function (...keys) {
    return fii$$1.AND(
      ...keys.map(GET)
    ).then(flattenMatch)
  };

  const SEARCH = (...q) => AND(...q)
    .then(resultSet => TFIDF({
      resultSet: resultSet,
      offset: 0,
      limit: 10
    }))
    .then(resultSet => DOCUMENTS(resultSet));

  const OR = (...q) => fii$$1.OR(
    ...flatten(q.map(GET))
  ).then(flattenMatch);

  // NOT
  const SET_DIFFERENCE = (a, b) => {
    if (typeof a === 'string') a = GET(a);
    if (typeof b === 'string') b = GET(b);
    return Promise.all([a, b]).then(result => {
      var [ a, b ] = result;
      b = b.map(item => item._id);
      return a.filter(item => b.indexOf(item._id))
    })
  };

  const GET = clause => {
    // could be a nested AND/OR/something else
    if (clause instanceof Promise) return clause
    // ELSE wildcard (*) search
    if (clause.slice(-2) === ':*') return fii$$1.GET(clause.replace(':*', '.'))
    // ELSE a clause with a specified field ("<fieldpath>:clause")
    if (clause.indexOf(':') > -1) return fii$$1.GET(clause.replace(':', '.') + ':')
    // ELSE a clause without specified field ("clause")
    return OR(...global.searchableFields.map(f => f + ':' + clause))
  };

  const DISTINCT = term => fii$$1.DISTINCT(term).then(result => {
    return [...result.reduce((acc, cur) => {
      acc.add(cur.split(':')[0]);
      return acc
    }, new Set())]
  });

  return {
    AND: AND,
    BUCKET: fii$$1.BUCKET,
    BUCKETFILTER: fii$$1.BUCKETFILTER,
    DICTIONARY: DICTIONARY,
    DISTINCT: DISTINCT,
    DOCUMENTS: DOCUMENTS,
    GET: GET,
    OR: OR,
    SCORENUMERIC: numericField,
    SCORETFIDF: TFIDF,
    SEARCH: SEARCH,
    SET_DIFFERENCE: SET_DIFFERENCE
  }
}

global.D = 0; // total docs in index
global.searchableFields = []; // fields that are available for searching

const makeASearchIndex = idx => {
  const w = writer(idx);
  const r = reader(idx);
  return {
    AND: r.AND,
    BUCKET: r.BUCKET,
    BUCKETFILTER: r.BUCKETFILTER,
    DELETE: w.DELETE,
    DICTIONARY: r.DICTIONARY,
    DISTINCT: r.DISTINCT,
    DOCUMENTS: r.DOCUMENTS,
    GET: r.GET,
    INDEX: idx,
    NOT: r.SET_DIFFERENCE,
    OR: r.OR,
    PUT: w.PUT,
    SCORENUMERIC: r.SCORENUMERIC,
    SCORETFIDF: r.SCORETFIDF,
    SEARCH: r.SEARCH
  }
};

function main (ops, callback) {
  // if no callback then return lazy load
  if (!callback) {
    let idx = ops.fii || fii(ops);
    // lazy calibration
    util(idx).calibrate();
    return makeASearchIndex(idx)
  } else {
    fii(ops, (err, idx) => {
      util(idx).calibrate()
        .then(() => callback(err, makeASearchIndex(idx)));
    });
  }
}

const sandbox = 'test/sandbox/';
const indexName = sandbox + 'DELETE-TEST';

const data = [
  {
    _id: 'a',
    title: 'quite a cool document',
    body: {
      text: 'this document is really cool cool cool',
      metadata: 'coolness documentness'
    },
    importantNumber: 5000
  },
  {
    _id: 'b',
    title: 'quite a cool document',
    body: {
      text: 'this document is really cool bananas',
      metadata: 'coolness documentness'
    },
    importantNumber: 500
  },
  {
    _id: 'c',
    title: 'something different',
    body: {
      text: 'something totally different',
      metadata: 'coolness documentness'
    },
    importantNumber: 200
  }
];

test('create a search index', t => {
  t.plan(1);
  global[indexName] = main({ name: indexName });
  t.pass('ok');
});

test('give lazy loading some time to complete', t => {
  t.plan(1);
  setTimeout(t.pass, 500);
});

test('can add some worldbank data', t => {
  t.plan(1);
  global[indexName].PUT(data).then(() => {
    t.pass('ok');
  });
});

test('can DELETE', t => {
  t.plan(1);
  global[indexName].DELETE([ 'b' ]).then((res) => t.looseEqual(res, [
    {
      'title': {
        'a': '0.25',
        'cool': '0.25',
        'document': '0.25',
        'quite': '0.25'
      },
      'body.text': {
        'bananas': '0.17',
        'cool': '0.17',
        'document': '0.17',
        'is': '0.17',
        'really': '0.17',
        'this': '0.17'
      },
      'body.metadata': {
        'coolness': '0.50',
        'documentness': '0.50'
      },
      'importantNumber': {
        '500': '500'
      },
      '_id': 'b',
      '!doc': {
        '_id': 'b',
        'title': 'quite a cool document',
        'body': {
          'text': 'this document is really cool bananas',
          'metadata': 'coolness documentness'
        },
        'importantNumber': 500
      }
    }
  ]));
});

test('verify DELETE', t => {
  t.plan(1);
  const indexStructure = [];
  const expectedIndexStructure = [
    { 'key': 'body.metadata.coolness:0.50', 'value': ['a', 'c'] },
    { 'key': 'body.metadata.documentness:0.50', 'value': ['a', 'c'] },
    { 'key': 'body.text.cool:0.60', 'value': ['a'] },
    { 'key': 'body.text.different:0.33', 'value': ['c'] },
    { 'key': 'body.text.document:0.20', 'value': ['a'] },
    { 'key': 'body.text.is:0.20', 'value': ['a'] },
    { 'key': 'body.text.really:0.20', 'value': ['a'] },
    { 'key': 'body.text.something:0.33', 'value': ['c'] },
    { 'key': 'body.text.this:0.20', 'value': ['a'] },
    { 'key': 'body.text.totally:0.33', 'value': ['c'] },
    { 'key': 'importantNumber.200:200', 'value': ['c'] },
    { 'key': 'importantNumber.5000:5000', 'value': ['a'] },
    { 'key': 'title.a:0.25', 'value': ['a'] },
    { 'key': 'title.cool:0.25', 'value': ['a'] },
    { 'key': 'title.different:0.50', 'value': ['c'] },
    { 'key': 'title.document:0.25', 'value': ['a'] },
    { 'key': 'title.quite:0.25', 'value': ['a'] },
    { 'key': 'title.something:0.50', 'value': ['c'] },
    { 'key': '￮DOC￮a￮', 'value': { 'title': { 'a': '0.25', 'cool': '0.25', 'document': '0.25', 'quite': '0.25' }, 'body.text': { 'cool': '0.60', 'document': '0.20', 'is': '0.20', 'really': '0.20', 'this': '0.20' }, 'body.metadata': { 'coolness': '0.50', 'documentness': '0.50' }, 'importantNumber': { '5000': 5000 }, '_id': 'a', '!doc': { '_id': 'a', 'title': 'quite a cool document', 'body': { 'text': 'this document is really cool cool cool', 'metadata': 'coolness documentness' }, 'importantNumber': 5000 } } },
    { 'key': '￮DOC￮c￮', 'value': { 'title': { 'different': '0.50', 'something': '0.50' }, 'body.text': { 'different': '0.33', 'something': '0.33', 'totally': '0.33' }, 'body.metadata': { 'coolness': '0.50', 'documentness': '0.50' }, 'importantNumber': { '200': 200 }, '_id': 'c', '!doc': { '_id': 'c', 'title': 'something different', 'body': { 'text': 'something totally different', 'metadata': 'coolness documentness' }, 'importantNumber': 200 } } },
    { 'key': '￮FIELD￮body.metadata￮', 'value': true },
    { 'key': '￮FIELD￮body.text￮', 'value': true },
    { 'key': '￮FIELD￮importantNumber￮', 'value': true },
    { 'key': '￮FIELD￮title￮', 'value': true }
  ];
  global[indexName].INDEX.STORE.createReadStream()
    .on('data', d => indexStructure.push(d))
    .on('end', d => t.looseEquals(indexStructure, expectedIndexStructure));
});
