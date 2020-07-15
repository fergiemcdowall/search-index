'use strict';

function _interopDefault (ex) { return (ex && (typeof ex === 'object') && 'default' in ex) ? ex['default'] : ex; }

var fii = _interopDefault(require('fergies-inverted-index'));
var tv = _interopDefault(require('term-vector'));

const scoreArrayTFIDF = arr => {
  const v = tv(arr);
  const mostTokenOccurances = v.reduce((acc, cur) => Math.max(cur.positions.length, acc), 0);
  return v
    .map(item => item.term[0] + '#' +
             (((item.positions.length / mostTokenOccurances)).toFixed(2)))
};

// traverse object, tokenising all leaves (strings to array) and then
// scoring them
// `ops` is a collection of indexing pipeline options
const createDocumentVector = (obj, ops) => Object.entries(obj).reduce((acc, [
  fieldName, fieldValue
]) => {
  // if fieldname is undefined, ignore and procede to next
  if (fieldValue === undefined) return acc
  ops = Object.assign({
    caseSensitive: false
  }, ops || {});
  if (fieldName === '_id') {
    acc[fieldName] = fieldValue + ''; // return _id "as is" and stringify
  } else if (Array.isArray(fieldValue)) {
    // split up fieldValue into an array or strings and an array of
    // other things. Then term-vectorize strings and recursively
    // process other things.
    const strings = scoreArrayTFIDF(
      fieldValue
        .filter(item => typeof item === 'string')
        .map(str => str.toLowerCase())
    );
    const notStrings = fieldValue.filter(
      item => typeof item !== 'string'
    ).map(createDocumentVector);
    acc[fieldName] = strings.concat(notStrings).sort();
  } else if (typeof fieldValue === 'object') {
    acc[fieldName] = createDocumentVector(fieldValue);
  } else {
    let str = fieldValue.toString().replace(/[^0-9a-z ]/gi, '');
    if (!ops.caseSensitive) str = str.toLowerCase();
    acc[fieldName] = scoreArrayTFIDF(str.split(' ')).sort();
  }
  return acc
}, {});

function writer (fii) {
  const incrementDocCount = increment => fii.STORE.get(
    '￮DOCUMENT_COUNT￮'
  ).then(
    count => fii.STORE.put('￮DOCUMENT_COUNT￮', +count + increment)
  ).catch(
    // if not found assume value to be 0
    e => fii.STORE.put('￮DOCUMENT_COUNT￮', increment)
  );

  const decrementDocCount = increment => fii.STORE.get(
    '￮DOCUMENT_COUNT￮'
  ).then(
    count => fii.STORE.put('￮DOCUMENT_COUNT￮', +count - increment)
  );

  const PUT = (docs, ops) => fii.PUT(
    docs.map(doc => createDocumentVector(doc, ops))
  ).then(documentVector => Promise.all(
    docs.map(doc =>
      fii.STORE.put('￮DOC_RAW￮' + doc._id + '￮', doc)
    )).then(
    result => incrementDocCount(documentVector.length)
  )
  );

  const DELETE = _ids => fii.DELETE(_ids).then(
    result => Promise.all(
      result.map(r => fii.STORE.del('￮DOC_RAW￮' + r._id + '￮'))
    ).then(
      result => decrementDocCount(_ids.length).then(() => _ids.map(
        _id => ({
          _id: _id,
          operation: 'DELETE',
          status: 'OK'
        })
      ))
    )
  );

  const parseJsonUpdate = update => {
    if (update.DELETE) return DELETE(update.DELETE)
  };

  return {
    // TODO: DELETE should be able to handle errors (_id not found etc.)
    DELETE: DELETE,
    PUT: PUT,
    parseJsonUpdate: parseJsonUpdate
  }
}

function reader (fii) {
  const DOCUMENTS = requestedDocs => {
    // Either return document per id
    if (Array.isArray(requestedDocs)) {
      return Promise.all(
        requestedDocs.map(
          doc => fii.STORE.get('￮DOC_RAW￮' + doc._id + '￮')
            .catch(e => null)
        )
      ).then(returnedDocs => requestedDocs.map((rd, i) => {
        rd._doc = returnedDocs[i];
        return rd
      }))
    }
    // or just dump out all docs
    // TODO should share getRange in indexUtils.js?
    return new Promise((resolve, reject) => {
      var result = [];
      fii.STORE.createReadStream({
        gte: '￮DOC_RAW￮',
        lte: '￮DOC_RAW￮￮'
      }).on('data', d => result.push({
        _id: d.value._id,
        _doc: d.value
      })).on('end', () => resolve(result));
    })
  };

  const DICTIONARY = token => DISTINCT(token).then(results =>
    Array.from(results.reduce(
      (acc, cur) => acc.add(cur.VALUE), new Set())
    ).sort()
  );

  const DISTINCT = token => fii.DISTINCT(token).then(result => [
    // Stringify Set entries so that Set can determine duplicates
    ...result.reduce((acc, cur) => acc.add(JSON.stringify(
      Object.assign(cur, {
        VALUE: cur.VALUE.split('#')[0]
      })
    )), new Set())
  ].map(JSON.parse)); // un-stringify

  const PAGE = (results, options) => {
    options = Object.assign({
      NUMBER: 0,
      SIZE: 20
    }, options || {});
    const start = options.NUMBER * options.SIZE;
    // handle end index correctly when (start + size) == 0
    // (when paging from the end with a negative page number)
    const end = (start + options.SIZE) || undefined;
    return results.slice(start, end)
  };

  // score by tfidf by default
  // TODO: should also be an option to score by field
  // TODO: Total hits (length of _match)
  const SCORE = (results, type) => {
    type = type || 'TFIDF'; // default
    if (type === 'TFIDF') {
      return DOCUMENT_COUNT().then(
        docCount => results.map((x, _, resultSet) => {
          const idf = Math.log((docCount + 1) / resultSet.length);
          x._score = +x._match.reduce(
            (acc, cur) => acc + idf * +cur.split('#')[1], 0
          ).toFixed(2); // TODO: make precision an option
          return x
        })
      )
    }
    if (type === 'PRODUCT') {
      return new Promise(resolve => resolve(
        results.map(r => {
          r._score = +r._match.reduce(
            (acc, cur) => acc * +cur.split('#')[1], 1
          ).toFixed(2); // TODO: make precision an option
          return r
        })
      ))
    }
    if (type === 'CONCAT') {
      return new Promise(resolve => resolve(
        results.map(r => {
          r._score = r._match.reduce(
            (acc, cur) => acc + cur.split('#')[1], ''
          );
          return r
        })
      ))
    }
    if (type === 'SUM') {
      return new Promise(resolve => resolve(
        results.map(r => {
          r._score = +r._match.reduce(
            (acc, cur) => acc + +cur.split('#')[1], 0
          ).toFixed(2); // TODO: make precision an option
          return r
        })
      ))
    }
  };

  const SEARCH = (...q) => fii
    .AND(...q)
    .then(SCORE)
    .then(SORT);

  const SORT = (results, options) => {
    options = Object.assign({
      DIRECTION: 'DESCENDING',
      FIELD: '_score',
      TYPE: 'NUMERIC'
    }, options || {});
    const deepRef = obj => {
      const path = options.FIELD.split('.');
      // TODO: dont like doing it this way- there should probably be a
      // way to dump the literal field value into _score, and always
      // sort on _score
      //
      // That said, it should be possible to score without fetching
      // the whole document for each _id
      //
      // special case: sorting on _match so that you dont have to
      // fetch all the documents before doing a sort
      if (path[0] === '_match') {
        return obj._match.find(
          _match => (path.slice(1).join('.') === _match.split(':')[0])
        ).split(':')[1].split('#')[0]
      }
      return path.reduce((o, i) => o[i], obj)
    };
    const sortFunction = {
      'NUMERIC': {
        'DESCENDING': (a, b) => +deepRef(b) - +deepRef(a),
        'ASCENDING': (a, b) => +deepRef(a) - +deepRef(b)
      },
      'ALPHABETIC': {
        'DESCENDING': (a, b) => {
          if (deepRef(a) < deepRef(b)) return 1
          if (deepRef(a) > deepRef(b)) return -1
          return 0
        },
        'ASCENDING': (a, b) => {
          if (deepRef(a) < deepRef(b)) return -1
          if (deepRef(a) > deepRef(b)) return 1
          return 0
        }
      }
    };
    return results.sort(sortFunction[options.TYPE][options.DIRECTION])
  };

  const DOCUMENT_COUNT = () => fii.STORE.get('￮DOCUMENT_COUNT￮');

  // This function reads queries in a JSON format and then translates them to
  // Promises
  const parseJsonQuery = (...q) => {
    // needs to be called with "command" and result from previous "thenable"
    var promisifyQuery = (command, resultFromPreceding) => {
      if (typeof command === 'string') return fii.GET(command)
      if (command.AND) return fii.AND(...command.AND.map(promisifyQuery))
      if (command.BUCKETFILTER) {
        if (command.BUCKETFILTER.BUCKETS.DISTINCT) {
          return fii.BUCKETFILTER(
            DISTINCT(command.BUCKETFILTER.BUCKETS.DISTINCT)
              .then(bkts => bkts.map(fii.BUCKET)),
            promisifyQuery(command.BUCKETFILTER.FILTER)
          )
        } else {
          return fii.BUCKETFILTER(
            command.BUCKETFILTER.BUCKETS.map(fii.BUCKET),
            promisifyQuery(command.BUCKETFILTER.FILTER)
          )
        }
      }
      // feed in preceding results if present (ie if not first promise)
      if (command.BUCKET) return fii.BUCKET(resultFromPreceding || command.BUCKET)
      if (command.DICTIONARY) return DICTIONARY(command.DICTIONARY)
      if (command.DISTINCT) return DISTINCT(command.DISTINCT)
      // feed in preceding results if present (ie if not first promise)
      if (command.DOCUMENTS) return DOCUMENTS(resultFromPreceding || command.DOCUMENTS)
      if (command.GET) return fii.GET(command.GET)
      if (command.NOT) {
        return fii.SET_SUBTRACTION(
          promisifyQuery(command.NOT.INCLUDE),
          promisifyQuery(command.NOT.EXCLUDE)
        )
      }
      if (command.OR) return fii.OR(...command.OR.map(promisifyQuery))
      if (command.PAGE) return PAGE(resultFromPreceding, command.PAGE)
      if (command.SCORE) return SCORE(resultFromPreceding, command.SCORE)
      if (command.SEARCH) return SEARCH(...command.SEARCH.map(promisifyQuery))
      if (command.SORT) return SORT(resultFromPreceding, command.SORT)
    };
    // Turn the array of commands into a chain of promises
    return q.reduce((acc, cur) => acc.then(
      result => promisifyQuery(cur, result)
    ), promisifyQuery(q.shift())) // <- Separate the first promise in the chain
    //                                  to be used as the start point in .reduce
  };

  return {
    AND: fii.AND,
    BUCKET: fii.BUCKET,
    BUCKETFILTER: fii.BUCKETFILTER,
    DICTIONARY: DICTIONARY,
    DISTINCT: DISTINCT,
    DOCUMENTS: DOCUMENTS,
    DOCUMENT_COUNT: DOCUMENT_COUNT,
    GET: fii.GET,
    OR: fii.OR,
    PAGE: PAGE,
    SCORE: SCORE,
    SEARCH: SEARCH,
    SET_SUBTRACTION: fii.SET_SUBTRACTION,
    SORT: SORT,
    parseJsonQuery: parseJsonQuery
  }
}

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
    DOCUMENT_COUNT: r.DOCUMENT_COUNT,
    GET: r.GET,
    INDEX: idx,
    NOT: r.SET_SUBTRACTION,
    OR: r.OR,
    PAGE: r.PAGE,
    PUT: w.PUT,
    SCORE: r.SCORE,
    SEARCH: r.SEARCH,
    SORT: r.SORT,
    QUERY: r.parseJsonQuery,
    UPDATE: w.parseJsonUpdate
  }
};

function main (ops) {
  return new Promise((resolve, reject) => {
    ops = Object.assign(ops || {}, {
      tokenAppend: '#'
    });
    // if a fergies-inverted-index is passed as an option
    if (ops.fii) return resolve(makeASearchIndex(ops.fii))
    // else make a new fergies-inverted-index
    fii(ops, (err, idx) => {
      if (err) return reject(err)
      resolve(makeASearchIndex(idx));
    });
  })
}

module.exports = main;
