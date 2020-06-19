import fii from 'fergies-inverted-index';
import tv from 'term-vector';

function _typeof(obj) {
  if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
    _typeof = function (obj) {
      return typeof obj;
    };
  } else {
    _typeof = function (obj) {
      return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj;
    };
  }

  return _typeof(obj);
}

function _slicedToArray(arr, i) {
  return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _nonIterableRest();
}

function _toConsumableArray(arr) {
  return _arrayWithoutHoles(arr) || _iterableToArray(arr) || _nonIterableSpread();
}

function _arrayWithoutHoles(arr) {
  if (Array.isArray(arr)) {
    for (var i = 0, arr2 = new Array(arr.length); i < arr.length; i++) arr2[i] = arr[i];

    return arr2;
  }
}

function _arrayWithHoles(arr) {
  if (Array.isArray(arr)) return arr;
}

function _iterableToArray(iter) {
  if (Symbol.iterator in Object(iter) || Object.prototype.toString.call(iter) === "[object Arguments]") return Array.from(iter);
}

function _iterableToArrayLimit(arr, i) {
  if (!(Symbol.iterator in Object(arr) || Object.prototype.toString.call(arr) === "[object Arguments]")) {
    return;
  }

  var _arr = [];
  var _n = true;
  var _d = false;
  var _e = undefined;

  try {
    for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) {
      _arr.push(_s.value);

      if (i && _arr.length === i) break;
    }
  } catch (err) {
    _d = true;
    _e = err;
  } finally {
    try {
      if (!_n && _i["return"] != null) _i["return"]();
    } finally {
      if (_d) throw _e;
    }
  }

  return _arr;
}

function _nonIterableSpread() {
  throw new TypeError("Invalid attempt to spread non-iterable instance");
}

function _nonIterableRest() {
  throw new TypeError("Invalid attempt to destructure non-iterable instance");
}

var scoreArrayTFIDF = function scoreArrayTFIDF(arr) {
  var v = tv(arr);
  var mostTokenOccurances = v.reduce(function (acc, cur) {
    return Math.max(cur.positions.length, acc);
  }, 0);
  return v.map(function (item) {
    return item.term[0] + '#' + (item.positions.length / mostTokenOccurances).toFixed(2);
  });
}; // traverse object, tokenising all leaves (strings to array) and then
// scoring them
// `ops` is a collection of indexing pipeline options


var createDocumentVector = function createDocumentVector(obj, ops) {
  return Object.entries(obj).reduce(function (acc, _ref) {
    var _ref2 = _slicedToArray(_ref, 2),
        fieldName = _ref2[0],
        fieldValue = _ref2[1];

    // if fieldname is undefined, ignore and procede to next
    if (fieldValue === undefined) return acc;
    ops = Object.assign({
      caseSensitive: false
    }, ops || {});

    if (fieldName === '_id') {
      acc[fieldName] = fieldValue; // return _id "as is"
    } else if (Array.isArray(fieldValue)) {
      // split up fieldValue into an array or strings and an array of
      // other things. Then term-vectorize strings and recursively
      // process other things.
      var strings = scoreArrayTFIDF(fieldValue.filter(function (item) {
        return typeof item === 'string';
      }).map(function (str) {
        return str.toLowerCase();
      }));
      var notStrings = fieldValue.filter(function (item) {
        return typeof item !== 'string';
      }).map(createDocumentVector);
      acc[fieldName] = strings.concat(notStrings).sort();
    } else if (_typeof(fieldValue) === 'object') {
      acc[fieldName] = createDocumentVector(fieldValue);
    } else {
      var str = fieldValue.toString().replace(/[^0-9a-z ]/gi, '');
      if (!ops.caseSensitive) str = str.toLowerCase();
      acc[fieldName] = scoreArrayTFIDF(str.split(' ')).sort();
    }

    return acc;
  }, {});
};

function writer (fii) {
  var incrementDocCount = function incrementDocCount(increment) {
    return fii.STORE.get('￮DOCUMENT_COUNT￮').then(function (count) {
      return fii.STORE.put('￮DOCUMENT_COUNT￮', +count + increment);
    })["catch"]( // if not found assume value to be 0
    function (e) {
      return fii.STORE.put('￮DOCUMENT_COUNT￮', increment);
    });
  };

  var PUT = function PUT(docs, ops) {
    return fii.PUT(docs.map(function (doc) {
      return createDocumentVector(doc, ops);
    })).then(function (documentVector) {
      return Promise.all(docs.map(function (doc) {
        return fii.STORE.put('￮DOC_RAW￮' + doc._id + '￮', doc);
      })).then(function (result) {
        return incrementDocCount(documentVector.length);
      });
    });
  };

  var DELETE = function DELETE(_ids) {
    return fii.DELETE(_ids).then(function (result) {
      return Promise.all(result.map(function (r) {
        return fii.STORE.del('￮DOC_RAW￮' + r._id + '￮');
      })).then(function (result) {
        return _ids.map(function (_id) {
          return {
            _id: _id,
            operation: 'DELETE',
            status: 'OK'
          };
        });
      });
    });
  };

  var parseJsonUpdate = function parseJsonUpdate(update) {
    if (update.DELETE) return DELETE(update.DELETE);
  };

  return {
    // TODO: DELETE should be able to handle errors (_id not found etc.)
    DELETE: DELETE,
    PUT: PUT,
    parseJsonUpdate: parseJsonUpdate
  };
}

function getAvailableFields(fii) {
  return getRange(fii, {
    gte: '￮FIELD￮',
    lte: '￮FIELD￮￮'
  }).then(function (fields) {
    return fields.map(function (field) {
      return field.split('￮')[2];
    });
  });
}
function getRange(fii, q) {
  return new Promise(function (resolve, reject) {
    var data = [];
    fii.STORE.createKeyStream(q).on('data', function (d) {
      return data.push(d);
    }).on('end', function () {
      return resolve(data);
    });
  });
}
function getDocCount(fii) {
  return fii.STORE.get('￮DOCUMENT_COUNT￮')["catch"]( // if not found assume value to be 0
  function (e) {
    return 0;
  });
}

function reader (fii) {
  var DICTIONARY = function DICTIONARY(q) {
    return new Promise(function (resolve) {
      var flatten = function flatten(arr) {
        return [].concat.apply([], arr);
      }; // if query is string convert to object
      // if no query, make empty query


      q = Object.assign({
        gte: '',
        lte: '￮'
      }, typeof q === 'string' ? {
        gte: q,
        lte: q + '￮'
      } : q); // options, defaults

      q.options = Object.assign({
        withFieldName: false
      }, q.options || {});
      return resolve(new Promise(function (resolve) {
        return resolve(q.fields || getAvailableFields(fii));
      }).then(function (fields) {
        return Promise.all(fields.map(function (field) {
          return getRange(fii, {
            gte: field + ':' + q.gte,
            lte: field + ':' + q.lte + '￮'
          });
        }));
      }).then(flatten) //        .then(res => {console.log(res); return res})
      .then(function (tokens) {
        return tokens.map(function (t) {
          return q.options.withFieldName ? t.split('#').shift() : t.split(':').pop().split('#').shift();
        });
      }).then(function (tokens) {
        return tokens.sort();
      }).then(function (tokens) {
        return _toConsumableArray(new Set(tokens));
      }));
    });
  };

  var DOCUMENTS = function DOCUMENTS(requestedDocs) {
    // Either return document per id
    if (Array.isArray(requestedDocs)) {
      return Promise.all(requestedDocs.map(function (doc) {
        return fii.STORE.get('￮DOC_RAW￮' + doc._id + '￮')["catch"](function (e) {
          return null;
        });
      })).then(function (returnedDocs) {
        return requestedDocs.map(function (rd, i) {
          rd._doc = returnedDocs[i];
          return rd;
        });
      });
    } // or just dump out all docs
    // TODO should share getRange in indexUtils.js?


    return new Promise(function (resolve, reject) {
      var result = [];
      fii.STORE.createReadStream({
        gte: '￮DOC_RAW￮',
        lte: '￮DOC_RAW￮￮'
      }).on('data', function (d) {
        return result.push({
          _id: d.value._id,
          _doc: d.value
        });
      }).on('end', function () {
        return resolve(result);
      });
    });
  };

  var SEARCH = function SEARCH() {
    return fii.AND.apply(fii, arguments).then(SCORE).then(SORT);
  };

  var PAGE = function PAGE(results, options) {
    options = Object.assign({
      number: 0,
      size: 20
    }, options || {});
    var start = options.number * options.size; // handle end index correctly when (start + size) == 0
    // (when paging from the end with a negative page number)

    var end = start + options.size || undefined;
    return results.slice(start, end);
  }; // score by tfidf by default


  var SCORE = function SCORE(results) {
    return getDocCount(fii).then(function (docCount) {
      return results.map(function (x, _, resultSet) {
        var idf = Math.log((docCount + 1) / resultSet.length);
        x._score = +x._match.reduce(function (acc, cur) {
          return acc + idf * +cur.split('#')[1];
        }, 0).toFixed(2); // TODO: make precision an option

        return x;
      });
    });
  };

  var SORT = function SORT(results, options) {
    options = Object.assign({
      direction: 'DESCENDING',
      field: '_score',
      type: 'NUMERIC'
    }, options || {});

    var deepRef = function deepRef(obj) {
      var path = options.field.split('.'); // special case: sorting on _match so that you dont have to
      // fetch all the documents before doing a sort

      if (path[0] === '_match') {
        return obj._match.find(function (_match) {
          return path.slice(1).join('.') === _match.split(':')[0];
        }).split(':')[1].split('#')[0];
      }

      return path.reduce(function (o, i) {
        return o[i];
      }, obj);
    };

    var sortFunction = {
      'NUMERIC': {
        'DESCENDING': function DESCENDING(a, b) {
          return +deepRef(b) - +deepRef(a);
        },
        'ASCENDING': function ASCENDING(a, b) {
          return +deepRef(a) - +deepRef(b);
        }
      },
      'ALPHABETIC': {
        'DESCENDING': function DESCENDING(a, b) {
          if (deepRef(a) < deepRef(b)) return 1;
          if (deepRef(a) > deepRef(b)) return -1;
          return 0;
        },
        'ASCENDING': function ASCENDING(a, b) {
          if (deepRef(a) < deepRef(b)) return -1;
          if (deepRef(a) > deepRef(b)) return 1;
          return 0;
        }
      }
    };
    return results.sort(sortFunction[options.type][options.direction]);
  };

  var DISTINCT = function DISTINCT(term) {
    return fii.DISTINCT(term).then(function (result) {
      return _toConsumableArray(result.reduce(function (acc, cur) {
        cur.value = cur.value.split('#')[0];
        acc.add(JSON.stringify(cur));
        return acc;
      }, new Set())).map(JSON.parse);
    });
  }; // This function reads queries in a JSON format and then translates them to
  // Promises


  var parseJsonQuery = function parseJsonQuery() {
    // needs to be called with "command" and result from previous "thenable"
    var promisifyQuery = function promisifyQuery(command, resultFromPreceding) {
      if (typeof command === 'string') return fii.GET(command);
      if (command.AND) return fii.AND.apply(fii, _toConsumableArray(command.AND.map(promisifyQuery)));

      if (command.BUCKETFILTER) {
        if (command.BUCKETFILTER.BUCKETS.DISTINCT) {
          return fii.BUCKETFILTER(DISTINCT(command.BUCKETFILTER.BUCKETS.DISTINCT).then(function (bkts) {
            return bkts.map(fii.BUCKET);
          }), promisifyQuery(command.BUCKETFILTER.FILTER));
        } else {
          return fii.BUCKETFILTER(command.BUCKETFILTER.BUCKETS.map(fii.BUCKET), promisifyQuery(command.BUCKETFILTER.FILTER));
        }
      } // feed in preceding results if present (ie if not first promise)


      if (command.BUCKET) return fii.BUCKET(resultFromPreceding || command.BUCKET);
      if (command.DICTIONARY) return DICTIONARY(command.DICTIONARY);
      if (command.DISTINCT) return DISTINCT(command.DISTINCT); // feed in preceding results if present (ie if not first promise)

      if (command.DOCUMENTS) return DOCUMENTS(resultFromPreceding || command.DOCUMENTS);
      if (command.GET) return fii.GET(command.GET);

      if (command.NOT) {
        return fii.SET_SUBTRACTION(promisifyQuery(command.NOT.INCLUDE), promisifyQuery(command.NOT.EXCLUDE));
      }

      if (command.OR) return fii.OR.apply(fii, _toConsumableArray(command.OR.map(promisifyQuery)));
      if (command.PAGE) return PAGE(resultFromPreceding, command.PAGE);
      if (command.SEARCH) return SEARCH.apply(void 0, _toConsumableArray(command.SEARCH.map(promisifyQuery)));
      if (command.SORT) return SORT(resultFromPreceding, command.SORT);
    }; // Turn the array of commands into a chain of promises


    for (var _len = arguments.length, q = new Array(_len), _key = 0; _key < _len; _key++) {
      q[_key] = arguments[_key];
    }

    return q.reduce(function (acc, cur) {
      return acc.then(function (result) {
        return promisifyQuery(cur, result);
      });
    }, promisifyQuery(q.shift())); // <- Separate the first promise in the chain
    //                                  to be used as the start point in .reduce
  };

  return {
    AND: fii.AND,
    BUCKET: fii.BUCKET,
    BUCKETFILTER: fii.BUCKETFILTER,
    DICTIONARY: DICTIONARY,
    DISTINCT: DISTINCT,
    DOCUMENTS: DOCUMENTS,
    GET: fii.GET,
    OR: fii.OR,
    PAGE: PAGE,
    SEARCH: SEARCH,
    SET_SUBTRACTION: fii.SET_SUBTRACTION,
    SORT: SORT,
    parseJsonQuery: parseJsonQuery
  };
}

function util (fii) {
  var prefetchSearchableFields = function prefetchSearchableFields() {
    var tmp = [];
    return new Promise(function (resolve) {
      fii.STORE.createKeyStream({
        gte: '￮FIELD!',
        lte: '￮FIELD￮￮'
      }).on('data', function (d) {
        return tmp.push(d.split('￮')[2]);
      }).on('end', function () {
        return resolve(global.searchableFields = tmp);
      });
    });
  };

  var countDocs = function countDocs() {
    var i = 0;
    return new Promise(function (resolve) {
      fii.STORE.createKeyStream({
        gte: '￮DOC￮!',
        lte: '￮DOC￮￮'
      }).on('data', function () {
        return i++;
      }).on('end', function () {
        return resolve(global.D = i);
      });
    });
  };

  var calibrate = function calibrate() {
    // can handle lazy opening
    if (fii.STORE.isOpen()) {
      return prefetchSearchableFields().then(countDocs);
    } else setTimeout(calibrate, 1000); // will rerun function every 1000ms until fii.STORE.isOpen()

  };

  return {
    calibrate: calibrate
  };
}

global.D = 0; // total docs in index

global.searchableFields = []; // fields that are available for searching

var makeASearchIndex = function makeASearchIndex(idx) {
  var w = writer(idx);
  var r = reader(idx);
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
    NOT: r.SET_SUBTRACTION,
    OR: r.OR,
    PAGE: r.PAGE,
    PUT: w.PUT,
    SCORENUMERIC: r.SCORENUMERIC,
    SCORETFIDF: r.SCORETFIDF,
    SEARCH: r.SEARCH,
    SORT: r.SORT,
    QUERY: r.parseJsonQuery,
    UPDATE: w.parseJsonUpdate
  };
};

function main (ops) {
  return new Promise(function (resolve, reject) {
    ops = Object.assign(ops || {}, {
      tokenAppend: '#'
    }); // if a fergies-inverted-index is passed as an option

    if (ops.fii) return resolve(makeASearchIndex(ops.fii)); // else make a new fergies-inverted-index

    fii(ops, function (err, idx) {
      if (err) return reject(err);
      resolve(util(idx).calibrate().then(function () {
        return makeASearchIndex(idx);
      }));
    });
  });
}

export default main;
