# Search

You can search in your index by passing a query object to the search
function like so:

```javascript
q.query = {
   AND: {'*': ['usa']}     // search for string 'usa' in all ('*') fields
}
si.search(q) // <- si is a readable stream that emits documents sorted by relevance
  .on('data', function(doc) {
    // do some stuff with doc
  })
```

## Search types
Search index supports several types of search:

### Simple Search

Use the wildcard token ('*') to do simple searches across all fields

```javascript
q.query = {
  AND: {'*': ['reagan']}
}
```

(shhhh! search index will also accept "lazy" queries):

```javascript
q = 'reagan'  // the same as the query above
```

### Fielded Search

You can search on specified fields

Every `<fieldName>: [<searchterms>]` in the query must return `true`
in order for the corresponding document to be included in the search
results. In the example below, only documents containing the word *reagan*
in the `title` field **AND** the word *usa* in the `body` field will be returned.

```javascript
q.query = 
  {
    AND: {
      'title': ['reagan'],
      'body':  ['usa']
    }
  }
}
```

### Boolean Search (AND, OR, NOT)

You can construct boolean AND, OR and NOT queries:

```javascript
q.query = [                   // Each array element is an OR condition
  {
    AND: {             
      'title': ['reagan'],    // 'reagan' AND 'ussr'   
      'body':  ['ussr']
    },
    NOT: {
      'body':  ['usa']        // but NOT 'usa' in the body field
    }
  },
  {                           // OR this condition
    AND: {                  
      'title': ['gorbachev'], // 'gorbachev' AND 'ussr'
      'body':  ['ussr']
    },
    NOT: {
      'body':  ['usa']        // NOT 'usa' in the body field
    }
  }
}
```


### Multi-word Search

To search for more than one term, the search string must be tokenised
into an array

A search on `<fieldName>` for `[<searchterms>]` returns `true` if and only if
all of `[<searchterms>]` appear in `<fieldName>`

```javascript
q.query = {
  AND: {'*': ['usa', 'reagan']}
}
```

### Phrase Search

Use one array element per phrase. You can search for combinations of
phrases if desired.

```javascript
q.query = {
  AND: {'*': ['ronald reagan']}
}
```

### Boosted Search

You can boost an `OR` condition of your search string like so. The
boost value is a positive or negative number that is summed into the
final hit score.

```javascript
query: [
  {
    AND:   {'name':        ['watch']},
    BOOST: 10
  },
  {
    AND:   {'description': ['swiss', 'watch']}
  }
]
```

## Filters

**Filters** are a way of limiting the resultset, using the
  categories/buckets that the same resultset makes available. Think of
  them as the query that must be called when you select
  buckets/categories.

```javascript
query: {
  AND: {
    'description': ['swiss', 'watch'],
    'price': [{
       gte: '1000',
       lte: '8'
    }]
  }
}
```

* **gte**: greater than or equal to
* **lte**: less than or equal to

Note that filters work by selecting ranges of strings- so the example
above would return prices of '70000' for example

## Paging

You can handle paging by setting an `offset` and a `pageSize`. For
example, the following would give you page 3 of a resultset, where the
page size is 50:

```javascript
q.query    = {AND: {'*': ['usa']}}
q.offset   = 100;
q.pageSize = 50;
```

## Gotchas

### Common error when test-querying your search solution

When you have indexed some documents, you want to check if your search
is working, and do a couple of test queries. Remember then, to not use
any of the stopwords. These are words that hold little meaning and are
therefore left out of the index. This is the [search-index stopword list for English](https://github.com/fergiemcdowall/stopword/blob/master/lib/stopwords_en.js#L25-L38), and here are all the [available stopword languages](https://github.com/fergiemcdowall/stopword/tree/master/lib).

```javascript
q.query = {
  AND: {'*': ['and']}
}
```

And since there iss an inherent logical `AND` when having multiple words in your query, it only takes one word from the stopword-list to get zero results back.

```javascript
q.query = {
  AND: {'*': ['and', 'meaningful']}
}
```
