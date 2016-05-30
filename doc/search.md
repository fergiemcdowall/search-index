# Search

You can search in your index by passing a query object to the search
function like so:

```javascript
  q.query = {
     AND: [{'*': ['usa']}] //search for string 'usa' in all ('*') fields
  }
  si.search(q, function (err, searchResults) {
    //do something with searchResults
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


### Fielded Search

You can search on specified fields

Every `<fieldName>: [<searchterms>]` in the query must return `true`
in order for the corresponding document to be included in the search
results. In the example below, only documents containing the word *reagan*
in the `title` field **AND** the word *usa* in the `body` field will be returned.

```javascript
q.query = 
  {
    AND: [
      {'title': ['reagan']},
      {'body':['usa']}
    ]
  }
}
```

### Boolean Search (AND, OR, NOT)

You can construct boolean AND, OR and NOT queries:

```javascript
q.query = [                    // Each array element is an OR condition
  {
    AND: [             
      {'title': ['reagan']},   // 'reagan' AND 'ussr'   
      {'body':['ussr']}
    ],
    NOT: [
      {'body':['usa']}         // but NOT 'usa' in the body field
    ]
  },
  {                            // OR this condition
    AND: [                  
      {'title': ['gorbachev']},// 'gorbachev' AND 'ussr'
      {'body':['ussr']}
    ],
    NOT: [
      {'body':['usa']}         // NOT 'usa' in the body field
    ]
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

### Boosting

You can boost an `OR` condition of your search string like so. The boost value is a positive or negative number that is summed into the final hit score.

```javascript
query: [
  {
    AND: [{'name': ['watch']}],
    boost: 10
  },
  {
    AND: [{'description': ['swiss', 'watch']}]
  }
]
```

##Categories, Buckets and Filters

You can add categories and buckets onto any given search. You can use
the results to then filter on either buckets or categories.

**Categories** display totals for each category in the resultset

**Buckets** display totals for ranges defined in the query

**Filters** are a way of limiting the resultset, using the categories/buckets that the same resultset makes available. Think of them as the query that must be called when you select buckets/categories.

To use categories or buckets, the docments you index must have at least one key/value pair where the value is an array of words, phrases or numbers. With categories you filter on one of the values in that array. With buckets you filter on a range values in that array .

### Simple categories

The following query will display a count for every single place in the
resultset

```javascript
q.query = {
  AND: {'*': ['reagan']}
}
q.categories = {
  field: 'places'    
}
```

### Limiting facet length

The following query will limit facet length

```javascript
q.query = {
  AND: {'*': ['reagan']}
}
q.categories = {
  field: 'places',
  limit: 10
}
```

### Sorting facets

The following query will sort a facet by its keys in a descending
order. Available sorts are `keyAsc`,`keyDesc`, `valueAsc`, and
`valueDesc`

```javascript
q.query = {
  AND: {'*': ['reagan']}
}
q.categories = {
  field: 'places',
  limit: 10,
  sort: keyDesc
}
```

### Filtering on categories
```javascript
q.filter = [
  {
    field: 'places',
    gte: 'zaire',
    lte: 'zaire'
  }
]
```

### Buckets

The following query will display a count for every range of values
within the given buckets

```javascript
q.query = {'*': ['africa', 'bank']};
q.buckets = [
  {
    field: 'totalamt',
    gte: '000000000000000',
    lte: '000000050000000'
  },
  {
    field: 'totalamt',
    gte: '000000050000001',
    lte: '100000000000000'
  },
  {
    field: 'mjtheme',
    gte: 'A',
    lte: 'J'
  },
  {
    field: 'mjtheme',
    gte: 'K',
    lte: 'Z'
  }      
]
```

### Filtering on buckets

```javascript
q.filter = [
  {
    field: 'totalamt',
    gte: '000000050000001',
    lte: '100000000000000'
  }
]
```

## Some other query stuff

### Paging

You can handle paging by setting an `offset` and a `pageSize`. For
example, the following would give you page 3 of a resultset, where the
page size is 50:

```javascript
q.query = {
  AND:{'*': ['usa']}
}
q.offset = 100;
q.pageSize = 50;
```

### Teasers and Search Term Highlighting

Search-index can generate a simple content preview in the search
results (teasers). The teaser will contain highlighted search terms.
Simply specify the field to generate the teaser on like so:

```javascript
q.query = {
  AND:{'*': ['usa']}
};
q.teaser = 'body';
```

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
};
```
