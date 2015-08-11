# Search

You can search in your index by passing a query object to the search
function like so:

```javascript
  q.query = {'*': ['usa']}; //search for string 'usa' in all ('*') fields
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
  '*': ['reagan']
}
```


### Fielded Search

You can search on specified fields

```javascript
q.query = {
  'title': ['reagan'],
  'body':['usa']
}
```

### Multi-word Search

To search for more than one term, the search string must be tokenised
into an array

```javascript
q.query = {
  '*': ['usa', 'reagan']
}
```

### Phrase Search

Use one array element per phrase. You can search for combinations of
phrases if desired.

```javascript
q.query = {
  '*': ['ronald reagan']
}
```

##Faceting and filtering

You can add facets or filters onto any given search.

**Facets** display agregations on the resultset. This could typically
  be price, metadata, source, or some other form of categorisation
  that can be used as a filter

**Filters** are a way of limiting the resultset. Think of them as the
  query that must be called when you select facets.


### Simple facets

The following query will display a count for every single place in the
resultset

```javascript
q.query = {
  '*': ['reagan']
};
q.facets = {places: {}};
```

### Limiting facet length

The following query will limit facet length

```javascript
q.query = {
  '*': ['reagan']
};
q.facets = {places: {limit: 10}};
```

### Sorting facets

The following query will sort a facet by its keys in a descending
order. Available sorts are `keyAsc`,`keyDesc`, `valueAsc`, and
`valueDesc`

```javascript
q.query = {
  '*': ['reagan']
};
q.facets = {places: {sort: 'keyDesc'}};
```

### Range facets

The following query will display a count for every range of values
that is defined in the query

```javascript

q.query = {'*': ['africa', 'bank']};
q.facets = {
  totalamt: {
    ranges:[
      [
        '000000000000000',
        '000000050000000'
      ],
      [
        '000000050000001',
        '100000000000000'
      ]
    ]
  },
  mjtheme: {
    ranges: [
      [
        'A',
        'J'
      ],
      [
        'K',
        'Z'
      ]
    ]
  }
}
```

## Some other query stuff

### Paging

You can handle paging by setting an `offset` and a `pageSize`. For
example, the following would give you page 3 of a resultset, where the
page size is 50:

```javascript
q.query = {
  '*': ['usa']
};
q.offset = 100;
q.pageSize = 50;
```

### Teasers and Search Term Highlighting

Search-index can generate a simple content preview in the search
results (teasers). The teaser will contain highlighted search terms.
Simply specify the field to generate the teaser on like so:

```javascript
q.query = {
  '*': ['usa']
};
q.teaser = 'body';
```
