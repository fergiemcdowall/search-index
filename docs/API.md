# API


## Initialisation
* <a href="#si"><code><b>si()</b></code></a>

## Altering the index
* <a href="#DELETE"><code>db.<b>DELETE()</b></code></a>
* <a href="#PUT"><code>db.<b>PUT()</b></code></a>

## Composable querying
* <a href="#AND"><code>db.<b>AND()</b></code></a>
* <a href="#DOCUMENTS"><code>db.<b>DOCUMENTS()</b></code></a>
* <a href="#NOT"><code>db.<b>NOT()</b></code></a>
* <a href="#OR"><code>db.<b>OR()</b></code></a>

## Searching
* <a href="#GET"><code>db.<b>GET()</b></code></a>
* <a href="#SEARCH"><code>db.<b>SEARCH()</b></code></a>

## Tokenisation
* <a href="#DICTIONARY"><code>db.<b>DICTIONARY()</b></code></a>

## Aggregation
* <a href="#BUCKET"><code>db.<b>BUCKET()</b></code></a>
* <a href="#BUCKETFILTER"><code>db.<b>BUCKETFILTER()</b></code></a>
* <a href="#DISTINCT"><code>db.<b>DISTINCT()</b></code></a>

## Accessing the underlying index
* <a href="#INDEX"><code>db.<b>INDEX</b></code></a>


## FAQ

### How do I get my data into search-index?

### What is the difference between AND, GET and SEARCH?

* **AND** `AND` returns a set of documents that contain *all* of the
terms contained in the query

* **GET** `GET` returns a set of documents that contain the terms
contained in the query. `GET` is the same as `AND` or `OR` with only
one term specified.

* **SEARCH** `SEARCH` performs an `AND` query and returns a set of
documents which is ordered in terms of relevance. Search-index uses a
TF-IDF algorithm to determine relevance.


### What is the difference between documents and objects?

### How do I get out entire documents and not just document IDs?

### How do I compose queries?

### How do I perform a simple aggregation on a field value?


<a name="si"></a>

## `si([options[, callback]])`

```javascript
import si from 'search-index'

// creates a DB called "myDB" using levelDB (node.js), or indexedDB (browser)
const db = si({ name: 'myDB' })
```

In some cases you will want to start operating on the database
instentaneously. In these cases you can wait for the callback:

```javascript
import si from 'search-index'

// creates a DB called "myDB" using levelDB (node.js), or indexedDB (browser)
si({ name: 'myDB' }, (err, db) => {
  // db is guaranteed to be open and available
})
```


<a name="AND"></a>

## `db.AND([ ...Promise ]).then(result)`

Boolean AND. Return IDs of objects that have prop.A AND prop.B


<a name="BUCKET"></a>

## `db.BUCKET([ ...Promise ]).then(result)`

Return IDs of objects that match the query


<a name="BUCKETFILTER"></a>

## `db.BUCKETFILTER([ ...bucket ], filter query).then(result)`

The first argument is an array of buckets, the second is an expression
that filters each bucket


<a name="DELETE"></a>

## `db.DELETE([ ...Promise ]).then(result)`

Deletes all objects by ID


<a name="DICTIONARY"></a>

## `db.DICTIONARY(options).then(result)`

Options:

* gte : greater than or equal to
* lte : less than or equal to

Gets an array of tokens stored in the index. Usefull for i.e. auto-complete or auto-suggest scenarios.

Examples on usage:

```javascript
// get all tokens in the index
idx.DICTIONARY().then( /* array of tokens */ )

// get all tokens in the body.text field
idx.DICTIONARY('body.text').then( /* array of tokens */ )

// get tokens in the body.text field that starts with 'cool'
idx.DICTIONARY('body.text.cool').then( /* array of tokens */ )

// you can also use gte/lte ("greater/less than or equal")
idx.DICTIONARY({
  gte: 'body.text.a',
  lte: 'body.text.g'
}).then( /* array of tokens */ )
```


<a name="DISTINCT"></a>

## `db.DISTINCT(options).then(result)`

`db.DISTINCT` returns every value in the db that is greater than equal
to `gte` and less than or equal to `lte` (sorted alphabetically)

For example- get all names between `h` and `l`:

```javascript
db.DISTINCT({ gte: 'h', lte: 'l' }).then(result)
```

<a name="DOCUMENTS"></a>

## `db.DOCUMENTS([ ...id ]).then(result)`

Get documents by ID


<a name="GET"></a>

## `db.GET(property).then(result)`

`db.GET` returns all object ids for objects that contain the given
property, aggregated by object id.

For example get all names between `h` and `l`:

```javascript
db.GET({ gte: 'h', lte: 'l' }).then(result)
```

Or to get all objects that have a `name` property that begins with 'h'

```javascript
db.GET('h').then(result)
```


<a name="INDEX"></a>

## `db.INDEX`

Points to the underlying [index](https://github.com/fergiemcdowall/fergies-inverted-index/).


<a name="NOT"></a>

## `db.NOT([ ...Promise ]).then(result)`

Where A and B are sets, `db.NOT` Returns the ids of objects that are
present in A, but not in B.


<a name="OR"></a>

## `db.OR([ ...Promise ]).then(result)`

Return ids of objects that are in one or more of the query clauses


<a name="PUT"></a>

## `db.PUT([ ...Promise ]).then(result)`

Add objects to database


<a name="SEARCH"></a>

## `db.SEARCH([ ...Promise ]).then(result)`

Search the database and get documents back. 

```javascript
  idx.SEARCH(
    idx.OR('bananas', 'different'),  // search clauses can be nested promises
    'coolness'                       // or strings (defaults to GET)
  ).then(result)
```
