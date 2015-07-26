<a name="module_search-index"></a>
## search-index
search-index module.


* [search-index](#module_search-index)
  * [module.exports([options])](#exp_module_search-index--module.exports) ⏏
    * [~getStopwords([lang])](#module_search-index--module.exports..getStopwords) ⇒ <code>Array.&lt;string&gt;</code>
    * [~add(batch, [batchOptions], callback)](#module_search-index--module.exports..add)
    * [~del(docID, callback)](#module_search-index--module.exports..del)
    * [~deleteBatch(docID, callback)](#module_search-index--module.exports..deleteBatch)
    * [~empty(callback)](#module_search-index--module.exports..empty)
    * [~get(docID, callback)](#module_search-index--module.exports..get)
    * [~match(beginsWith, callback)](#module_search-index--module.exports..match)
    * [~replicate(readStream, callback)](#module_search-index--module.exports..replicate)
    * [~replicateBatch(serializedDB, callback)](#module_search-index--module.exports..replicateBatch)
    * [~search(q, callback)](#module_search-index--module.exports..search)
    * [~snapShotBatch(callback)](#module_search-index--module.exports..snapShotBatch)
    * [~snapShot(callback)](#module_search-index--module.exports..snapShot)
    * [~tellMeAboutMySearchIndex(callback)](#module_search-index--module.exports..tellMeAboutMySearchIndex)
    * [~close(callback)](#module_search-index--module.exports..close)

<a name="exp_module_search-index--module.exports"></a>
### module.exports([options]) ⏏
get an instance of search-index

**Kind**: Exported function  

| Param | Type | Default | Description |
| --- | --- | --- | --- |
| [options] | <code>Object</code> |  |  |
| [options.deletable] | <code>boolean</code> | <code>true</code> | Can documents be deleted, or re-added? |
| [options.fieldedSearch] | <code>boolean</code> | <code>true</code> | Can documents fields be searched individually? |
| [options.indexPath] | <code>string</code> | <code>&quot;\&quot;si\&quot;&quot;</code> | Name of (path to) levelDB store |
| [options.logLevel] | <code>string</code> | <code>&quot;\&quot;error\&quot;&quot;</code> | Bunyan.js log level |
| [options.nGramLength] | <code>number</code> &#124; <code>Array.&lt;number&gt;</code> &#124; <code>Object</code> | <code>1</code> | xooooooooooooooooooooooooooooooox The length of ngrams to return, can be either a number (single length), and array of numbers (mulitple lengths), or an object containing a number value for 'gte' and 'lte' (a range of lengths) |
| [options.stopwords] | <code>Array.&lt;string&gt;</code> | <code>require(&#x27;stopword&#x27;).getStopwords()</code> | An array of words that will be ignored |
| [options.fieldsToStore] | <code>string</code> | <code>&quot;\&quot;all\&quot;&quot;</code> | Which fields to store (you may wish to only store links/references/IDs instead of whole documents) |

<a name="module_search-index--module.exports..getStopwords"></a>
#### module.exports~getStopwords([lang]) ⇒ <code>Array.&lt;string&gt;</code>
Returns an array of stopwords

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  
**Returns**: <code>Array.&lt;string&gt;</code> - The stopword list  

| Param | Type | Default | Description |
| --- | --- | --- | --- |
| [lang] | <code>string</code> | <code>&quot;en&quot;</code> | a language code |

<a name="module_search-index--module.exports..add"></a>
#### module.exports~add(batch, [batchOptions], callback)
Adds a document to the index

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  

| Param | Type | Default | Description |
| --- | --- | --- | --- |
| batch | <code>Array.&lt;Object&gt;</code> |  | a batch of documents to be indexed |
| [batchOptions] | <code>Object</code> |  |  |
| [batchOptions.batchName] | <code>string</code> | <code>&quot;\&quot;my batch\&quot;&quot;</code> | Name of batch |
| [batchOptions.fieldOptions] | <code>Array.&lt;Object&gt;</code> | <code>[]</code> | each object can be as defaultFieldOptions with an extra field "fieldName" to identify fields |
| [batchOptions.fieldsToStore] | <code>Array.&lt;string&gt;</code> | <code>&quot;all&quot;</code> | array of fieldnames to be stored, defaults to all fields |
| [batchOptions.defaultFieldOptions] | <code>Object</code> |  |  |
| [batchOptions.defaultFieldOptions.filter] |  | <code>true</code> | this field can be used for facets and filters |
| [batchOptions.defaultFieldOptions.nGramLength] |  | <code>set at startup</code> | phrase search |
| [batchOptions.defaultFieldOptions.searchable] |  | <code>true</code> | is this field searchable? |
| [batchOptions.defaultFieldOptions.weight] |  | <code>1</code> | weight that this field has for relevancy |
| [batchOptions.defaultFieldOptions.fieldedSearch] |  | <code>set at startup</code> | if this field can be searched on individually |
| callback | <code>callback(err)</code> |  | A callback to run. |

<a name="module_search-index--module.exports..del"></a>
#### module.exports~del(docID, callback)
Delete a document with the given ID

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  

| Param | Type | Description |
| --- | --- | --- |
| docID | <code>string</code> | A document ID. |
| callback | <code>callback(err)</code> | A callback to run. |

<a name="module_search-index--module.exports..deleteBatch"></a>
#### module.exports~deleteBatch(docID, callback)
Delete documents with the given IDs

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  

| Param | Type | Description |
| --- | --- | --- |
| docID | <code>Array.&lt;string&gt;</code> | An array of document IDs. |
| callback | <code>callback(err)</code> | A callback to run. |

<a name="module_search-index--module.exports..empty"></a>
#### module.exports~empty(callback)
Empty the index

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  

| Param | Type | Description |
| --- | --- | --- |
| callback | <code>callback(err)</code> | A callback to run. |

<a name="module_search-index--module.exports..get"></a>
#### module.exports~get(docID, callback)
Get document with the given ID

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  

| Param | Type | Description |
| --- | --- | --- |
| docID | <code>string</code> | An document ID. |
| callback | <code>callback(err, result)</code> | A callback to run. |

<a name="module_search-index--module.exports..match"></a>
#### module.exports~match(beginsWith, callback)
Get words from the index that match the input string. Return them weighted by frequency

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  

| Param | Type | Description |
| --- | --- | --- |
| beginsWith | <code>string</code> | The string to match. |
| callback | <code>callback(err, result)</code> | A callback to run. |

<a name="module_search-index--module.exports..replicate"></a>
#### module.exports~replicate(readStream, callback)
Feed another index into this (empty) index

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  

| Param | Type | Description |
| --- | --- | --- |
| readStream | <code>readStream</code> | A readStream from a saved search-index. |
| callback | <code>callback(err)</code> | A callback to run. |

<a name="module_search-index--module.exports..replicateBatch"></a>
#### module.exports~replicateBatch(serializedDB, callback)
Feed another index into this (empty) index

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  

| Param | Type | Description |
| --- | --- | --- |
| serializedDB | <code>Array.&lt;Object&gt;</code> | A serialized levelDB. |
| callback | <code>callback(err)</code> | A callback to run. |

<a name="module_search-index--module.exports..search"></a>
#### module.exports~search(q, callback)
Search the index

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  

| Param | Type | Description |
| --- | --- | --- |
| q | <code>Array.&lt;Object&gt;</code> | a query object TODO: add a good explanation here. |
| callback | <code>callback(err, result)</code> | A callback to run that returns the results or an error. |

<a name="module_search-index--module.exports..snapShotBatch"></a>
#### module.exports~snapShotBatch(callback)
Create a snapshot of this index

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  

| Param | Type | Description |
| --- | --- | --- |
| callback | <code>callback(snapshot)</code> | returns snapshot as an array. |

<a name="module_search-index--module.exports..snapShot"></a>
#### module.exports~snapShot(callback)
Create a snapshot of this index

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  

| Param | Type | Description |
| --- | --- | --- |
| callback | <code>callback(snapshot)</code> | returns snapshot as a stream. |

<a name="module_search-index--module.exports..tellMeAboutMySearchIndex"></a>
#### module.exports~tellMeAboutMySearchIndex(callback)
Create a snapshot of this index

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  

| Param | Type | Description |
| --- | --- | --- |
| callback | <code>callback(info)</code> | returns information about the search-index |

<a name="module_search-index--module.exports..close"></a>
#### module.exports~close(callback)
Close this index

**Kind**: inner method of <code>[module.exports](#exp_module_search-index--module.exports)</code>  

| Param | Type | Description |
| --- | --- | --- |
| callback | <code>callback(err)</code> | Did it error? |

