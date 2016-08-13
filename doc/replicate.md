# Replication

You can easily create a snapshot of an index or read in a snapshot
from another index.

For replication from a server to a browser [see this
example](examples/browserify-replicate-full-index-to-browser).


## Create a Snapshot

```javascript
// assumes that: var fs = require('fs')
si.DBReadStream({ gzip: true })
  .pipe(fs.createWriteStream(sandboxPath + '/backup.gz'))
  .on('close', function() {
    // done
  })
```

## Replicate a snapshot file into another index

Note: the new index must be empty

```javascript
// assumes that backup is in a file called 'backup.gz'
fs.createReadStream(sandboxPath + '/backup.gz')
  .pipe(zlib.createGunzip())
  .pipe(JSONStream.parse())
  .pipe(si.DBWriteStream())
  .on('close', function() {
    // done
  })
```


