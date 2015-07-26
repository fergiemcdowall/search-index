# Replication

You can easily create a snapshot of an index or read in a snapshot
from another index.

For replication from a server to a browser [see this
example](examples/browserify-replicate-full-index-to-browser).


## Create a Snapshot

```javascript
//assumes that: var fs = require('fs')
si.snapShot(function(readStream) {
  readStream.pipe(fs.createWriteStream('backup.gz'))
    .on('close', function() {
    //a snapshot of the search-index now exists in the file 'backup.gz'
  });
});
```

## Replicate a snapshot file into another index

Note: the new index must be empty

```javascript
//assumes that backup is in a file called 'backup.gz'
si.replicate(fs.createReadStream('backup.gz'), function(msg){
  that.completed = true;
});
```


