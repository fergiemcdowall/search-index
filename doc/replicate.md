# Syncing

You can easily merge, replicate, and move around indexes


## Save index to file

```javascript
// assumes that: var fs = require('fs')
si.dbReadStream({ gzip: true })
  .pipe(fs.createWriteStream('backup.gz'))
  .on('close', function() {
    // done
  })
```

## Merge saved file into index

```javascript
// assumes that backup is in a file called 'backup.gz'
fs.createReadStream('backup.gz')
  .pipe(zlib.createGunzip())
  .pipe(JSONStream.parse())
  .pipe(si.dbWriteStream())
  .on('close', function() {
    // done
  })
```

## Sync directly from one index to another

```javascript
// syncFrom is the index that is being read from
// syncTo is the index that is being merged into
syncFrom.dbReadStream({gzip: true})
  .pipe(zlib.createGunzip())
  .pipe(JSONStream.parse())
  .pipe(syncTo.dbWriteStream())
  .on('data', function () {})
  .on('end', function () {
    // done!
  })
```

