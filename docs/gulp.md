# Using Gulp.js

The stream-based nature of _search-index_ makes working with [Gulp.js](http://gulpjs.com/) is easy. The following code snippet adapts the [quick start](quickstart.html) and demonstrates how to add documents via a Gulp stream.

## tl;dr

The following, along with the implementation for [`addTopicToStream()`](#adding-content-to-the-gulp-stream), allows you to pipe an instance of a _search-index_ index into a Gulp stream. [The full code listing is available here](https://gist.github.com/craigshoemaker/2d67998a0d74fa2f5e8f47789fbb9d94).

````
gulp.src(args.src)
	.on('error', (e) => console.log(e))
	.pipe(addTopicToStream())
	.pipe(index.defaultPipeline())
	.pipe(index.add())
	.on('data', () => {})
	.on('end', done);
````

## The Gulp Task
The Gulp task runs once the _search-index_ database is open and the index is ready for use.

````
const searchIndex = require('search-index');
const args = require('yargs')
				.usage('gulp db --dbpath PATH_TO_DATABASE --src PATH_TO_FILES_TO_ADD_TO_DATABASE')
				.demand(['dbpath', 'src'])
				.argv;

module.exports.registerTask = (gulp) => {
	
	gulp.task('db', (done) => {

		const options = { indexPath:  args.dbpath };

        searchIndex(options, (openDatabaseError, index) => {

			if(openDatabaseError) {
				console.log(`Error: ${openDatabaseError}`);
				return;
			}

			const addTopicToStream = require('./addTopicToStream.js');
			
			gulp.src(args.src)
				.on('error', (e) => console.log(e))
				.pipe(addTopicToStream())
				.pipe(index.defaultPipeline())
				.pipe(index.add())
				.on('data', () => {})
				.on('end', done);
		});
	});
};
````
Starting from the top, this module imports [`search-index`](https://www.npmjs.com/package/search-index) and [`yargs`](https://www.npmjs.com/package/yargsv). The `yargs` module makes it easy to read command line parameters and enforce required values.

As `registerTask` is exported from this module, the function takes a `gulp` instance as a parameter. Since this is an async operation, the `done` parameter is defined on the Gulp task and is not run until the `end` event is handled on the stream.

Next, `searchIndex` is initialized by passing in options that include the path to the database. The associated callback includes a possible error object and the instance of the index. Once the index is available, the Gulp stream works with _search-index_ in order to add data to the database and index the contents.

After the initial ceremony of adding a source for the Gulp stream and handling the error event, the first pipe is to the `addTopicToStream()` function (see below for details). Then the stream is piped through `index.defaultPipeline()` and `index.add()`.

> **Note**: The stream's `data` event must be handled (even if you don't do anything with it) for the `end` event to fire.

Once the stream is done processing the source files, the `end` event fires which in turn calls the `done` function passed into the Gulp task. Executing the `done` function signals to the running Gulp process that the async operation is complete.

## Adding Content to the Gulp Stream
The key to adding content to the database via Gulp is to implement a function that is piped into the stream that adds content in the appropriate form. In this case each new document includes a unique `id` value and the contents of the file injected into the `body` of the document before being added to the stream.

````
const through = require('through2');
const uuidV4 = require('uuid/v4');

module.exports = () => {
	return through.obj(function(file, encoding, next){

		this.push({
            id: uuidV4(),
            body: file.contents.toString(encoding)
        });
        
		next();
	});
};
````

> **Note**: This example uses [uuid](https://www.npmjs.com/package/uuid) as a means for generating a unique identifier. You may choose to identify your documents in some other way.

After importing [through2](https://github.com/rvagg/through2) and [uuid](https://www.npmjs.com/package/uuid), this module exports a single function that in turn returns an instance of and object stream via `through.obj`. 

The file is added to the index by creating a object containing a value for `id` and `body`. The `id` property is set to a [uuid4()](http://stackoverflow.com/questions/20342058/which-uuid-version-to-use) value to ensure uniqueness. The `body` property is filled by calling `toString` on `file.contents`. Notice that the given `encoding` is passed to `toString` to ensure the original encoding is preserved.

> **Note:** You may be tempted to converted the function passed into `through.obj` to use JavaScript's [arrow notation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions#Arrow_functions). Resist this urge as using arrow notation binds the context of `this` to the function itself, but here you want `this` to reference the stream.

## Resources
To see the entire code sample in context, make sure to check out the [full code listing](https://gist.github.com/craigshoemaker/2d67998a0d74fa2f5e8f47789fbb9d94).