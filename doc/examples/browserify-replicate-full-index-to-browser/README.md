This is a demo of how you can replicate an entire index into a browser using browserify. Ideally the whole thing should be much streamier, but I havent managed to get `fs.createReadStream` working in browserify yet...

1. Ensure that node and browserify(npm install -g browserify)  is installed, and open a command prompt at this directory.

1. run `node indexgenerator.js`

1. run `gunzip backup.gz`

1. run `browserify -t brfs main.js > bundle.js`

1. Open `index.html` in a browser and search for [some of the articles you indexed](https://raw.githubusercontent.com/fergiemcdowall/reuters-21578-json/master/data/justTen/justTen.json)
