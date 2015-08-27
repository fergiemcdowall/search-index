var si = require('../../../')({indexPath: 'data'});
var data = require('../../../node_modules/reuters-21578-json/data/justTen/justTen.json');

module.exports = function (self) {
    si.add({batchName: 'firstBatch'}, data, function (err) { 
        self.postMessage(err || 'indexed batch');
    });
    self.addEventListener('message', function (ev) {
        si.search(ev.data, function (err, data) {
                self.postMessage(data);
        });
    });
};
