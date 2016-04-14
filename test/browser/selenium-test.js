const fs = require('fs')
const test = require('tape')
var server

test('check size of bundle', function (t) {
  t.plan(1)
  fs.stat('./test/sandbox/bundle.js', function(err, stats) {
    console.log(stats.size)
    t.ok((stats.size < 1100000), 'bundle should be less than 1mb')
  })
})


test('start server', function (t) {
  t.plan(1)
  server = require('http').createServer(function (req, res) {
    if (req.url === '/bundle.js') {
      fs.readFile('./test/sandbox' + req.url, function (err, file) {
        if (err) console.log('problem setting up test server ' + err)
        res.writeHeader(200)
        res.write(file)
        res.end()
      })
    } else {
      res.writeHeader(200, {'Content-Type': 'text/html'})
      res.write('<script src="bundle.js"></script>')
      res.write('<div name="result" id="result">waiting...</div>')
      res.end()
    }
  }).listen(8080, function (err) {
    t.error(err)
  })
})


test('connect to test html page', function (t) {
  t.plan(2)
  var webdriver = require('selenium-webdriver')
  var browser

  if (process.env.SAUCE_USERNAME != undefined) {
    browser = new webdriver.Builder()
      .usingServer('http://'+ process.env.SAUCE_USERNAME+':'+process.env.SAUCE_ACCESS_KEY+'@ondemand.saucelabs.com:80/wd/hub')
      .withCapabilities({
        'tunnel-identifier': process.env.TRAVIS_JOB_NUMBER,
        build: process.env.TRAVIS_BUILD_NUMBER,
        username: process.env.SAUCE_USERNAME,
        accessKey: process.env.SAUCE_ACCESS_KEY,
        browserName: "chrome"
      }).build();
  } else {
    browser = new webdriver.Builder()
      .withCapabilities({
        browserName: "chrome"
      }).build();
  }
  browser.get('http://localhost:8080')

  // var browser = new webdriver.Builder()
  //   .forBrowser('chrome')
  //   .build()
  // browser.get('http://localhost:8080')
  var resultDiv = browser.findElement(webdriver.By.id('result'))
  resultDiv.getInnerHtml().then(function (html) {
    t.equal(html, 'waiting...')
  })
  browser.wait(webdriver.until.elementTextIs(resultDiv, 'TALKING POINT/BANKAMERICA EQUITY OFFER'), 30000)
  resultDiv.getInnerHtml().then(function (html) {
    t.equal(html, 'TALKING POINT/BANKAMERICA <bac> EQUITY OFFER</bac>')
  })
  browser.quit()
})

test('teardown', function (t) {
  server.close()
  t.end()
})
