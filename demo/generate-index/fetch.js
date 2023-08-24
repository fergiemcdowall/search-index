import fs from 'fs'
import https from 'https'
import path from 'path'
import { URL } from 'url'

const maxResults = 300

let results = []

const __dirname = new URL('.', import.meta.url).pathname

const getPage = (sub, cat, after) =>
  new Promise((resolve, reject) => {
    const url =
      'https://www.reddit.com/r/' +
      sub +
      '/' +
      cat +
      '/.json?limit=100&t=all&after=' +
      after
    https.get(
      url,
      {
        headers: {
          'User-agent': 'the ferginator'
        }
      },
      res => {
        console.log('fetching...')
        console.log(url)
        res.setEncoding('utf8')
        let rawData = ''
        res.on('data', chunk => {
          rawData += chunk
        })
        res.on('end', () => {
          try {
            const d = JSON.parse(rawData)
            return resolve([d.data.children, d.data.after])
          } catch (e) {
            return resolve(e.message)
          }
        })
      }
    )
  })

const run = (sub, cat, after) =>
  getPage(sub, cat, after).then(([res, after]) => {
    results = [...results, ...res]
    console.log('fetched ' + results.length + ' links')
    if (results.length < maxResults) run(sub, cat, after)
    else {
      fs.writeFileSync(
        path.join(__dirname, '/' + sub + '-' + cat + '-raw.json'),
        JSON.stringify(results, null, 2)
      )
    }
  })

run('EarthPorn', 'top', '')
