const fs = require('fs')
const json = require('./EarthPorn-top-raw.json')

fs.writeFileSync(
  __dirname + '/EarthPorn-top-processed.json',
  JSON.stringify(
    json.map((r, i) => ({
      _id: i + '',
      created_utc: r.data.created_utc,
      title: r.data.title,
      thumbnail: r.data.thumbnail,
      url_overridden_by_dest: r.data.url_overridden_by_dest
    })), null, 2
  )
)
