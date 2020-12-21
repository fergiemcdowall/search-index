const fs = require('fs')
const json = require('./EarthPorn-top-raw.json')

fs.writeFileSync(
  __dirname + '/EarthPorn-top-processed.json',
  JSON.stringify(
    json.map((r, i) => {
      const d = new Date(r.data.created_utc * 1000)
      return {
        _id: i + '',
        author: r.data.author,
        created_utc: r.data.created_utc,
        month: d.toLocaleString('default', { month: 'long' }),
        permalink: r.data.permalink,
        year: d.getFullYear(),
        title: r.data.title,
        thumbnail: r.data.thumbnail,
        url_overridden_by_dest: r.data.url_overridden_by_dest
      }
    }), null, 2
  )
)
