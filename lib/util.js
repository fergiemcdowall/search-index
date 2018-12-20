module.exports = fii => {
  const prefetchSearchableFields = () => {
    const tmp = []
    return new Promise((resolve, reject) => {
      fii.STORE.createKeyStream({
        gte: '￮FIELD!',
        lte: '￮FIELD￮￮'
      }).on('data', d => tmp.push(d.split('￮')[2]))
        .on('end', () => resolve(global.searchableFields = tmp))
    })
  }

  const countDocs = () => {
    var i = 0
    return new Promise((resolve, reject) => {
      fii.STORE.createKeyStream({
        gte: '￮DOC￮!',
        lte: '￮DOC￮￮'
      }).on('data', d => i++)
        .on('end', () => resolve(global.D = i))
    })
  }

  return {
    countDocs: countDocs,
    prefetchSearchableFields: prefetchSearchableFields
  }
}
