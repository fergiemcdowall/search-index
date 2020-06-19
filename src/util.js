export default function (fii) {
  const prefetchSearchableFields = () => {
    const tmp = []
    return new Promise((resolve) => {
      fii.STORE.createKeyStream({
        gte: '￮FIELD!',
        lte: '￮FIELD￮￮'
      }).on('data', d => tmp.push(d.split('￮')[2]))
        .on('end', () => resolve(global.searchableFields = tmp))
    })
  }

  const countDocs = () => {
    let i = 0
    return new Promise((resolve) => {
      fii.STORE.createKeyStream({
        gte: '￮DOC￮!',
        lte: '￮DOC￮￮'
      }).on('data', () => i++)
        .on('end', () => resolve(global.D = i))
    })
  }

  const calibrate = () => {
    // can handle lazy opening
    if (fii.STORE.isOpen()) {
      return prefetchSearchableFields().then(countDocs)
    } else setTimeout(calibrate, 1000) // will rerun function every 1000ms until fii.STORE.isOpen()
  }

  return {
    calibrate: calibrate
  }
}
