export function getAvailableFields (fii) {
  return getRange(fii, {
    gte: '￮FIELD￮',
    lte: '￮FIELD￮￮'
  }).then(fields => fields.map(field => field.split('￮')[2]))
}

export function getRange (fii, q) {
  return new Promise((resolve, reject) => {
    var data = []
    fii.STORE.createKeyStream(q)
      .on('data', d => data.push(d))
      .on('end', () => resolve(data))
  })
}

export function getDocCount (fii) {
  return fii.STORE.get(
    '￮DOCUMENT_COUNT￮'
  ).catch(
    // if not found assume value to be 0
    e => 0
  )
}
