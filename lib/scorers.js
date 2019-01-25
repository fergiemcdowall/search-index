// TODO: put in some defaults
exports.TFIDF = ops => {
  const calculateScore = (x, i, resultset) => {
    const idf = Math.log((global.D + 1) / resultset.length)
    x.score = +x.match.reduce(
      (acc, cur) => acc + idf * +cur.split(':')[1], 0
    ).toFixed(2) // TODO: make precision an option
    return x
  }
  return ops
    .resultSet
    .map(calculateScore)
  // sort by score descending
    .sort((a, b) => b.score - a.score)
  // limit to n hits
    .slice(ops.offset, ops.limit)
}

// TODO: put in some defaults
exports.numericField = ops => {
  const calculateScore = (x, i, resultset) => {
    x.score = +x.match.filter(
      item => item.startsWith(ops.fieldName)
    )[0].split(':')[1]
    return x
  }
  return ops
    .resultSet
    .map(calculateScore)
  // sort by score descending
    .sort(ops.sort)
  // limit to n hits
    .slice(ops.offset, ops.limit)
}
