const tv = require('term-vector')
const ngraminator = require('ngraminator')

module.exports.SPLIT = tokens => tokens.match(/[\p{L}\d]+/ug)

module.exports.LOWCASE = (tokens, field, ops) => tokens.map(
  t => ops.caseSensitive ? t : t.toLowerCase()
)

module.exports.NGRAMS = (tokens, field, ops) => {
  console.log('in NGRAMS pipeline stage')
  const { fields, ngramOps } = ops.ngrams
  console.log(fields)
  console.log(ngramOps)
  return tokens
}

module.exports.SCORE_TFIDF = tokens => {
  const v = tv(tokens)
  const mostTokenOccurances = v.reduce((acc, cur) => Math.max(cur.positions.length, acc), 0)
  return v.map(
    item => item.term[0] + '#' + (
      ((item.positions.length / mostTokenOccurances)).toFixed(2)
    )
  ).sort()
}
