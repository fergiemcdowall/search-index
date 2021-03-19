const tv = require('term-vector')
const ngraminator = require('ngraminator')

module.exports.SPLIT = tokens => tokens.match(/[\p{L}\d]+/ug)

module.exports.LOWCASE = (tokens, field, ops) => tokens.map(
  t => ops.caseSensitive ? t : t.toLowerCase()
)

module.exports.NGRAMS = (tokens, field, ops) => {
  const { fields, lengths, join = ' ' } = ops.ngrams
  if (lengths) {
    if (fields.includes(field)) { return ngraminator(tokens, lengths).map(t => t.join(join)) }
  }
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
