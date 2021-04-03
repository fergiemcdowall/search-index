const tv = require('term-vector')
const ngraminator = require('ngraminator')

module.exports.DONT_INDEX_FIELD = (tokens, field, ops) => {
  return ops.doNotIndexField.includes(field.toLowerCase()) ? [] : tokens
}

module.exports.SPLIT = tokens => tokens.match(/[\p{L}\d]+/ug)

module.exports.LOWCASE = (tokens, field, ops) => tokens.map(
  t => ops.caseSensitive ? t : t.toLowerCase()
)

module.exports.NGRAMS = (tokens, field, ops) => {
  let { fields, lengths, join = ' ' } = ops.ngrams
  // if no fields are specified then ngramify all fields
  if (!fields) fields = [field]
  if (lengths) {
    if (fields.includes(field)) {
      return ngraminator(tokens, lengths).map(t => t.join(join))
    }
  }
  return tokens
}

module.exports.REPLACE = (tokens, field, ops) => {
  const { fields, values } = ops.replace

  const replace = () => tokens.reduce((acc, cur) =>
    [cur, ...acc, ...(values[cur] || [])], []
  )

  if (!values) return tokens
  if (!fields) return replace()
  if (fields.includes(field)) return replace()
  return tokens
}

module.exports.SCORE_TERM_FREQUENCY = tokens => {
  const v = tv(tokens)
  const mostTokenOccurances = v.reduce((acc, cur) => Math.max(cur.positions.length, acc), 0)
  return v.map(
    item => item.term[0] + '#' + (
      ((item.positions.length / mostTokenOccurances)).toFixed(2)
    )
  ).sort()
}

module.exports.STOPWORDS = (tokens, field, ops) => tokens.filter(
  t => !ops.stopwords.includes(t.toLowerCase())
)

module.exports.SPY = (tokens, field, ops) => {
  console.log('----------------')
  console.log('field ->')
  console.log(field)
  console.log('tokens ->')
  console.log(tokens)
  console.log('----------------')
  return tokens
}
