const tv = require('term-vector')
const { ngraminator } = require('ngraminator')

const SPLIT = ([tokens, field, ops]) =>
  Promise.resolve([tokens.match(ops.tokenSplitRegex) || [], field, ops])

const SKIP = ([tokens, field, ops]) => [
  ops.skipFields.includes(field) ? [] : tokens,
  field,
  ops
]

const LOWCASE = ([tokens, field, ops]) =>
  Promise.resolve([
    tokens.map(t => (ops.caseSensitive ? t : t.toLowerCase())),
    field,
    ops
  ])

const REPLACE = ([tokens, field, ops]) => {
  const { fields, values } = ops.replace
  const replace = () =>
    tokens.reduce((acc, cur) => [cur, ...acc, ...(values[cur] || [])], [])
  if (!values) return Promise.resolve([tokens, field, ops])
  if (!fields) return Promise.resolve([replace(), field, ops])
  if (fields.includes(field)) return Promise.resolve([replace(), field, ops])
  return Promise.resolve([tokens, field, ops])
}

const NGRAMS = ([tokens, field, ops]) => {
  let { fields, lengths, join = ' ' } = ops.ngrams
  // if no fields are specified then ngramify all fields
  if (!fields) fields = [field]
  if (lengths) {
    if (fields.includes(field)) {
      // filter tokens to remove undefined, null, etc
      return [
        ngraminator(
          tokens.filter(t => t !== null),
          lengths
        ).map(t => t.join(join)),
        field,
        ops
      ]
    }
  }
  return Promise.resolve([tokens, field, ops])
}

const STOPWORDS = ([tokens, field, ops]) => {
  return [
    tokens.filter(t => !ops.stopwords.includes(t.toLowerCase())),
    field,
    ops
  ]
}

const SCORE_TERM_FREQUENCY = ([tokens, field, ops]) => {
  const v = tv(tokens)
  const mostTokenOccurances = v.reduce(
    (acc, cur) => Math.max(cur.positions.length, acc),
    0
  )
  return Promise.resolve([
    v
      .map(
        item =>
          // TODO: maybe stringify should be its own pipeline stage
          // JSON.stringify(
          [
            item.term[0],
            (item.positions.length / mostTokenOccurances).toFixed(2)
          ]
        // )
      )
      // TODO: is this working on Arrays?
      .sort(),
    field,
    ops
  ])
}

exports.SPY = ([tokens, field, ops]) => {
  console.log('----------------')
  console.log('field ->')
  console.log(field)
  console.log('tokens ->')
  console.log(tokens)
  console.log('----------------')
  return Promise.resolve([tokens, field, ops])
}

exports.tokenizer = (tokens, field, ops) =>
  SPLIT([tokens, field, ops])
    .then(SKIP)
    .then(LOWCASE)
    .then(REPLACE)
    .then(NGRAMS)
    .then(STOPWORDS)
    .then(SCORE_TERM_FREQUENCY)
    .then(([tokens, field, ops]) => tokens)

exports.SPLIT = SPLIT
exports.SKIP = SKIP
exports.LOWACASE = LOWCASE
exports.REPLACE = REPLACE
exports.NGRAMS = NGRAMS
exports.STOPWORDS = STOPWORDS
exports.SCORE_TERM_FREQUENCY = SCORE_TERM_FREQUENCY
