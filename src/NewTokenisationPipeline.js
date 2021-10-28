const tv = require('term-vector')
const ngraminator = require('ngraminator')

class NewTokenisationPipeline {
  SPLIT = ([tokens, field, ops]) =>
    Promise.resolve([tokens.match(/[\p{L}\d]+/gu), field, ops])

  SKIP = ([tokens, field, ops]) => [
    ops.skipFields.includes(field) ? [] : tokens,
    field,
    ops
  ]

  LOWCASE = ([tokens, field, ops]) =>
    Promise.resolve([
      tokens.map(t => (ops.caseSensitive ? t : t.toLowerCase())),
      field,
      ops
    ])

  REPLACE = ([tokens, field, ops]) => {
    const { fields, values } = ops.replace
    const replace = () =>
      tokens.reduce((acc, cur) => [cur, ...acc, ...(values[cur] || [])], [])
    if (!values) return Promise.resolve([tokens, field, ops])
    if (!fields) return Promise.resolve([replace(), field, ops])
    if (fields.includes(field)) return Promise.resolve([replace(), field, ops])
    return Promise.resolve([tokens, field, ops])
  }

  NGRAMS = ([tokens, field, ops]) => {
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

  STOPWORDS = ([tokens, field, ops]) => {
    return [
      tokens.filter(t => !ops.stopwords.includes(t.toLowerCase())),
      field,
      ops
    ]
  }

  SCORE_TERM_FREQUENCY = ([tokens, field, ops]) => {
    const v = tv(tokens)
    const mostTokenOccurances = v.reduce(
      (acc, cur) => Math.max(cur.positions.length, acc),
      0
    )
    return Promise.resolve([
      v
        .map(item =>
          // TODO: maybe stringify should be its own pipeline stage
          JSON.stringify([
            item.term[0],
            (item.positions.length / mostTokenOccurances).toFixed(2)
          ])
        )
        // TODO: is this working on Arrays?
        .sort(),
      field,
      ops
    ])
  }

  SPY = ([tokens, field, ops]) => {
    console.log('----------------')
    console.log('field ->')
    console.log(field)
    console.log('tokens ->')
    console.log(tokens)
    console.log('----------------')
    return Promise.resolve([tokens, field, ops])
  }

  tokenizer = (tokens, field, ops) =>
    this.SPLIT([tokens, field, ops])
      .then(this.SKIP)
      .then(this.LOWCASE)
      .then(this.REPLACE)
      .then(this.NGRAMS)
      .then(this.STOPWORDS)
      .then(this.SCORE_TERM_FREQUENCY)
      .then(([tokens, field, ops]) => tokens)
}

module.exports = NewTokenisationPipeline
