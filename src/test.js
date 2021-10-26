const DocumentProcessor = require('./DocumentProcessor')
const NewTokenisationPipeline = require('./NewTokenisationPipeline')

const docs = [
  {
    _id: '0',
    make: 'Tesla',
    manufacturer: 'Volvo',
    brand: null,
    extraField: 'EXTRA FIELD- w00t!',
    anObj: {
      cheese: 'doodles'
    },
    aRandomArray: [
      'this',
      'should',
      'index',
      {
        so: 'should',
        these: 'values'
      }
    ]
  },
  {
    _id: '1',
    make: 'BMW',
    manufacturer: 'Volvo',
    brand: [null],
    afieldThatIsntHere: undefined,
    nestedValues: {
      foo: 'bar',
      boom: 'diggy',
      _id: 'this should be be be treated normally'
    },
    bands: [
      {
        drums: 'ringo',
        vocals: 'john'
      },
      {
        drums: 'charlie',
        vocals: 'mick',
        equipment: ['bass', 'drums', 'guitar']
      }
    ]
  }
]

new DocumentProcessor({
  tokenizer: new NewTokenisationPipeline().tokenizer,
  skipFields: [],
  ngrams: {},
  replace: {},
  stopwords: []
})
  .processDocuments(docs)
  .then(out => console.log(JSON.stringify(out, null, 2)))
