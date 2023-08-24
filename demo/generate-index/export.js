import fs from 'fs'
import path from 'path'
// import data from './EarthPorn-top-processed.json' assert { type: 'json' }
import { SearchIndex } from 'search-index'
import { URL } from 'url'

const data = JSON.parse(fs.readFileSync('./EarthPorn-top-processed.json'))

const __dirname = new URL('.', import.meta.url).pathname

const stopwords = [
  'about',
  'after',
  'all',
  'also',
  'am',
  'an',
  'and',
  'another',
  'any',
  'are',
  'as',
  'at',
  'be',
  'because',
  'been',
  'before',
  'being',
  'between',
  'both',
  'but',
  'by',
  'came',
  'can',
  'come',
  'could',
  'did',
  'do',
  'each',
  'for',
  'from',
  'get',
  'got',
  'has',
  'had',
  'he',
  'have',
  'her',
  'here',
  'him',
  'himself',
  'his',
  'how',
  'if',
  'in',
  'into',
  'is',
  'it',
  'like',
  'make',
  'many',
  'me',
  'might',
  'more',
  'most',
  'much',
  'must',
  'my',
  'never',
  'now',
  'of',
  'on',
  'only',
  'or',
  'other',
  'our',
  'out',
  'over',
  'said',
  'same',
  'see',
  'should',
  'since',
  'some',
  'still',
  'such',
  'take',
  'than',
  'that',
  'the',
  'their',
  'them',
  'then',
  'there',
  'these',
  'they',
  'this',
  'those',
  'through',
  'to',
  'too',
  'under',
  'up',
  'very',
  'was',
  'way',
  'we',
  'well',
  'were',
  'what',
  'where',
  'which',
  'while',
  'who',
  'with',
  'would',
  'you',
  'your',
  'a',
  'i'
]

const { INDEX, EXPORT, PUT } = new SearchIndex({
  name: path.join(__dirname, '/earthporn'),
  stopwords
})

INDEX.STORE.clear()
  .then(() =>
    PUT(data, {
      doNotIndexField: ['thumbnail', 'url_overridden_by_dest']
    })
  )
  .then(() => EXPORT())
  .then(idx =>
    fs.writeFileSync(
      path.join(__dirname, '/EarthPorn-top-search-index.json'),
      JSON.stringify(idx, null, 2)
    )
  )
