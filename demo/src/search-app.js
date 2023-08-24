/* global renderFacet, renderResult, renderFilter, autocomplete, stopwords, SearchIndex, fetch */

// store query state in a global variable to avoid synchronisation
// issues when reading and writing quickly to the DOM
let queryState = ''
// search index
let si = null

// shorthand convenience function
const el = name => document.getElementById(name)

// render page
const renderResults = (q, { RESULT, FACETS, RESULT_LENGTH }) => {
  console.log('IN RENDER RESULTS')
  console.log(RESULT_LENGTH)
  const getFacet = name =>
    FACETS.filter(f => f.FIELD === name).filter(f => f._id.length)
  const yearFacet = getFacet('year')
  const monthFacet = getFacet('month')
  el('query').value = queryState
  el('resultLength').innerHTML = queryState.length
    ? `${RESULT_LENGTH} hits for "${queryState}"`
    : ''
  el('result').innerHTML = RESULT.reduce(renderResult, '')
  el('facets').innerHTML =
    (yearFacet.length ? yearFacet.reduce(renderFacet, '<b>Year: </b>') : '') +
    (monthFacet.length
      ? monthFacet.reduce(renderFacet, '<br /><b>Month: </b>')
      : '')
}

/* HANDLE SEARCH */

// search
const searchQuery = q => [
  q.concat(getFilters()),
  {
    FACETS: [
      {
        FIELD: ['month', 'year']
      }
    ],
    DOCUMENTS: true
  }
]

// treat empty search as a special case: instead of showing nothing,
// show everything.
const emptySearchQuery = () => [
  {
    ALL_DOCUMENTS: true
  },
  {
    FACETS: [
      {
        FIELD: ['month', 'year']
      }
    ]
  }
]

const search = (q = '') => {
  queryState = q
  const queryTokens = q.split(/\s+/).filter(item => item)
  const formatResult = result => ({
    query: q,
    result
  })
  return (
    queryTokens.length + getFilters().length
      ? si.SEARCH(...searchQuery(queryTokens)).then(formatResult)
      : si.QUERY(...emptySearchQuery()).then(formatResult)
  ).then(result =>
    el('query').value === result.query // only display if the results are for the terms in the search box
      ? renderResults(queryTokens, result.result)
      : null
  )
}

/* HANDLE FILTERS */

// eslint-disable-next-line no-unused-vars
const addFilter = f => {
  el('filters').innerHTML += renderFilter(f)
  search(queryState)
}

// eslint-disable-next-line no-unused-vars
const removeFilter = f => {
  el(f).remove()
  search(queryState)
}

const getFilters = () =>
  Array.from(document.querySelectorAll('#filters>li')).map(li =>
    li.textContent.trim()
  )

/* INPUT LISTENERS */

// listen for typing and search accordingly
el('query').addEventListener('input', function (e) {
  search(this.value)
})

// listen for typing and create suggestions. Listen for clicked
// suggestions
autocomplete('#query', { hint: false }, [
  {
    source: (query, cb) =>
      // eslint-disable-next-line
      query.length >= 2 ? si.DICTIONARY(query).then(cb) : cb([]),
    templates: { suggestion: suggestion => suggestion }
  }
]).on('autocomplete:selected', function (event, suggestion) {
  search(suggestion)
})

/* INITIALIZE */

// // TODO: put in logic here to see if index is imported or not...
// Promise.all([
//   SearchIndex({
//     name: 'mySearchIndex',
//     stopwords
//   }),
//   fetch('generate-index/EarthPorn-top-search-index.json').then(res =>
//     res.json()
//   )
// ])
//   .then(([thisSi, dump]) => {
//     // set global variable (in practice you might not want to do this)
//     si = thisSi
//     // replicate pregenerated index
//     si.IMPORT(dump).then(search)
//   })
//   .catch(console.log)

si = new SearchIndex.SearchIndex({
  name: 'mySearchIndex',
  stopwords
})

fetch('generate-index/EarthPorn-top-search-index.json')
  .then(res => res.json())
  .then(dump => si.IMPORT(dump))
  .then(search)
