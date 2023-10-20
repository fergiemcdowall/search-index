/* global renderFacet, renderResult, renderFilter, autocomplete, stopwords, SearchIndex, fetch */

// store query state in a global variable to avoid synchronisation
// issues when reading and writing quickly to the DOM
const queryState = ''
// search index
let si = null

// shorthand convenience function
const el = name => document.getElementById(name)

// render page
const renderResults = (q, { RESULT, FACETS, RESULT_LENGTH }) => {
  const getFacet = name =>
    FACETS.filter(f => f.FIELD === name).filter(f => f._id.length)
  const yearFacet = getFacet('year')
  const monthFacet = getFacet('month')
  // el('query').value = queryState
  el('resultLength').innerHTML = queryState.length
    ? `${RESULT_LENGTH} hits for "${queryState}"`
    : ''
  el('result').innerHTML = RESULT.reduce(renderResult, '')
  el('facets').innerHTML =
    (yearFacet.length ? yearFacet.reduce(renderFacet, '<b>Years: </b>') : '') +
    (monthFacet.length
      ? monthFacet.reduce(renderFacet, '<br /><b>Months: </b>')
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

// const search = (q = '') => {
//   queryState = q
//   const queryTokens = q.split(/\s+/).filter(item => item)
//   const formatResult = result => ({
//     query: q,
//     result
//   })
//   return (
//     queryTokens.length + getFilters().length
//       ? si.SEARCH(...searchQuery(queryTokens)).then(formatResult)
//       : si.QUERY(...emptySearchQuery()).then(formatResult)
//   ).then(result =>
//     el('query').value === result.query // only display if the results are for the terms in the search box
//       ? renderResults(queryTokens, result.result)
//       : null
//   )
// }

const getSearchString = () =>
  new URL(window.location).searchParams.get('q') || ''

const search = () => {
  console.log('in search...')
  const q = getSearchString()
    .split(/\s+/)
    .filter(item => item)
  ;(q.length ? si.SEARCH(...searchQuery(q)) : si.QUERY(...emptySearchQuery()))
    .then(result => ({
      query: q,
      result
    }))
    .then(result => {
      console.log(result)
      return result
    })
    .then(result => renderResults(q, result.result))
}

/* HANDLE FILTERS */

const getFilters = () => new URL(window.location).searchParams.getAll('filter')

const removeParam = (name, value) => {
  const url = new URL(window.location)
  const params = new URLSearchParams(url.search)
  params.delete(name, value)
  url.search = '?' + params.toString()
  return url.toString()
}

/* INPUT LISTENERS */

autocomplete({
  initialState: {
    // This uses the `search` query parameter as the initial query
    query: new URL(window.location).searchParams.get('q')
  },
  container: '#searchbox',
  placeholder: 'Search for pictures of nature',
  getSources({ query }) {
    return [
      {
        sourceId: 'dictionary',
        getItems({ query }) {
          return si.DICTIONARY(query).then(res => [
            ...res.map(item => ({
              label: item,
              value: item
            })),
            { label: 'clear', value: '' }
          ])
        },
        getItemUrl({ item }) {
          console.log('fetching ... ' + item.label)
          return '?q=' + item.label
        },
        templates: {
          item({ item, html }) {
            return html`<a href="?q=${item.value}">${item.label}</a>`
          }
        }
      }

      // {
      //   sourceId: 'freetext',
      //   getItems ({ query }) {
      //     return [
      //       {
      //         label: query
      //       }
      //     ]
      //   },
      //   getItemUrl ({ item }) {
      //     return '?q=' + item.label
      //   },
      //   templates: {
      //     item ({ item, html }) {
      //       return html`<a>${item.label}</a>`
      //     }
      //   }
      // }
    ]
  }
})

/* INITIALIZE */

si = new SearchIndex.SearchIndex({
  name: 'mySearchIndex',
  stopwords
})

// TODO: this should only kick in when the index needs to be updated
fetch('generate-index/EarthPorn-top-search-index.json')
  .then(res => res.json())
  .then(dump => si.IMPORT(dump))
  .then(search)

getFilters().forEach(f => (el('filters').innerHTML += renderFilter(f)))

if (getSearchString()) {
  el('query').innerHTML =
    '<b>Results for </b><i>"' + getSearchString() + '"</i>'
}
