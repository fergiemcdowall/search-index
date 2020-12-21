const stopwords = [
  'about', 'after', 'all', 'also', 'am', 'an', 'and', 'another', 'any', 'are', 'as', 'at', 'be',
  'because', 'been', 'before', 'being', 'between', 'both', 'but', 'by', 'came', 'can',
  'come', 'could', 'did', 'do', 'each', 'for', 'from', 'get', 'got', 'has', 'had',
  'he', 'have', 'her', 'here', 'him', 'himself', 'his', 'how', 'if', 'in', 'into',
  'is', 'it', 'like', 'make', 'many', 'me', 'might', 'more', 'most', 'much', 'must',
  'my', 'never', 'now', 'of', 'on', 'only', 'or', 'other', 'our', 'out', 'over',
  'said', 'same', 'see', 'should', 'since', 'some', 'still', 'such', 'take', 'than',
  'that', 'the', 'their', 'them', 'then', 'there', 'these', 'they', 'this', 'those',
  'through', 'to', 'too', 'under', 'up', 'very', 'was', 'way', 'we', 'well', 'were',
  'what', 'where', 'which', 'while', 'who', 'with', 'would', 'you', 'your', 'a', 'i']

const renderFacet = (acc, cur) => acc + `
  <li>
    <div>`
      + (
        document.getElementById(cur.FIELD + ':' + cur.VALUE)
          ? '<a>'
          : `<a href="/#" onclick="addFilter('${cur.FIELD}:${cur.VALUE}')">`
      )
      + `${cur.VALUE}
      </a>
      (${cur._id.length})
    </div>
  </li>
`

const renderResult = (acc, { _doc }) => acc + `
  <li class="hit">
    <a href="https://www.reddit.com${_doc.permalink}" target="_blank">
      <img src="${_doc.thumbnail}" />
    </a>
    <p>      
      ${_doc.title}
      <br /><br />
      <a href="https://www.reddit.com/user/${_doc.author}"
         target="_blank">
        ${_doc.author}</a>
      ${_doc.month}
      ${_doc.year}
    </p>
  </li>
`

const renderFilter = f => `
  <li id="${f}" class="filter">
    <a href="/#" onclick=removeFilter('${f}')>${f}</a>
  </li>
`

const addFilter = (f, q) => {
  document.getElementById('filters').innerHTML += renderFilter(f)
  search(globalq)
}


var globalq = []

const renderResults = (q, { RESULT, FACETS, RESULT_LENGTH }) => {

  const getFacet = name => FACETS
        .filter(f => f.FIELD === name)
        .filter(f => f._id.length)
  
  const yearFacet = getFacet('year')
  const monthFacet = getFacet('month')

  globalq = q
  document.getElementById('query').value = q
  
  document.getElementById('resultLength').innerHTML =
    (q.length)
    ? `${RESULT_LENGTH} hits for "${q}"`
    : ''
  
  document.getElementById('result').innerHTML =
    RESULT.reduce(renderResult, '')

  document.getElementById('facets').innerHTML =
     ((yearFacet.length) ? yearFacet.reduce(renderFacet, '<h3>Year</h3>') : '')
    + ((monthFacet.length) ? monthFacet.reduce(renderFacet, '<h3>Month</h3>') : '')
}

const searchQuery = q => [{
  SEARCH: q.concat(getFilters())
}, {
  FACETS: [{
    FIELD: [ 'month', 'year' ]
  }],
  DOCUMENTS: true
}]

const emptySearchQuery = () => [{
  DOCUMENTS: true
}, {
  FACETS: [{
    FIELD: [ 'month', 'year' ]
  }]
}]

const search = (q = []) => {
  return (
    (q.length + getFilters().length)
      ? si.QUERY(...searchQuery(q))
      : si.QUERY(...emptySearchQuery())
  ).then(result => renderResults(q, result))
}

const removeFilter = f => {
  document.getElementById(f).remove()
  search(globalq)
}

const getFilters = () => Array.from(
  document.querySelectorAll('#filters>li')
).map(li => li.textContent.trim())

// init
Promise.all([
  SearchIndex({
    name: 'mySearchIndex',
    stopwords: stopwords
  }),
  fetch('./EarthPorn-top-search-index.json').then(res => res.json())
]).then(([ thisSi, dump ]) => {
  // set global variable (in practice you might not want to do this)
  si = thisSi
  // replicate pregenerated index
  si.IMPORT(dump).then(search)
})


document.getElementById('query')
  .addEventListener('input', function (e) {
    search(this.value.trim().split(/\s+/).filter(item => item))
  })

autocomplete('#query', { hint: false }, [{
  source: (query, cb) => (query.length >= 3)
    ? si.DICTIONARY(query).then(cb)
    : cb ([]),
  templates: { suggestion: suggestion => suggestion }
}]).on('autocomplete:selected', function(event, suggestion) {
  search([ suggestion ]);
});
