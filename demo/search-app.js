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
    <img src="${_doc.thumbnail}" />
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

const renderResults = ({ RESULT, FACETS, RESULT_LENGTH }) => {

  const getFacet = name => FACETS
        .filter(f => f.FIELD === name)
        .filter(f => f._id.length)
  
  const yearFacet = getFacet('year')
  const monthFacet = getFacet('month')

  const q = document.getElementById('query').value
  
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

const search = () => {
  const q = document.getElementById('query').value.trim()
        .split(/\s+/).filter(item => item)
  return (
    (q.length + getFilters().length)
      ? si.QUERY(...searchQuery(q))
      : si.QUERY(...emptySearchQuery())
  ).then(renderResults)
}


const addFilter = f => {
  document.getElementById('filters').innerHTML += `
    <li id="${f}" class="filter">
      <a href="/#" onclick=removeFilter('${f}')>${f}</a>
    </li>
  `
  search()
}

const removeFilter = f => {
  document.getElementById(f).remove()
  search()
}


const getFilters = () => Array.from(
  document.querySelectorAll('#filters>li')
).map(li => li.textContent.trim())

// init
Promise.all([
  SearchIndex({ name: 'mySearchIndex' }),
  fetch('./EarthPorn-top-search-index.json').then(res => res.json())
]).then(([ thisSi, dump ]) => {
  // set global variable (in practice you might not want to do this)
  si = thisSi
  // replicate pregenerated index
  si.IMPORT(dump).then(search)
})


document.getElementById('query')
  .addEventListener('input', function (e) {
    search()
  })

