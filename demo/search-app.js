
const renderFacet = () => 'BOOOOM'

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
  ${_doc._id}
    </p>
  </li>
`

const renderResults = ({ RESULT, FACETS }) => {
  const yearFacet = FACETS
        .filter(f => f.FIELD === 'year')
        .filter(f => f._id.length)
  console.log(FACETS)
  console.log(yearFacet)

  document.getElementById('result').innerHTML =
    RESULT.reduce(renderResult, '')

  document.getElementById('facets').innerHTML = `
    <div>
      <b>Year</b>`
    + yearFacet.reduce((acc, cur) => {
      return acc + `<li>
        <div>
          <a href="/#">${cur.VALUE}</a>
          (${cur._id.length})
        </div>
      </li>
    `
    }, '')
    + "</div>"
}

const searchQuery = q => [{
  SEARCH: q
}, {
  FACETS: [{
    FIELD: 'year'
  }],
  DOCUMENTS: true
}]

const emptySearchQuery = () => [{
  DOCUMENTS: true
}, {
  FACETS: [{
    FIELD: 'year'
  }]
}]

const search = q => (
  (q.length)
    ? si.QUERY(...searchQuery(q))
    : si.QUERY(...emptySearchQuery())
).then(res => {
  console.log('rendering')
  return renderResults(res)
})

// init
Promise.all([
  SearchIndex({ name: 'mySearchIndex' }),
  fetch('./EarthPorn-top-search-index.json').then(res => res.json())
]).then(([ thisSi, dump ]) => {
  console.log(dump[0])
  // set global variable (in practice you might not want to do this)
  si = thisSi
  // replicate pregenerated index
  console.log('importing')
  si.IMPORT(dump).then(() => {
    console.log('searching...')
    return search([])
  })
})

document.getElementById('query')
  .addEventListener('input', function (e) {
    console.log('searching...')
    search(this.value.trim().split(/\s+/).filter(item => item))
  })

