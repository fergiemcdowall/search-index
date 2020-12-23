// eslint-disable-next-line no-unused-vars
const renderFacet = (acc, cur) => acc + `
  <li>
    <div>` +
      (
        document.getElementById(cur.FIELD + ':' + cur.VALUE)
          ? '<a>'
          : `<a href="/#" onclick="addFilter('${cur.FIELD}:${cur.VALUE}')">`
      ) +
      `${cur.VALUE}
      </a>
      (${cur._id.length})
    </div>
  </li>
`

// eslint-disable-next-line no-unused-vars
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

// eslint-disable-next-line no-unused-vars
const renderFilter = f => `
  <li id="${f}" class="filter">
    <a href="/#" onclick=removeFilter('${f}')>${f}</a>
  </li>
`
