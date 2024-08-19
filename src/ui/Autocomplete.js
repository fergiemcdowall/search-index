/**
 * Options for suggestions
 * @typedef { Object } SuggestionsOptions
 * @memberof UI
 * @property { string } [ elementId="suggestions" ] - The id of the HTML element that contains the suggestions
 * @property { number } [ limit=20 ] - Max suggestions
 * @property { number } [ threshold=2 ] - Amount of keystrokes before suggestions are generated
 */

export class Autocomplete {
  constructor (searchboxEl, search, suggestions = {}) {
    this.autoCompleteFunction = suggestions.autoCompleteFunction
    this.currentFocus = -1
    this.searchboxEl = searchboxEl
    this.search = search
    this.limit = suggestions.limit || 20
    this.threshold = suggestions.threshold || 2
    this.autocompleteListId = searchboxEl.id + '-autocomplete-list'
    this.suggestionsElementId = suggestions.elementId
    searchboxEl.addEventListener('input', this.searchBoxInputListener)
    searchboxEl.addEventListener('keydown', this.searchBoxKeydownListener)
    document.addEventListener('click', e => {
      this.closeAllLists(e.target)
    })
  }

  closeAllLists = elmnt => {
    for (const item of document.getElementsByClassName('autocomplete-items')) {
      if (elmnt !== item && elmnt !== this.searchboxEl) {
        item.parentNode.removeChild(item)
      }
    }
  }

  searchBoxKeydownListener = e => {
    let x = document.getElementById(this.autocompleteListId)
    if (x) x = x.getElementsByTagName('div')
    if (e.keyCode === 40) {
      /* If the arrow DOWN key is pressed,
        increase the currentFocus variable: */
      this.currentFocus++
      /* and and make the current item more visible: */
      this.addActive(x)
    } else if (e.keyCode === 38) {
      // up
      /* If the arrow UP key is pressed,
        decrease the currentFocus variable: */
      this.currentFocus--
      /* and and make the current item more visible: */
      this.addActive(x)
    } else if (e.keyCode === 13 || e.keyCode === 9) {
      /* If the ENTER/TAB key is pressed, prevent the form from being submitted, */
      e.preventDefault()
      if (this.currentFocus > -1) {
        /* and simulate a click on the "active" item: */
        if (x) x[this.currentFocus].click()
      } else {
        // or simply close the suggestion list
        this.closeAllLists()
      }
    } else if (e.keyCode === 27) {
      /* if ESC pressed, close list */
      this.closeAllLists()
    }
  }

  searchBoxInputListener = async e => {
    const suggestions = (await this.autoCompleteFunction(e.target.value)).slice(
      0,
      this.limit
    )
    let b
    const val = e.target.value
    /* close any already open lists of autocompleted values */
    this.closeAllLists()
    if (!val) return false
    if (val.length < this.threshold) return false
    this.currentFocus = -1
    /* create a DIV element that will contain the items (values): */
    const a = document.createElement('DIV')
    a.setAttribute('id', this.autocompleteListId)
    a.setAttribute('class', 'autocomplete-items')
    a.setAttribute('style', 'position: absolute; background-color:cyan;')
    /* append the DIV element as a child of the autocomplete container: */
    document.getElementById(this.suggestionsElementId).appendChild(a)
    /* for each item in the array... */
    suggestions.forEach(sug => {
      /* check if the item starts with the same letters as the text field value: */
      /* create a DIV element for each matching element: */
      b = document.createElement('DIV')
      /* make the matching letters bold: */
      b.innerHTML = '<strong>' + sug.substr(0, val.length) + '</strong>'
      b.innerHTML += sug.substr(val.length)
      /* insert a input field that will hold the current array item's value: */
      b.innerHTML += "<input type='hidden' value='" + sug + "'>"
      /* execute a function when someone clicks on the item value (DIV element): */
      b.addEventListener('click', e => {
        /* insert the value for the autocomplete text field: */
        this.searchboxEl.value = e.target.getElementsByTagName('input')[0].value
        /* close the list of autocompleted values,
              (or any other open lists of autocompleted values: */
        this.closeAllLists()
        this.search()
      })
      a.appendChild(b)
    })
  }

  addActive = x => {
    /* a function to classify an item as "active": */
    if (!x) return false
    /* start by removing the "active" class on all items: */
    this.removeActive(x)
    if (this.currentFocus >= x.length) this.currentFocus = 0
    if (this.currentFocus < 0) this.currentFocus = x.length - 1
    /* add class "autocomplete-active": */
    x[this.currentFocus].classList.add('autocomplete-active')
  }

  removeActive = x => {
    /* a function to remove the "active" class from all autocomplete items: */
    for (let i = 0; i < x.length; i++) {
      x[i].classList.remove('autocomplete-active')
    }
  }
}
