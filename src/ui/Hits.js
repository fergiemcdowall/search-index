/**
 * Displays each hit
 * @typedef { Object } HitsOptions
 * @memberof UI
 * @property { string } [ elementId="hits" ] - The id of the HTML element that contains all of the hits
 * @property { function } [ template=doc => `<p>${JSON.stringify(doc)}</p>` ] - A template for each hit
 */
export class Hits {
  constructor ({
    template = doc => `<p>${JSON.stringify(doc)}</p>`,
    elementId = 'hits'
  }) {
    this.template = template
    this.el = document.getElementById(elementId)
  }

  update = results =>
    (this.el.innerHTML = results
      .map(({ _doc }) => this.template(_doc))
      .join('\n'))
}
