/**
 * Options for element used to display total document count
 * @typedef { Object } CountOptions
 * @memberof UI
 * @property { function } [template=count => `${count} hits`] - A <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals">template literal</a> used to render the innerHTML of the count element
 * @property { string } [elementId='count'] - The <code>id</code> of the element to be used
 */
export class Count {
  constructor ({ template = count => `${count} hits`, elementId = 'count' }) {
    this.template = template
    this.el = document.getElementById(elementId)
  }

  update = count => (this.el.innerHTML = this.template(count))
}
