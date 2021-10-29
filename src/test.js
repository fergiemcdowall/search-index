function * generateId () {
  let i = 0
  while (true) {
    yield Date.now() + '-' + i++
  }
}

const idGen = generateId()

console.log(idGen.next())
console.log(idGen.next())
console.log(idGen.next())
