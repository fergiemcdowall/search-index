module.exports = ops => {
  console.log('initing browser placeholder version of si')
  return new Promise(resolve => {
    resolve({
      placeHolder: () =>
        'this is a placeholder for the search-index browser functionality',
      printAThing: thing => thing
    })
  })
}
