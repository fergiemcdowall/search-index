si = new SearchIndex.SearchIndex({
    name: "mySearchIndex",
    stopwords
});
fetch("data/EarthPorn-top-search-index.json").then((res)=>{
    debugger;
    return res.json();
}).then((dump)=>si.IMPORT(dump)).catch(console.error);
// .then(search)
console.log(SearchIndex);

//# sourceMappingURL=index.67973f04.js.map
