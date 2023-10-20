/*! For license information please see search-index-5.0.0-rc1.js.LICENSE.txt */ var SearchIndex;
(()=>{
    var t = {
        464: (t, e, i)=>{
            "use strict";
            const { fromCallback: n } = i(957), r = i(473), { getCallback: s, getOptions: o } = i(520), h = Symbol("promise"), a = Symbol("status"), c = Symbol("operations"), u = Symbol("finishClose"), l = Symbol("closeCallbacks");
            e.AbstractChainedBatch = class {
                constructor(t){
                    if ("object" != typeof t || null === t) throw new TypeError("The first argument must be an abstract-level database, received " + (null === t ? "null" : typeof t));
                    this[c] = [], this[l] = [], this[a] = "open", this[u] = this[u].bind(this), this.db = t, this.db.attachResource(this), this.nextTick = t.nextTick;
                }
                get length() {
                    return this[c].length;
                }
                put(t, e, i) {
                    if ("open" !== this[a]) throw new r("Batch is not open: cannot call put() after write() or close()", {
                        code: "LEVEL_BATCH_NOT_OPEN"
                    });
                    const n = this.db._checkKey(t) || this.db._checkValue(e);
                    if (n) throw n;
                    const s = i && null != i.sublevel ? i.sublevel : this.db, o = i, h = s.keyEncoding(i && i.keyEncoding), u = s.valueEncoding(i && i.valueEncoding), l = h.format;
                    i = {
                        ...i,
                        keyEncoding: l,
                        valueEncoding: u.format
                    }, s !== this.db && (i.sublevel = null);
                    const f = s.prefixKey(h.encode(t), l), d = u.encode(e);
                    return this._put(f, d, i), this[c].push({
                        ...o,
                        type: "put",
                        key: t,
                        value: e
                    }), this;
                }
                _put(t, e, i) {}
                del(t, e) {
                    if ("open" !== this[a]) throw new r("Batch is not open: cannot call del() after write() or close()", {
                        code: "LEVEL_BATCH_NOT_OPEN"
                    });
                    const i = this.db._checkKey(t);
                    if (i) throw i;
                    const n = e && null != e.sublevel ? e.sublevel : this.db, s = e, o = n.keyEncoding(e && e.keyEncoding), h = o.format;
                    return e = {
                        ...e,
                        keyEncoding: h
                    }, n !== this.db && (e.sublevel = null), this._del(n.prefixKey(o.encode(t), h), e), this[c].push({
                        ...s,
                        type: "del",
                        key: t
                    }), this;
                }
                _del(t, e) {}
                clear() {
                    if ("open" !== this[a]) throw new r("Batch is not open: cannot call clear() after write() or close()", {
                        code: "LEVEL_BATCH_NOT_OPEN"
                    });
                    return this._clear(), this[c] = [], this;
                }
                _clear() {}
                write(t, e) {
                    return e = s(t, e), e = n(e, h), t = o(t), "open" !== this[a] ? this.nextTick(e, new r("Batch is not open: cannot call write() after write() or close()", {
                        code: "LEVEL_BATCH_NOT_OPEN"
                    })) : 0 === this.length ? this.close(e) : (this[a] = "writing", this._write(t, (t)=>{
                        this[a] = "closing", this[l].push(()=>e(t)), t || this.db.emit("batch", this[c]), this._close(this[u]);
                    })), e[h];
                }
                _write(t, e) {}
                close(t) {
                    return t = n(t, h), "closing" === this[a] ? this[l].push(t) : "closed" === this[a] ? this.nextTick(t) : (this[l].push(t), "writing" !== this[a] && (this[a] = "closing", this._close(this[u]))), t[h];
                }
                _close(t) {
                    this.nextTick(t);
                }
                [u]() {
                    this[a] = "closed", this.db.detachResource(this);
                    const t = this[l];
                    this[l] = [];
                    for (const e of t)e();
                }
            };
        },
        961: (t, e, i)=>{
            "use strict";
            const { fromCallback: n } = i(957), r = i(473), { getOptions: s, getCallback: o } = i(520), h = Symbol("promise"), a = Symbol("callback"), c = Symbol("working"), u = Symbol("handleOne"), l = Symbol("handleMany"), f = Symbol("autoClose"), d = Symbol("finishWork"), p = Symbol("returnMany"), y = Symbol("closing"), E = Symbol("handleClose"), g = Symbol("closed"), m = Symbol("closeCallbacks"), v = Symbol("keyEncoding"), b = Symbol("valueEncoding"), T = Symbol("abortOnClose"), w = Symbol("legacy"), _ = Symbol("keys"), S = Symbol("values"), L = Symbol("limit"), A = Symbol("count"), O = Object.freeze({}), I = ()=>{};
            let x = !1;
            class k {
                constructor(t, e, i){
                    if ("object" != typeof t || null === t) throw new TypeError("The first argument must be an abstract-level database, received " + (null === t ? "null" : typeof t));
                    if ("object" != typeof e || null === e) throw new TypeError("The second argument must be an options object");
                    this[g] = !1, this[m] = [], this[c] = !1, this[y] = !1, this[f] = !1, this[a] = null, this[u] = this[u].bind(this), this[l] = this[l].bind(this), this[E] = this[E].bind(this), this[v] = e[v], this[b] = e[b], this[w] = i, this[L] = Number.isInteger(e.limit) && e.limit >= 0 ? e.limit : 1 / 0, this[A] = 0, this[T] = !!e.abortOnClose, this.db = t, this.db.attachResource(this), this.nextTick = t.nextTick;
                }
                get count() {
                    return this[A];
                }
                get limit() {
                    return this[L];
                }
                next(t) {
                    let e;
                    if (void 0 === t) e = new Promise((e, i)=>{
                        t = (t, n, r)=>{
                            t ? i(t) : this[w] ? void 0 === n && void 0 === r ? e() : e([
                                n,
                                r
                            ]) : e(n);
                        };
                    });
                    else if ("function" != typeof t) throw new TypeError("Callback must be a function");
                    return this[y] ? this.nextTick(t, new r("Iterator is not open: cannot call next() after close()", {
                        code: "LEVEL_ITERATOR_NOT_OPEN"
                    })) : this[c] ? this.nextTick(t, new r("Iterator is busy: cannot call next() until previous call has completed", {
                        code: "LEVEL_ITERATOR_BUSY"
                    })) : (this[c] = !0, this[a] = t, this[A] >= this[L] ? this.nextTick(this[u], null) : this._next(this[u])), e;
                }
                _next(t) {
                    this.nextTick(t);
                }
                nextv(t, e, i) {
                    return i = o(e, i), i = n(i, h), e = s(e, O), Number.isInteger(t) ? (this[y] ? this.nextTick(i, new r("Iterator is not open: cannot call nextv() after close()", {
                        code: "LEVEL_ITERATOR_NOT_OPEN"
                    })) : this[c] ? this.nextTick(i, new r("Iterator is busy: cannot call nextv() until previous call has completed", {
                        code: "LEVEL_ITERATOR_BUSY"
                    })) : (t < 1 && (t = 1), this[L] < 1 / 0 && (t = Math.min(t, this[L] - this[A])), this[c] = !0, this[a] = i, t <= 0 ? this.nextTick(this[l], null, []) : this._nextv(t, e, this[l])), i[h]) : (this.nextTick(i, new TypeError("The first argument 'size' must be an integer")), i[h]);
                }
                _nextv(t, e, i) {
                    const n = [], r = (e, s, o)=>e ? i(e) : (this[w] ? void 0 === s && void 0 === o : void 0 === s) ? i(null, n) : (n.push(this[w] ? [
                            s,
                            o
                        ] : s), void (n.length === t ? i(null, n) : this._next(r)));
                    this._next(r);
                }
                all(t, e) {
                    return e = o(t, e), e = n(e, h), t = s(t, O), this[y] ? this.nextTick(e, new r("Iterator is not open: cannot call all() after close()", {
                        code: "LEVEL_ITERATOR_NOT_OPEN"
                    })) : this[c] ? this.nextTick(e, new r("Iterator is busy: cannot call all() until previous call has completed", {
                        code: "LEVEL_ITERATOR_BUSY"
                    })) : (this[c] = !0, this[a] = e, this[f] = !0, this[A] >= this[L] ? this.nextTick(this[l], null, []) : this._all(t, this[l])), e[h];
                }
                _all(t, e) {
                    let i = this[A];
                    const n = [], r = ()=>{
                        const t = this[L] < 1 / 0 ? Math.min(1e3, this[L] - i) : 1e3;
                        t <= 0 ? this.nextTick(e, null, n) : this._nextv(t, O, s);
                    }, s = (t, s)=>{
                        t ? e(t) : 0 === s.length ? e(null, n) : (n.push.apply(n, s), i += s.length, r());
                    };
                    r();
                }
                [d]() {
                    const t = this[a];
                    return this[T] && null === t ? I : (this[c] = !1, this[a] = null, this[y] && this._close(this[E]), t);
                }
                [p](t, e, i) {
                    this[f] ? this.close(t.bind(null, e, i)) : t(e, i);
                }
                seek(t, e) {
                    if (e = s(e, O), this[y]) ;
                    else {
                        if (this[c]) throw new r("Iterator is busy: cannot call seek() until next() has completed", {
                            code: "LEVEL_ITERATOR_BUSY"
                        });
                        {
                            const i = this.db.keyEncoding(e.keyEncoding || this[v]), n = i.format;
                            e.keyEncoding !== n && (e = {
                                ...e,
                                keyEncoding: n
                            });
                            const r = this.db.prefixKey(i.encode(t), n);
                            this._seek(r, e);
                        }
                    }
                }
                _seek(t, e) {
                    throw new r("Iterator does not support seek()", {
                        code: "LEVEL_NOT_SUPPORTED"
                    });
                }
                close(t) {
                    return t = n(t, h), this[g] ? this.nextTick(t) : this[y] ? this[m].push(t) : (this[y] = !0, this[m].push(t), this[c] ? this[T] && this[d]()(new r("Aborted on iterator close()", {
                        code: "LEVEL_ITERATOR_NOT_OPEN"
                    })) : this._close(this[E])), t[h];
                }
                _close(t) {
                    this.nextTick(t);
                }
                [E]() {
                    this[g] = !0, this.db.detachResource(this);
                    const t = this[m];
                    this[m] = [];
                    for (const e of t)e();
                }
                async *[Symbol.asyncIterator]() {
                    try {
                        let t;
                        for(; void 0 !== (t = await this.next());)yield t;
                    } finally{
                        this[g] || await this.close();
                    }
                }
            }
            class C extends k {
                constructor(t, e){
                    super(t, e, !0), this[_] = !1 !== e.keys, this[S] = !1 !== e.values;
                }
                [u](t, e, i) {
                    const n = this[d]();
                    if (t) return n(t);
                    try {
                        e = this[_] && void 0 !== e ? this[v].decode(e) : void 0, i = this[S] && void 0 !== i ? this[b].decode(i) : void 0;
                    } catch (t) {
                        return n(new D("entry", t));
                    }
                    void 0 === e && void 0 === i || this[A]++, n(null, e, i);
                }
                [l](t, e) {
                    const i = this[d]();
                    if (t) return this[p](i, t);
                    try {
                        for (const t of e){
                            const e = t[0], i = t[1];
                            t[0] = this[_] && void 0 !== e ? this[v].decode(e) : void 0, t[1] = this[S] && void 0 !== i ? this[b].decode(i) : void 0;
                        }
                    } catch (t) {
                        return this[p](i, new D("entries", t));
                    }
                    this[A] += e.length, this[p](i, null, e);
                }
                end(t) {
                    return x || "undefined" == typeof console || (x = !0, console.warn(new r("The iterator.end() method was renamed to close() and end() is an alias that will be removed in a future version", {
                        code: "LEVEL_LEGACY"
                    }))), this.close(t);
                }
            }
            class D extends r {
                constructor(t, e){
                    super(`Iterator could not decode ${t}`, {
                        code: "LEVEL_DECODE_ERROR",
                        cause: e
                    });
                }
            }
            for (const t of [
                "_ended property",
                "_nexting property",
                "_end method"
            ])Object.defineProperty(C.prototype, t.split(" ")[0], {
                get () {
                    throw new r(`The ${t} has been removed`, {
                        code: "LEVEL_LEGACY"
                    });
                },
                set () {
                    throw new r(`The ${t} has been removed`, {
                        code: "LEVEL_LEGACY"
                    });
                }
            });
            C.keyEncoding = v, C.valueEncoding = b, e.AbstractIterator = C, e.AbstractKeyIterator = class extends k {
                constructor(t, e){
                    super(t, e, !1);
                }
                [u](t, e) {
                    const i = this[d]();
                    if (t) return i(t);
                    try {
                        e = void 0 !== e ? this[v].decode(e) : void 0;
                    } catch (t) {
                        return i(new D("key", t));
                    }
                    void 0 !== e && this[A]++, i(null, e);
                }
                [l](t, e) {
                    const i = this[d]();
                    if (t) return this[p](i, t);
                    try {
                        for(let t = 0; t < e.length; t++){
                            const i = e[t];
                            e[t] = void 0 !== i ? this[v].decode(i) : void 0;
                        }
                    } catch (t) {
                        return this[p](i, new D("keys", t));
                    }
                    this[A] += e.length, this[p](i, null, e);
                }
            }, e.AbstractValueIterator = class extends k {
                constructor(t, e){
                    super(t, e, !1);
                }
                [u](t, e) {
                    const i = this[d]();
                    if (t) return i(t);
                    try {
                        e = void 0 !== e ? this[b].decode(e) : void 0;
                    } catch (t) {
                        return i(new D("value", t));
                    }
                    void 0 !== e && this[A]++, i(null, e);
                }
                [l](t, e) {
                    const i = this[d]();
                    if (t) return this[p](i, t);
                    try {
                        for(let t = 0; t < e.length; t++){
                            const i = e[t];
                            e[t] = void 0 !== i ? this[b].decode(i) : void 0;
                        }
                    } catch (t) {
                        return this[p](i, new D("values", t));
                    }
                    this[A] += e.length, this[p](i, null, e);
                }
            };
        },
        71: (t, e, i)=>{
            "use strict";
            const { supports: n } = i(658), { Transcoder: r } = i(499), { EventEmitter: s } = i(187), { fromCallback: o } = i(957), h = i(473), { AbstractIterator: a } = i(961), { DefaultKeyIterator: c, DefaultValueIterator: u } = i(429), { DeferredIterator: l, DeferredKeyIterator: f, DeferredValueIterator: d } = i(593), { DefaultChainedBatch: p } = i(765), { getCallback: y, getOptions: E } = i(520), g = i(56), m = Symbol("promise"), v = Symbol("landed"), b = Symbol("resources"), T = Symbol("closeResources"), w = Symbol("operations"), _ = Symbol("undefer"), S = Symbol("deferOpen"), L = Symbol("options"), A = Symbol("status"), O = Symbol("defaultOptions"), I = Symbol("transcoder"), x = Symbol("keyEncoding"), k = Symbol("valueEncoding"), C = ()=>{};
            class D extends s {
                constructor(t, e){
                    if (super(), "object" != typeof t || null === t) throw new TypeError("The first argument 'manifest' must be an object");
                    e = E(e);
                    const { keyEncoding: i, valueEncoding: s, passive: o, ...h } = e;
                    this[b] = new Set, this[w] = [], this[S] = !0, this[L] = h, this[A] = "opening", this.supports = n(t, {
                        status: !0,
                        promises: !0,
                        clear: !0,
                        getMany: !0,
                        deferredOpen: !0,
                        snapshots: !1 !== t.snapshots,
                        permanence: !1 !== t.permanence,
                        keyIterator: !0,
                        valueIterator: !0,
                        iteratorNextv: !0,
                        iteratorAll: !0,
                        encodings: t.encodings || {},
                        events: Object.assign({}, t.events, {
                            opening: !0,
                            open: !0,
                            closing: !0,
                            closed: !0,
                            put: !0,
                            del: !0,
                            batch: !0,
                            clear: !0
                        })
                    }), this[I] = new r(U(this)), this[x] = this[I].encoding(i || "utf8"), this[k] = this[I].encoding(s || "utf8");
                    for (const t of this[I].encodings())this.supports.encodings[t.commonName] || (this.supports.encodings[t.commonName] = !0);
                    this[O] = {
                        empty: Object.freeze({}),
                        entry: Object.freeze({
                            keyEncoding: this[x].commonName,
                            valueEncoding: this[k].commonName
                        }),
                        key: Object.freeze({
                            keyEncoding: this[x].commonName
                        })
                    }, this.nextTick(()=>{
                        this[S] && this.open({
                            passive: !1
                        }, C);
                    });
                }
                get status() {
                    return this[A];
                }
                keyEncoding(t) {
                    return this[I].encoding(null != t ? t : this[x]);
                }
                valueEncoding(t) {
                    return this[I].encoding(null != t ? t : this[k]);
                }
                open(t, e) {
                    e = y(t, e), e = o(e, m), (t = {
                        ...this[L],
                        ...E(t)
                    }).createIfMissing = !1 !== t.createIfMissing, t.errorIfExists = !!t.errorIfExists;
                    const i = (t)=>{
                        "closing" === this[A] || "opening" === this[A] ? this.once(v, t ? ()=>i(t) : i) : "open" !== this[A] ? e(new h("Database is not open", {
                            code: "LEVEL_DATABASE_NOT_OPEN",
                            cause: t
                        })) : e();
                    };
                    return t.passive ? "opening" === this[A] ? this.once(v, i) : this.nextTick(i) : "closed" === this[A] || this[S] ? (this[S] = !1, this[A] = "opening", this.emit("opening"), this._open(t, (t)=>{
                        if (t) return this[A] = "closed", this[T](()=>{
                            this.emit(v), i(t);
                        }), void this[_]();
                        this[A] = "open", this[_](), this.emit(v), "open" === this[A] && this.emit("open"), "open" === this[A] && this.emit("ready"), i();
                    })) : "open" === this[A] ? this.nextTick(i) : this.once(v, ()=>this.open(t, e)), e[m];
                }
                _open(t, e) {
                    this.nextTick(e);
                }
                close(t) {
                    t = o(t, m);
                    const e = (i)=>{
                        "opening" === this[A] || "closing" === this[A] ? this.once(v, i ? e(i) : e) : "closed" !== this[A] ? t(new h("Database is not closed", {
                            code: "LEVEL_DATABASE_NOT_CLOSED",
                            cause: i
                        })) : t();
                    };
                    if ("open" === this[A]) {
                        this[A] = "closing", this.emit("closing");
                        const t = (t)=>{
                            this[A] = "open", this[_](), this.emit(v), e(t);
                        };
                        this[T](()=>{
                            this._close((i)=>{
                                if (i) return t(i);
                                this[A] = "closed", this[_](), this.emit(v), "closed" === this[A] && this.emit("closed"), e();
                            });
                        });
                    } else "closed" === this[A] ? this.nextTick(e) : this.once(v, ()=>this.close(t));
                    return t[m];
                }
                [T](t) {
                    if (0 === this[b].size) return this.nextTick(t);
                    let e = this[b].size, i = !0;
                    const n = ()=>{
                        0 == --e && (i ? this.nextTick(t) : t());
                    };
                    for (const t of this[b])t.close(n);
                    i = !1, this[b].clear();
                }
                _close(t) {
                    this.nextTick(t);
                }
                get(t, e, i) {
                    if (i = y(e, i), i = o(i, m), e = E(e, this[O].entry), "opening" === this[A]) return this.defer(()=>this.get(t, e, i)), i[m];
                    if (N(this, i)) return i[m];
                    const n = this._checkKey(t);
                    if (n) return this.nextTick(i, n), i[m];
                    const r = this.keyEncoding(e.keyEncoding), s = this.valueEncoding(e.valueEncoding), a = r.format, c = s.format;
                    return e.keyEncoding === a && e.valueEncoding === c || (e = Object.assign({}, e, {
                        keyEncoding: a,
                        valueEncoding: c
                    })), this._get(this.prefixKey(r.encode(t), a), e, (t, e)=>{
                        if (t) return ("LEVEL_NOT_FOUND" === t.code || t.notFound || /NotFound/i.test(t)) && (t.code || (t.code = "LEVEL_NOT_FOUND"), t.notFound || (t.notFound = !0), t.status || (t.status = 404)), i(t);
                        try {
                            e = s.decode(e);
                        } catch (t) {
                            return i(new h("Could not decode value", {
                                code: "LEVEL_DECODE_ERROR",
                                cause: t
                            }));
                        }
                        i(null, e);
                    }), i[m];
                }
                _get(t, e, i) {
                    this.nextTick(i, new Error("NotFound"));
                }
                getMany(t, e, i) {
                    if (i = y(e, i), i = o(i, m), e = E(e, this[O].entry), "opening" === this[A]) return this.defer(()=>this.getMany(t, e, i)), i[m];
                    if (N(this, i)) return i[m];
                    if (!Array.isArray(t)) return this.nextTick(i, new TypeError("The first argument 'keys' must be an array")), i[m];
                    if (0 === t.length) return this.nextTick(i, null, []), i[m];
                    const n = this.keyEncoding(e.keyEncoding), r = this.valueEncoding(e.valueEncoding), s = n.format, a = r.format;
                    e.keyEncoding === s && e.valueEncoding === a || (e = Object.assign({}, e, {
                        keyEncoding: s,
                        valueEncoding: a
                    }));
                    const c = new Array(t.length);
                    for(let e = 0; e < t.length; e++){
                        const r = t[e], o = this._checkKey(r);
                        if (o) return this.nextTick(i, o), i[m];
                        c[e] = this.prefixKey(n.encode(r), s);
                    }
                    return this._getMany(c, e, (t, e)=>{
                        if (t) return i(t);
                        try {
                            for(let t = 0; t < e.length; t++)void 0 !== e[t] && (e[t] = r.decode(e[t]));
                        } catch (t) {
                            return i(new h(`Could not decode one or more of ${e.length} value(s)`, {
                                code: "LEVEL_DECODE_ERROR",
                                cause: t
                            }));
                        }
                        i(null, e);
                    }), i[m];
                }
                _getMany(t, e, i) {
                    this.nextTick(i, null, new Array(t.length).fill(void 0));
                }
                put(t, e, i, n) {
                    if (n = y(i, n), n = o(n, m), i = E(i, this[O].entry), "opening" === this[A]) return this.defer(()=>this.put(t, e, i, n)), n[m];
                    if (N(this, n)) return n[m];
                    const r = this._checkKey(t) || this._checkValue(e);
                    if (r) return this.nextTick(n, r), n[m];
                    const s = this.keyEncoding(i.keyEncoding), h = this.valueEncoding(i.valueEncoding), a = s.format, c = h.format;
                    i.keyEncoding === a && i.valueEncoding === c || (i = Object.assign({}, i, {
                        keyEncoding: a,
                        valueEncoding: c
                    }));
                    const u = this.prefixKey(s.encode(t), a), l = h.encode(e);
                    return this._put(u, l, i, (i)=>{
                        if (i) return n(i);
                        this.emit("put", t, e), n();
                    }), n[m];
                }
                _put(t, e, i, n) {
                    this.nextTick(n);
                }
                del(t, e, i) {
                    if (i = y(e, i), i = o(i, m), e = E(e, this[O].key), "opening" === this[A]) return this.defer(()=>this.del(t, e, i)), i[m];
                    if (N(this, i)) return i[m];
                    const n = this._checkKey(t);
                    if (n) return this.nextTick(i, n), i[m];
                    const r = this.keyEncoding(e.keyEncoding), s = r.format;
                    return e.keyEncoding !== s && (e = Object.assign({}, e, {
                        keyEncoding: s
                    })), this._del(this.prefixKey(r.encode(t), s), e, (e)=>{
                        if (e) return i(e);
                        this.emit("del", t), i();
                    }), i[m];
                }
                _del(t, e, i) {
                    this.nextTick(i);
                }
                batch(t, e, i) {
                    if (!arguments.length) {
                        if ("opening" === this[A]) return new p(this);
                        if ("open" !== this[A]) throw new h("Database is not open", {
                            code: "LEVEL_DATABASE_NOT_OPEN"
                        });
                        return this._chainedBatch();
                    }
                    if (i = "function" == typeof t ? t : y(e, i), i = o(i, m), e = E(e, this[O].empty), "opening" === this[A]) return this.defer(()=>this.batch(t, e, i)), i[m];
                    if (N(this, i)) return i[m];
                    if (!Array.isArray(t)) return this.nextTick(i, new TypeError("The first argument 'operations' must be an array")), i[m];
                    if (0 === t.length) return this.nextTick(i), i[m];
                    const n = new Array(t.length), { keyEncoding: r, valueEncoding: s, ...a } = e;
                    for(let e = 0; e < t.length; e++){
                        if ("object" != typeof t[e] || null === t[e]) return this.nextTick(i, new TypeError("A batch operation must be an object")), i[m];
                        const o = Object.assign({}, t[e]);
                        if ("put" !== o.type && "del" !== o.type) return this.nextTick(i, new TypeError("A batch operation must have a type property that is 'put' or 'del'")), i[m];
                        const h = this._checkKey(o.key);
                        if (h) return this.nextTick(i, h), i[m];
                        const a = null != o.sublevel ? o.sublevel : this, c = a.keyEncoding(o.keyEncoding || r), u = c.format;
                        if (o.key = a.prefixKey(c.encode(o.key), u), o.keyEncoding = u, "put" === o.type) {
                            const t = this._checkValue(o.value);
                            if (t) return this.nextTick(i, t), i[m];
                            const e = a.valueEncoding(o.valueEncoding || s);
                            o.value = e.encode(o.value), o.valueEncoding = e.format;
                        }
                        a !== this && (o.sublevel = null), n[e] = o;
                    }
                    return this._batch(n, a, (e)=>{
                        if (e) return i(e);
                        this.emit("batch", t), i();
                    }), i[m];
                }
                _batch(t, e, i) {
                    this.nextTick(i);
                }
                sublevel(t, e) {
                    return this._sublevel(t, R.defaults(e));
                }
                _sublevel(t, e) {
                    return new R(this, t, e);
                }
                prefixKey(t, e) {
                    return t;
                }
                clear(t, e) {
                    if (e = y(t, e), e = o(e, m), t = E(t, this[O].empty), "opening" === this[A]) return this.defer(()=>this.clear(t, e)), e[m];
                    if (N(this, e)) return e[m];
                    const i = t, n = this.keyEncoding(t.keyEncoding);
                    return (t = g(t, n)).keyEncoding = n.format, 0 === t.limit ? this.nextTick(e) : this._clear(t, (t)=>{
                        if (t) return e(t);
                        this.emit("clear", i), e();
                    }), e[m];
                }
                _clear(t, e) {
                    this.nextTick(e);
                }
                iterator(t) {
                    const e = this.keyEncoding(t && t.keyEncoding), i = this.valueEncoding(t && t.valueEncoding);
                    if ((t = g(t, e)).keys = !1 !== t.keys, t.values = !1 !== t.values, t[a.keyEncoding] = e, t[a.valueEncoding] = i, t.keyEncoding = e.format, t.valueEncoding = i.format, "opening" === this[A]) return new l(this, t);
                    if ("open" !== this[A]) throw new h("Database is not open", {
                        code: "LEVEL_DATABASE_NOT_OPEN"
                    });
                    return this._iterator(t);
                }
                _iterator(t) {
                    return new a(this, t);
                }
                keys(t) {
                    const e = this.keyEncoding(t && t.keyEncoding), i = this.valueEncoding(t && t.valueEncoding);
                    if ((t = g(t, e))[a.keyEncoding] = e, t[a.valueEncoding] = i, t.keyEncoding = e.format, t.valueEncoding = i.format, "opening" === this[A]) return new f(this, t);
                    if ("open" !== this[A]) throw new h("Database is not open", {
                        code: "LEVEL_DATABASE_NOT_OPEN"
                    });
                    return this._keys(t);
                }
                _keys(t) {
                    return new c(this, t);
                }
                values(t) {
                    const e = this.keyEncoding(t && t.keyEncoding), i = this.valueEncoding(t && t.valueEncoding);
                    if ((t = g(t, e))[a.keyEncoding] = e, t[a.valueEncoding] = i, t.keyEncoding = e.format, t.valueEncoding = i.format, "opening" === this[A]) return new d(this, t);
                    if ("open" !== this[A]) throw new h("Database is not open", {
                        code: "LEVEL_DATABASE_NOT_OPEN"
                    });
                    return this._values(t);
                }
                _values(t) {
                    return new u(this, t);
                }
                defer(t) {
                    if ("function" != typeof t) throw new TypeError("The first argument must be a function");
                    this[w].push(t);
                }
                [_]() {
                    if (0 === this[w].length) return;
                    const t = this[w];
                    this[w] = [];
                    for (const e of t)e();
                }
                attachResource(t) {
                    if ("object" != typeof t || null === t || "function" != typeof t.close) throw new TypeError("The first argument must be a resource object");
                    this[b].add(t);
                }
                detachResource(t) {
                    this[b].delete(t);
                }
                _chainedBatch() {
                    return new p(this);
                }
                _checkKey(t) {
                    if (null == t) return new h("Key cannot be null or undefined", {
                        code: "LEVEL_INVALID_KEY"
                    });
                }
                _checkValue(t) {
                    if (null == t) return new h("Value cannot be null or undefined", {
                        code: "LEVEL_INVALID_VALUE"
                    });
                }
            }
            D.prototype.nextTick = i(909);
            const { AbstractSublevel: R } = i(650)({
                AbstractLevel: D
            });
            e.AbstractLevel = D, e.AbstractSublevel = R;
            const N = function(t, e) {
                return "open" !== t[A] && (t.nextTick(e, new h("Database is not open", {
                    code: "LEVEL_DATABASE_NOT_OPEN"
                })), !0);
            }, U = function(t) {
                return Object.keys(t.supports.encodings).filter((e)=>!!t.supports.encodings[e]);
            };
        },
        875: (t, e, i)=>{
            "use strict";
            e.AbstractLevel = i(71).AbstractLevel, e.AbstractSublevel = i(71).AbstractSublevel, e.AbstractIterator = i(961).AbstractIterator, e.AbstractKeyIterator = i(961).AbstractKeyIterator, e.AbstractValueIterator = i(961).AbstractValueIterator, e.AbstractChainedBatch = i(464).AbstractChainedBatch;
        },
        970: (t, e, i)=>{
            "use strict";
            const { AbstractIterator: n, AbstractKeyIterator: r, AbstractValueIterator: s } = i(961), o = Symbol("unfix"), h = Symbol("iterator"), a = Symbol("handleOne"), c = Symbol("handleMany"), u = Symbol("callback");
            class l extends n {
                constructor(t, e, i, n){
                    super(t, e), this[h] = i, this[o] = n, this[a] = this[a].bind(this), this[c] = this[c].bind(this), this[u] = null;
                }
                [a](t, e, i) {
                    const n = this[u];
                    if (t) return n(t);
                    void 0 !== e && (e = this[o](e)), n(t, e, i);
                }
                [c](t, e) {
                    const i = this[u];
                    if (t) return i(t);
                    for (const t of e){
                        const e = t[0];
                        void 0 !== e && (t[0] = this[o](e));
                    }
                    i(t, e);
                }
            }
            class f extends r {
                constructor(t, e, i, n){
                    super(t, e), this[h] = i, this[o] = n, this[a] = this[a].bind(this), this[c] = this[c].bind(this), this[u] = null;
                }
                [a](t, e) {
                    const i = this[u];
                    if (t) return i(t);
                    void 0 !== e && (e = this[o](e)), i(t, e);
                }
                [c](t, e) {
                    const i = this[u];
                    if (t) return i(t);
                    for(let t = 0; t < e.length; t++){
                        const i = e[t];
                        void 0 !== i && (e[t] = this[o](i));
                    }
                    i(t, e);
                }
            }
            class d extends s {
                constructor(t, e, i){
                    super(t, e), this[h] = i;
                }
            }
            for (const t of [
                l,
                f
            ])t.prototype._next = function(t) {
                this[u] = t, this[h].next(this[a]);
            }, t.prototype._nextv = function(t, e, i) {
                this[u] = i, this[h].nextv(t, e, this[c]);
            }, t.prototype._all = function(t, e) {
                this[u] = e, this[h].all(t, this[c]);
            };
            for (const t of [
                d
            ])t.prototype._next = function(t) {
                this[h].next(t);
            }, t.prototype._nextv = function(t, e, i) {
                this[h].nextv(t, e, i);
            }, t.prototype._all = function(t, e) {
                this[h].all(t, e);
            };
            for (const t of [
                l,
                f,
                d
            ])t.prototype._seek = function(t, e) {
                this[h].seek(t, e);
            }, t.prototype._close = function(t) {
                this[h].close(t);
            };
            e.AbstractSublevelIterator = l, e.AbstractSublevelKeyIterator = f, e.AbstractSublevelValueIterator = d;
        },
        650: (t, e, i)=>{
            "use strict";
            const n = i(473), { Buffer: r } = i(764) || {}, { AbstractSublevelIterator: s, AbstractSublevelKeyIterator: o, AbstractSublevelValueIterator: h } = i(970), a = Symbol("prefix"), c = Symbol("upperBound"), u = Symbol("prefixRange"), l = Symbol("parent"), f = Symbol("unfix"), d = new TextEncoder, p = {
                separator: "!"
            };
            t.exports = function({ AbstractLevel: t }) {
                class e extends t {
                    static defaults(t) {
                        if ("string" == typeof t) throw new n("The subleveldown string shorthand for { separator } has been removed", {
                            code: "LEVEL_LEGACY"
                        });
                        if (t && t.open) throw new n("The subleveldown open option has been removed", {
                            code: "LEVEL_LEGACY"
                        });
                        return null == t ? p : t.separator ? t : {
                            ...t,
                            separator: "!"
                        };
                    }
                    constructor(t, i, r){
                        const { separator: s, manifest: o, ...h } = e.defaults(r);
                        i = v(i, s);
                        const u = s.charCodeAt(0) + 1, p = t[l] || t;
                        if (!d.encode(i).every((t)=>t > u && t < 127)) throw new n(`Prefix must use bytes > ${u} < 127`, {
                            code: "LEVEL_INVALID_PREFIX"
                        });
                        super(y(p, o), h);
                        const E = (t.prefix || "") + s + i + s, b = E.slice(0, -1) + String.fromCharCode(u);
                        this[l] = p, this[a] = new g(E), this[c] = new g(b), this[f] = new m, this.nextTick = p.nextTick;
                    }
                    prefixKey(t, e) {
                        if ("utf8" === e) return this[a].utf8 + t;
                        if (0 === t.byteLength) return this[a][e];
                        if ("view" === e) {
                            const e = this[a].view, i = new Uint8Array(e.byteLength + t.byteLength);
                            return i.set(e, 0), i.set(t, e.byteLength), i;
                        }
                        {
                            const e = this[a].buffer;
                            return r.concat([
                                e,
                                t
                            ], e.byteLength + t.byteLength);
                        }
                    }
                    [u](t, e) {
                        void 0 !== t.gte ? t.gte = this.prefixKey(t.gte, e) : void 0 !== t.gt ? t.gt = this.prefixKey(t.gt, e) : t.gte = this[a][e], void 0 !== t.lte ? t.lte = this.prefixKey(t.lte, e) : void 0 !== t.lt ? t.lt = this.prefixKey(t.lt, e) : t.lte = this[c][e];
                    }
                    get prefix() {
                        return this[a].utf8;
                    }
                    get db() {
                        return this[l];
                    }
                    _open(t, e) {
                        this[l].open({
                            passive: !0
                        }, e);
                    }
                    _put(t, e, i, n) {
                        this[l].put(t, e, i, n);
                    }
                    _get(t, e, i) {
                        this[l].get(t, e, i);
                    }
                    _getMany(t, e, i) {
                        this[l].getMany(t, e, i);
                    }
                    _del(t, e, i) {
                        this[l].del(t, e, i);
                    }
                    _batch(t, e, i) {
                        this[l].batch(t, e, i);
                    }
                    _clear(t, e) {
                        this[u](t, t.keyEncoding), this[l].clear(t, e);
                    }
                    _iterator(t) {
                        this[u](t, t.keyEncoding);
                        const e = this[l].iterator(t), i = this[f].get(this[a].utf8.length, t.keyEncoding);
                        return new s(this, t, e, i);
                    }
                    _keys(t) {
                        this[u](t, t.keyEncoding);
                        const e = this[l].keys(t), i = this[f].get(this[a].utf8.length, t.keyEncoding);
                        return new o(this, t, e, i);
                    }
                    _values(t) {
                        this[u](t, t.keyEncoding);
                        const e = this[l].values(t);
                        return new h(this, t, e);
                    }
                }
                return {
                    AbstractSublevel: e
                };
            };
            const y = function(t, e) {
                return {
                    ...t.supports,
                    createIfMissing: !1,
                    errorIfExists: !1,
                    events: {},
                    additionalMethods: {},
                    ...e,
                    encodings: {
                        utf8: E(t, "utf8"),
                        buffer: E(t, "buffer"),
                        view: E(t, "view")
                    }
                };
            }, E = function(t, e) {
                return !!t.supports.encodings[e] && t.keyEncoding(e).name === e;
            };
            class g {
                constructor(t){
                    this.utf8 = t, this.view = d.encode(t), this.buffer = r ? r.from(this.view.buffer, 0, this.view.byteLength) : {};
                }
            }
            class m {
                constructor(){
                    this.cache = new Map;
                }
                get(t, e) {
                    let i = this.cache.get(e);
                    return void 0 === i && (i = "view" === e ? (function(t, e) {
                        return e.subarray(t);
                    }).bind(null, t) : (function(t, e) {
                        return e.slice(t);
                    }).bind(null, t), this.cache.set(e, i)), i;
                }
            }
            const v = function(t, e) {
                let i = 0, n = t.length;
                for(; i < n && t[i] === e;)i++;
                for(; n > i && t[n - 1] === e;)n--;
                return t.slice(i, n);
            };
        },
        520: (t, e)=>{
            "use strict";
            e.getCallback = function(t, e) {
                return "function" == typeof t ? t : e;
            }, e.getOptions = function(t, e) {
                return "object" == typeof t && null !== t ? t : void 0 !== e ? e : {};
            };
        },
        765: (t, e, i)=>{
            "use strict";
            const { AbstractChainedBatch: n } = i(464), r = i(473), s = Symbol("encoded");
            e.DefaultChainedBatch = class extends n {
                constructor(t){
                    super(t), this[s] = [];
                }
                _put(t, e, i) {
                    this[s].push({
                        ...i,
                        type: "put",
                        key: t,
                        value: e
                    });
                }
                _del(t, e) {
                    this[s].push({
                        ...e,
                        type: "del",
                        key: t
                    });
                }
                _clear() {
                    this[s] = [];
                }
                _write(t, e) {
                    "opening" === this.db.status ? this.db.defer(()=>this._write(t, e)) : "open" === this.db.status ? 0 === this[s].length ? this.nextTick(e) : this.db._batch(this[s], t, e) : this.nextTick(e, new r("Batch is not open: cannot call write() after write() or close()", {
                        code: "LEVEL_BATCH_NOT_OPEN"
                    }));
                }
            };
        },
        429: (t, e, i)=>{
            "use strict";
            const { AbstractKeyIterator: n, AbstractValueIterator: r } = i(961), s = Symbol("iterator"), o = Symbol("callback"), h = Symbol("handleOne"), a = Symbol("handleMany");
            class c extends n {
                constructor(t, e){
                    super(t, e), this[s] = t.iterator({
                        ...e,
                        keys: !0,
                        values: !1
                    }), this[h] = this[h].bind(this), this[a] = this[a].bind(this);
                }
            }
            class u extends r {
                constructor(t, e){
                    super(t, e), this[s] = t.iterator({
                        ...e,
                        keys: !1,
                        values: !0
                    }), this[h] = this[h].bind(this), this[a] = this[a].bind(this);
                }
            }
            for (const t of [
                c,
                u
            ]){
                const e = t === c, i = e ? (t)=>t[0] : (t)=>t[1];
                t.prototype._next = function(t) {
                    this[o] = t, this[s].next(this[h]);
                }, t.prototype[h] = function(t, i, n) {
                    const r = this[o];
                    t ? r(t) : r(null, e ? i : n);
                }, t.prototype._nextv = function(t, e, i) {
                    this[o] = i, this[s].nextv(t, e, this[a]);
                }, t.prototype._all = function(t, e) {
                    this[o] = e, this[s].all(t, this[a]);
                }, t.prototype[a] = function(t, e) {
                    const n = this[o];
                    t ? n(t) : n(null, e.map(i));
                }, t.prototype._seek = function(t, e) {
                    this[s].seek(t, e);
                }, t.prototype._close = function(t) {
                    this[s].close(t);
                };
            }
            e.DefaultKeyIterator = c, e.DefaultValueIterator = u;
        },
        593: (t, e, i)=>{
            "use strict";
            const { AbstractIterator: n, AbstractKeyIterator: r, AbstractValueIterator: s } = i(961), o = i(473), h = Symbol("nut"), a = Symbol("undefer"), c = Symbol("factory");
            class u extends n {
                constructor(t, e){
                    super(t, e), this[h] = null, this[c] = ()=>t.iterator(e), this.db.defer(()=>this[a]());
                }
            }
            class l extends r {
                constructor(t, e){
                    super(t, e), this[h] = null, this[c] = ()=>t.keys(e), this.db.defer(()=>this[a]());
                }
            }
            class f extends s {
                constructor(t, e){
                    super(t, e), this[h] = null, this[c] = ()=>t.values(e), this.db.defer(()=>this[a]());
                }
            }
            for (const t of [
                u,
                l,
                f
            ])t.prototype[a] = function() {
                "open" === this.db.status && (this[h] = this[c]());
            }, t.prototype._next = function(t) {
                null !== this[h] ? this[h].next(t) : "opening" === this.db.status ? this.db.defer(()=>this._next(t)) : this.nextTick(t, new o("Iterator is not open: cannot call next() after close()", {
                    code: "LEVEL_ITERATOR_NOT_OPEN"
                }));
            }, t.prototype._nextv = function(t, e, i) {
                null !== this[h] ? this[h].nextv(t, e, i) : "opening" === this.db.status ? this.db.defer(()=>this._nextv(t, e, i)) : this.nextTick(i, new o("Iterator is not open: cannot call nextv() after close()", {
                    code: "LEVEL_ITERATOR_NOT_OPEN"
                }));
            }, t.prototype._all = function(t, e) {
                null !== this[h] ? this[h].all(e) : "opening" === this.db.status ? this.db.defer(()=>this._all(t, e)) : this.nextTick(e, new o("Iterator is not open: cannot call all() after close()", {
                    code: "LEVEL_ITERATOR_NOT_OPEN"
                }));
            }, t.prototype._seek = function(t, e) {
                null !== this[h] ? this[h]._seek(t, e) : "opening" === this.db.status && this.db.defer(()=>this._seek(t, e));
            }, t.prototype._close = function(t) {
                null !== this[h] ? this[h].close(t) : "opening" === this.db.status ? this.db.defer(()=>this._close(t)) : this.nextTick(t);
            };
            e.DeferredIterator = u, e.DeferredKeyIterator = l, e.DeferredValueIterator = f;
        },
        909: (t, e, i)=>{
            "use strict";
            const n = i(375);
            t.exports = function(t, ...e) {
                0 === e.length ? n(t) : n(()=>t(...e));
            };
        },
        56: (t, e, i)=>{
            "use strict";
            const n = i(473), r = Object.prototype.hasOwnProperty, s = new Set([
                "lt",
                "lte",
                "gt",
                "gte"
            ]);
            t.exports = function(t, e) {
                const i = {};
                for(const o in t)if (r.call(t, o) && "keyEncoding" !== o && "valueEncoding" !== o) {
                    if ("start" === o || "end" === o) throw new n(`The legacy range option '${o}' has been removed`, {
                        code: "LEVEL_LEGACY"
                    });
                    if ("encoding" === o) throw new n("The levelup-style 'encoding' alias has been removed, use 'valueEncoding' instead", {
                        code: "LEVEL_LEGACY"
                    });
                    s.has(o) ? i[o] = e.encode(t[o]) : i[o] = t[o];
                }
                return i.reverse = !!i.reverse, i.limit = Number.isInteger(i.limit) && i.limit >= 0 ? i.limit : -1, i;
            };
        },
        658: (t, e)=>{
            "use strict";
            e.supports = function(...t) {
                const e = t.reduce((t, e)=>Object.assign(t, e), {});
                return Object.assign(e, {
                    snapshots: e.snapshots || !1,
                    permanence: e.permanence || !1,
                    seek: e.seek || !1,
                    clear: e.clear || !1,
                    getMany: e.getMany || !1,
                    keyIterator: e.keyIterator || !1,
                    valueIterator: e.valueIterator || !1,
                    iteratorNextv: e.iteratorNextv || !1,
                    iteratorAll: e.iteratorAll || !1,
                    status: e.status || !1,
                    createIfMissing: e.createIfMissing || !1,
                    errorIfExists: e.errorIfExists || !1,
                    deferredOpen: e.deferredOpen || !1,
                    promises: e.promises || !1,
                    streams: e.streams || !1,
                    encodings: Object.assign({}, e.encodings),
                    events: Object.assign({}, e.events),
                    additionalMethods: Object.assign({}, e.additionalMethods)
                });
            };
        },
        742: (t, e)=>{
            "use strict";
            e.byteLength = function(t) {
                var e = h(t), i = e[0], n = e[1];
                return 3 * (i + n) / 4 - n;
            }, e.toByteArray = function(t) {
                var e, i, s = h(t), o = s[0], a = s[1], c = new r(function(t, e, i) {
                    return 3 * (e + i) / 4 - i;
                }(0, o, a)), u = 0, l = a > 0 ? o - 4 : o;
                for(i = 0; i < l; i += 4)e = n[t.charCodeAt(i)] << 18 | n[t.charCodeAt(i + 1)] << 12 | n[t.charCodeAt(i + 2)] << 6 | n[t.charCodeAt(i + 3)], c[u++] = e >> 16 & 255, c[u++] = e >> 8 & 255, c[u++] = 255 & e;
                return 2 === a && (e = n[t.charCodeAt(i)] << 2 | n[t.charCodeAt(i + 1)] >> 4, c[u++] = 255 & e), 1 === a && (e = n[t.charCodeAt(i)] << 10 | n[t.charCodeAt(i + 1)] << 4 | n[t.charCodeAt(i + 2)] >> 2, c[u++] = e >> 8 & 255, c[u++] = 255 & e), c;
            }, e.fromByteArray = function(t) {
                for(var e, n = t.length, r = n % 3, s = [], o = 16383, h = 0, c = n - r; h < c; h += o)s.push(a(t, h, h + o > c ? c : h + o));
                return 1 === r ? (e = t[n - 1], s.push(i[e >> 2] + i[e << 4 & 63] + "==")) : 2 === r && (e = (t[n - 2] << 8) + t[n - 1], s.push(i[e >> 10] + i[e >> 4 & 63] + i[e << 2 & 63] + "=")), s.join("");
            };
            for(var i = [], n = [], r = "undefined" != typeof Uint8Array ? Uint8Array : Array, s = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", o = 0; o < 64; ++o)i[o] = s[o], n[s.charCodeAt(o)] = o;
            function h(t) {
                var e = t.length;
                if (e % 4 > 0) throw new Error("Invalid string. Length must be a multiple of 4");
                var i = t.indexOf("=");
                return -1 === i && (i = e), [
                    i,
                    i === e ? 0 : 4 - i % 4
                ];
            }
            function a(t, e, n) {
                for(var r, s, o = [], h = e; h < n; h += 3)r = (t[h] << 16 & 16711680) + (t[h + 1] << 8 & 65280) + (255 & t[h + 2]), o.push(i[(s = r) >> 18 & 63] + i[s >> 12 & 63] + i[s >> 6 & 63] + i[63 & s]);
                return o.join("");
            }
            n["-".charCodeAt(0)] = 62, n["_".charCodeAt(0)] = 63;
        },
        708: (t, e, i)=>{
            "use strict";
            const { AbstractLevel: n } = i(875), r = i(473), s = i(967), { fromCallback: o } = i(957), { Iterator: h } = i(212), a = i(687), c = i(968), u = i(217), l = "level-js-", f = Symbol("idb"), d = Symbol("namePrefix"), p = Symbol("location"), y = Symbol("version"), E = Symbol("store"), g = Symbol("onComplete"), m = Symbol("promise");
            class v extends n {
                constructor(t, e, i){
                    if ("function" == typeof e || "function" == typeof i) throw new r("The levelup-style callback argument has been removed", {
                        code: "LEVEL_LEGACY"
                    });
                    const { prefix: n, version: s, ...o } = e || {};
                    if (super({
                        encodings: {
                            view: !0
                        },
                        snapshots: !1,
                        createIfMissing: !1,
                        errorIfExists: !1,
                        seek: !0
                    }, o), "string" != typeof t) throw new Error("constructor requires a location string argument");
                    this[p] = t, this[d] = null == n ? l : n, this[y] = parseInt(s || 1, 10), this[f] = null;
                }
                get location() {
                    return this[p];
                }
                get namePrefix() {
                    return this[d];
                }
                get version() {
                    return this[y];
                }
                get db() {
                    return this[f];
                }
                get type() {
                    return "browser-level";
                }
                _open(t, e) {
                    const i = indexedDB.open(this[d] + this[p], this[y]);
                    i.onerror = function() {
                        e(i.error || new Error("unknown error"));
                    }, i.onsuccess = ()=>{
                        this[f] = i.result, e();
                    }, i.onupgradeneeded = (t)=>{
                        const e = t.target.result;
                        e.objectStoreNames.contains(this[p]) || e.createObjectStore(this[p]);
                    };
                }
                [E](t) {
                    return this[f].transaction([
                        this[p]
                    ], t).objectStore(this[p]);
                }
                [g](t, e) {
                    const i = t.transaction;
                    i.onabort = function() {
                        e(i.error || new Error("aborted by user"));
                    }, i.oncomplete = function() {
                        e(null, t.result);
                    };
                }
                _get(t, e, i) {
                    const n = this[E]("readonly");
                    let s;
                    try {
                        s = n.get(t);
                    } catch (t) {
                        return this.nextTick(i, t);
                    }
                    this[g](s, function(t, e) {
                        return t ? i(t) : void 0 === e ? i(new r("Entry not found", {
                            code: "LEVEL_NOT_FOUND"
                        })) : void i(null, a(e));
                    });
                }
                _getMany(t, e, i) {
                    const n = this[E]("readonly"), r = t.map((t)=>(e)=>{
                            let i;
                            try {
                                i = n.get(t);
                            } catch (t) {
                                return e(t);
                            }
                            i.onsuccess = ()=>{
                                const t = i.result;
                                e(null, void 0 === t ? t : a(t));
                            }, i.onerror = (t)=>{
                                t.stopPropagation(), e(i.error);
                            };
                        });
                    s(r, 16, i);
                }
                _del(t, e, i) {
                    const n = this[E]("readwrite");
                    let r;
                    try {
                        r = n.delete(t);
                    } catch (t) {
                        return this.nextTick(i, t);
                    }
                    this[g](r, i);
                }
                _put(t, e, i, n) {
                    const r = this[E]("readwrite");
                    let s;
                    try {
                        s = r.put(e, t);
                    } catch (t) {
                        return this.nextTick(n, t);
                    }
                    this[g](s, n);
                }
                _iterator(t) {
                    return new h(this, this[p], t);
                }
                _batch(t, e, i) {
                    const n = this[E]("readwrite"), r = n.transaction;
                    let s, o = 0;
                    r.onabort = function() {
                        i(s || r.error || new Error("aborted by user"));
                    }, r.oncomplete = function() {
                        i();
                    }, function e() {
                        const i = t[o++], h = i.key;
                        let a;
                        try {
                            a = "del" === i.type ? n.delete(h) : n.put(i.value, h);
                        } catch (t) {
                            return s = t, void r.abort();
                        }
                        o < t.length ? a.onsuccess = e : "function" == typeof r.commit && r.commit();
                    }();
                }
                _clear(t, e) {
                    let i, n;
                    try {
                        i = u(t);
                    } catch (t) {
                        return this.nextTick(e);
                    }
                    if (t.limit >= 0) return c(this, this[p], i, t, e);
                    try {
                        const t = this[E]("readwrite");
                        n = i ? t.delete(i) : t.clear();
                    } catch (t) {
                        return this.nextTick(e, t);
                    }
                    this[g](n, e);
                }
                _close(t) {
                    this[f].close(), this.nextTick(t);
                }
            }
            v.destroy = function(t, e, i) {
                "function" == typeof e && (i = e, e = l), i = o(i, m);
                const n = indexedDB.deleteDatabase(e + t);
                return n.onsuccess = function() {
                    i();
                }, n.onerror = function(t) {
                    i(t);
                }, i[m];
            }, e.v = v;
        },
        212: (t, e, i)=>{
            "use strict";
            const { AbstractIterator: n } = i(875), r = i(217), s = i(687), o = Symbol("cache"), h = Symbol("finished"), a = Symbol("options"), c = Symbol("currentOptions"), u = Symbol("position"), l = Symbol("location"), f = Symbol("first"), d = {};
            function p(t) {
                "function" == typeof t.commit && t.commit();
            }
            e.Iterator = class extends n {
                constructor(t, e, i){
                    super(t, i), this[o] = [], this[h] = 0 === this.limit, this[a] = i, this[c] = {
                        ...i
                    }, this[u] = void 0, this[l] = e, this[f] = !0;
                }
                _nextv(t, e, i) {
                    if (this[f] = !1, this[h]) return this.nextTick(i, null, []);
                    if (this[o].length > 0) return t = Math.min(t, this[o].length), this.nextTick(i, null, this[o].splice(0, t));
                    let n;
                    void 0 !== this[u] && (this[a].reverse ? (this[c].lt = this[u], this[c].lte = void 0) : (this[c].gt = this[u], this[c].gte = void 0));
                    try {
                        n = r(this[c]);
                    } catch (t) {
                        return this[h] = !0, this.nextTick(i, null, []);
                    }
                    const d = this.db.db.transaction([
                        this[l]
                    ], "readonly"), y = d.objectStore(this[l]), E = [];
                    if (this[a].reverse) y[!this[a].values && y.openKeyCursor ? "openKeyCursor" : "openCursor"](n, "prev").onsuccess = (e)=>{
                        const i = e.target.result;
                        if (i) {
                            const { key: e, value: n } = i;
                            this[u] = e, E.push([
                                this[a].keys && void 0 !== e ? s(e) : void 0,
                                this[a].values && void 0 !== n ? s(n) : void 0
                            ]), E.length < t ? i.continue() : p(d);
                        } else this[h] = !0;
                    };
                    else {
                        let e, i;
                        const r = ()=>{
                            if (void 0 === e || void 0 === i) return;
                            const n = Math.max(e.length, i.length);
                            0 === n || t === 1 / 0 ? this[h] = !0 : this[u] = e[n - 1], E.length = n;
                            for(let t = 0; t < n; t++){
                                const n = e[t], r = i[t];
                                E[t] = [
                                    this[a].keys && void 0 !== n ? s(n) : void 0,
                                    this[a].values && void 0 !== r ? s(r) : void 0
                                ];
                            }
                            p(d);
                        };
                        this[a].keys || t < 1 / 0 ? y.getAllKeys(n, t < 1 / 0 ? t : void 0).onsuccess = (t)=>{
                            e = t.target.result, r();
                        } : (e = [], this.nextTick(r)), this[a].values ? y.getAll(n, t < 1 / 0 ? t : void 0).onsuccess = (t)=>{
                            i = t.target.result, r();
                        } : (i = [], this.nextTick(r));
                    }
                    d.onabort = ()=>{
                        i(d.error || new Error("aborted by user")), i = null;
                    }, d.oncomplete = ()=>{
                        i(null, E), i = null;
                    };
                }
                _next(t) {
                    if (this[o].length > 0) {
                        const [e, i] = this[o].shift();
                        this.nextTick(t, null, e, i);
                    } else if (this[h]) this.nextTick(t);
                    else {
                        let e = Math.min(100, this.limit - this.count);
                        this[f] && (this[f] = !1, e = 1), this._nextv(e, d, (e, i)=>{
                            if (e) return t(e);
                            this[o] = i, this._next(t);
                        });
                    }
                }
                _all(t, e) {
                    this[f] = !1;
                    const i = this[o].splice(0, this[o].length), n = this.limit - this.count - i.length;
                    if (n <= 0) return this.nextTick(e, null, i);
                    this._nextv(n, d, (t, n)=>{
                        if (t) return e(t);
                        i.length > 0 && (n = i.concat(n)), e(null, n);
                    });
                }
                _seek(t, e) {
                    let i;
                    this[f] = !0, this[o] = [], this[h] = !1, this[u] = void 0, this[c] = {
                        ...this[a]
                    };
                    try {
                        i = r(this[a]);
                    } catch (t) {
                        return void (this[h] = !0);
                    }
                    null === i || i.includes(t) ? this[a].reverse ? this[c].lte = t : this[c].gte = t : this[h] = !0;
                }
            };
        },
        968: (t)=>{
            "use strict";
            t.exports = function(t, e, i, n, r) {
                if (0 === n.limit) return t.nextTick(r);
                const s = t.db.transaction([
                    e
                ], "readwrite"), o = s.objectStore(e);
                let h = 0;
                s.oncomplete = function() {
                    r();
                }, s.onabort = function() {
                    r(s.error || new Error("aborted by user"));
                };
                const a = o.openKeyCursor ? "openKeyCursor" : "openCursor", c = n.reverse ? "prev" : "next";
                o[a](i, c).onsuccess = function(t) {
                    const e = t.target.result;
                    e && (o.delete(e.key).onsuccess = function() {
                        (n.limit <= 0 || ++h < n.limit) && e.continue();
                    });
                };
            };
        },
        687: (t)=>{
            "use strict";
            const e = new TextEncoder;
            t.exports = function(t) {
                return t instanceof Uint8Array ? t : t instanceof ArrayBuffer ? new Uint8Array(t) : e.encode(t);
            };
        },
        217: (t)=>{
            "use strict";
            t.exports = function(t) {
                const e = void 0 !== t.gte ? t.gte : void 0 !== t.gt ? t.gt : void 0, i = void 0 !== t.lte ? t.lte : void 0 !== t.lt ? t.lt : void 0, n = void 0 === t.gte, r = void 0 === t.lte;
                return void 0 !== e && void 0 !== i ? IDBKeyRange.bound(e, i, n, r) : void 0 !== e ? IDBKeyRange.lowerBound(e, n) : void 0 !== i ? IDBKeyRange.upperBound(i, r) : null;
            };
        },
        764: (t, e, i)=>{
            "use strict";
            const n = i(742), r = i(645), s = "function" == typeof Symbol && "function" == typeof Symbol.for ? Symbol.for("nodejs.util.inspect.custom") : null;
            e.Buffer = a, e.SlowBuffer = function(t) {
                return +t != t && (t = 0), a.alloc(+t);
            }, e.INSPECT_MAX_BYTES = 50;
            const o = 2147483647;
            function h(t) {
                if (t > o) throw new RangeError('The value "' + t + '" is invalid for option "size"');
                const e = new Uint8Array(t);
                return Object.setPrototypeOf(e, a.prototype), e;
            }
            function a(t, e, i) {
                if ("number" == typeof t) {
                    if ("string" == typeof e) throw new TypeError('The "string" argument must be of type string. Received type number');
                    return l(t);
                }
                return c(t, e, i);
            }
            function c(t, e, i) {
                if ("string" == typeof t) return function(t, e) {
                    if ("string" == typeof e && "" !== e || (e = "utf8"), !a.isEncoding(e)) throw new TypeError("Unknown encoding: " + e);
                    const i = 0 | y(t, e);
                    let n = h(i);
                    const r = n.write(t, e);
                    return r !== i && (n = n.slice(0, r)), n;
                }(t, e);
                if (ArrayBuffer.isView(t)) return function(t) {
                    if (H(t, Uint8Array)) {
                        const e = new Uint8Array(t);
                        return d(e.buffer, e.byteOffset, e.byteLength);
                    }
                    return f(t);
                }(t);
                if (null == t) throw new TypeError("The first argument must be one of type string, Buffer, ArrayBuffer, Array, or Array-like Object. Received type " + typeof t);
                if (H(t, ArrayBuffer) || t && H(t.buffer, ArrayBuffer)) return d(t, e, i);
                if ("undefined" != typeof SharedArrayBuffer && (H(t, SharedArrayBuffer) || t && H(t.buffer, SharedArrayBuffer))) return d(t, e, i);
                if ("number" == typeof t) throw new TypeError('The "value" argument must not be of type number. Received type number');
                const n = t.valueOf && t.valueOf();
                if (null != n && n !== t) return a.from(n, e, i);
                const r = function(t) {
                    if (a.isBuffer(t)) {
                        const e = 0 | p(t.length), i = h(e);
                        return 0 === i.length || t.copy(i, 0, 0, e), i;
                    }
                    return void 0 !== t.length ? "number" != typeof t.length || J(t.length) ? h(0) : f(t) : "Buffer" === t.type && Array.isArray(t.data) ? f(t.data) : void 0;
                }(t);
                if (r) return r;
                if ("undefined" != typeof Symbol && null != Symbol.toPrimitive && "function" == typeof t[Symbol.toPrimitive]) return a.from(t[Symbol.toPrimitive]("string"), e, i);
                throw new TypeError("The first argument must be one of type string, Buffer, ArrayBuffer, Array, or Array-like Object. Received type " + typeof t);
            }
            function u(t) {
                if ("number" != typeof t) throw new TypeError('"size" argument must be of type number');
                if (t < 0) throw new RangeError('The value "' + t + '" is invalid for option "size"');
            }
            function l(t) {
                return u(t), h(t < 0 ? 0 : 0 | p(t));
            }
            function f(t) {
                const e = t.length < 0 ? 0 : 0 | p(t.length), i = h(e);
                for(let n = 0; n < e; n += 1)i[n] = 255 & t[n];
                return i;
            }
            function d(t, e, i) {
                if (e < 0 || t.byteLength < e) throw new RangeError('"offset" is outside of buffer bounds');
                if (t.byteLength < e + (i || 0)) throw new RangeError('"length" is outside of buffer bounds');
                let n;
                return n = void 0 === e && void 0 === i ? new Uint8Array(t) : void 0 === i ? new Uint8Array(t, e) : new Uint8Array(t, e, i), Object.setPrototypeOf(n, a.prototype), n;
            }
            function p(t) {
                if (t >= o) throw new RangeError("Attempt to allocate Buffer larger than maximum size: 0x" + o.toString(16) + " bytes");
                return 0 | t;
            }
            function y(t, e) {
                if (a.isBuffer(t)) return t.length;
                if (ArrayBuffer.isView(t) || H(t, ArrayBuffer)) return t.byteLength;
                if ("string" != typeof t) throw new TypeError('The "string" argument must be one of type string, Buffer, or ArrayBuffer. Received type ' + typeof t);
                const i = t.length, n = arguments.length > 2 && !0 === arguments[2];
                if (!n && 0 === i) return 0;
                let r = !1;
                for(;;)switch(e){
                    case "ascii":
                    case "latin1":
                    case "binary":
                        return i;
                    case "utf8":
                    case "utf-8":
                        return $(t).length;
                    case "ucs2":
                    case "ucs-2":
                    case "utf16le":
                    case "utf-16le":
                        return 2 * i;
                    case "hex":
                        return i >>> 1;
                    case "base64":
                        return Y(t).length;
                    default:
                        if (r) return n ? -1 : $(t).length;
                        e = ("" + e).toLowerCase(), r = !0;
                }
            }
            function E(t, e, i) {
                let n = !1;
                if ((void 0 === e || e < 0) && (e = 0), e > this.length) return "";
                if ((void 0 === i || i > this.length) && (i = this.length), i <= 0) return "";
                if ((i >>>= 0) <= (e >>>= 0)) return "";
                for(t || (t = "utf8");;)switch(t){
                    case "hex":
                        return k(this, e, i);
                    case "utf8":
                    case "utf-8":
                        return A(this, e, i);
                    case "ascii":
                        return I(this, e, i);
                    case "latin1":
                    case "binary":
                        return x(this, e, i);
                    case "base64":
                        return L(this, e, i);
                    case "ucs2":
                    case "ucs-2":
                    case "utf16le":
                    case "utf-16le":
                        return C(this, e, i);
                    default:
                        if (n) throw new TypeError("Unknown encoding: " + t);
                        t = (t + "").toLowerCase(), n = !0;
                }
            }
            function g(t, e, i) {
                const n = t[e];
                t[e] = t[i], t[i] = n;
            }
            function m(t, e, i, n, r) {
                if (0 === t.length) return -1;
                if ("string" == typeof i ? (n = i, i = 0) : i > 2147483647 ? i = 2147483647 : i < -2147483648 && (i = -2147483648), J(i = +i) && (i = r ? 0 : t.length - 1), i < 0 && (i = t.length + i), i >= t.length) {
                    if (r) return -1;
                    i = t.length - 1;
                } else if (i < 0) {
                    if (!r) return -1;
                    i = 0;
                }
                if ("string" == typeof e && (e = a.from(e, n)), a.isBuffer(e)) return 0 === e.length ? -1 : v(t, e, i, n, r);
                if ("number" == typeof e) return e &= 255, "function" == typeof Uint8Array.prototype.indexOf ? r ? Uint8Array.prototype.indexOf.call(t, e, i) : Uint8Array.prototype.lastIndexOf.call(t, e, i) : v(t, [
                    e
                ], i, n, r);
                throw new TypeError("val must be string, number or Buffer");
            }
            function v(t, e, i, n, r) {
                let s, o = 1, h = t.length, a = e.length;
                if (void 0 !== n && ("ucs2" === (n = String(n).toLowerCase()) || "ucs-2" === n || "utf16le" === n || "utf-16le" === n)) {
                    if (t.length < 2 || e.length < 2) return -1;
                    o = 2, h /= 2, a /= 2, i /= 2;
                }
                function c(t, e) {
                    return 1 === o ? t[e] : t.readUInt16BE(e * o);
                }
                if (r) {
                    let n = -1;
                    for(s = i; s < h; s++)if (c(t, s) === c(e, -1 === n ? 0 : s - n)) {
                        if (-1 === n && (n = s), s - n + 1 === a) return n * o;
                    } else -1 !== n && (s -= s - n), n = -1;
                } else for(i + a > h && (i = h - a), s = i; s >= 0; s--){
                    let i = !0;
                    for(let n = 0; n < a; n++)if (c(t, s + n) !== c(e, n)) {
                        i = !1;
                        break;
                    }
                    if (i) return s;
                }
                return -1;
            }
            function b(t, e, i, n) {
                i = Number(i) || 0;
                const r = t.length - i;
                n ? (n = Number(n)) > r && (n = r) : n = r;
                const s = e.length;
                let o;
                for(n > s / 2 && (n = s / 2), o = 0; o < n; ++o){
                    const n = parseInt(e.substr(2 * o, 2), 16);
                    if (J(n)) return o;
                    t[i + o] = n;
                }
                return o;
            }
            function T(t, e, i, n) {
                return X($(e, t.length - i), t, i, n);
            }
            function w(t, e, i, n) {
                return X(function(t) {
                    const e = [];
                    for(let i = 0; i < t.length; ++i)e.push(255 & t.charCodeAt(i));
                    return e;
                }(e), t, i, n);
            }
            function _(t, e, i, n) {
                return X(Y(e), t, i, n);
            }
            function S(t, e, i, n) {
                return X(function(t, e) {
                    let i, n, r;
                    const s = [];
                    for(let o = 0; o < t.length && !((e -= 2) < 0); ++o)i = t.charCodeAt(o), n = i >> 8, r = i % 256, s.push(r), s.push(n);
                    return s;
                }(e, t.length - i), t, i, n);
            }
            function L(t, e, i) {
                return 0 === e && i === t.length ? n.fromByteArray(t) : n.fromByteArray(t.slice(e, i));
            }
            function A(t, e, i) {
                i = Math.min(t.length, i);
                const n = [];
                let r = e;
                for(; r < i;){
                    const e = t[r];
                    let s = null, o = e > 239 ? 4 : e > 223 ? 3 : e > 191 ? 2 : 1;
                    if (r + o <= i) {
                        let i, n, h, a;
                        switch(o){
                            case 1:
                                e < 128 && (s = e);
                                break;
                            case 2:
                                i = t[r + 1], 128 == (192 & i) && (a = (31 & e) << 6 | 63 & i, a > 127 && (s = a));
                                break;
                            case 3:
                                i = t[r + 1], n = t[r + 2], 128 == (192 & i) && 128 == (192 & n) && (a = (15 & e) << 12 | (63 & i) << 6 | 63 & n, a > 2047 && (a < 55296 || a > 57343) && (s = a));
                                break;
                            case 4:
                                i = t[r + 1], n = t[r + 2], h = t[r + 3], 128 == (192 & i) && 128 == (192 & n) && 128 == (192 & h) && (a = (15 & e) << 18 | (63 & i) << 12 | (63 & n) << 6 | 63 & h, a > 65535 && a < 1114112 && (s = a));
                        }
                    }
                    null === s ? (s = 65533, o = 1) : s > 65535 && (s -= 65536, n.push(s >>> 10 & 1023 | 55296), s = 56320 | 1023 & s), n.push(s), r += o;
                }
                return function(t) {
                    const e = t.length;
                    if (e <= O) return String.fromCharCode.apply(String, t);
                    let i = "", n = 0;
                    for(; n < e;)i += String.fromCharCode.apply(String, t.slice(n, n += O));
                    return i;
                }(n);
            }
            e.kMaxLength = o, a.TYPED_ARRAY_SUPPORT = function() {
                try {
                    const t = new Uint8Array(1), e = {
                        foo: function() {
                            return 42;
                        }
                    };
                    return Object.setPrototypeOf(e, Uint8Array.prototype), Object.setPrototypeOf(t, e), 42 === t.foo();
                } catch (t) {
                    return !1;
                }
            }(), a.TYPED_ARRAY_SUPPORT || "undefined" == typeof console || "function" != typeof console.error || console.error("This browser lacks typed array (Uint8Array) support which is required by `buffer` v5.x. Use `buffer` v4.x if you require old browser support."), Object.defineProperty(a.prototype, "parent", {
                enumerable: !0,
                get: function() {
                    if (a.isBuffer(this)) return this.buffer;
                }
            }), Object.defineProperty(a.prototype, "offset", {
                enumerable: !0,
                get: function() {
                    if (a.isBuffer(this)) return this.byteOffset;
                }
            }), a.poolSize = 8192, a.from = function(t, e, i) {
                return c(t, e, i);
            }, Object.setPrototypeOf(a.prototype, Uint8Array.prototype), Object.setPrototypeOf(a, Uint8Array), a.alloc = function(t, e, i) {
                return function(t, e, i) {
                    return u(t), t <= 0 ? h(t) : void 0 !== e ? "string" == typeof i ? h(t).fill(e, i) : h(t).fill(e) : h(t);
                }(t, e, i);
            }, a.allocUnsafe = function(t) {
                return l(t);
            }, a.allocUnsafeSlow = function(t) {
                return l(t);
            }, a.isBuffer = function(t) {
                return null != t && !0 === t._isBuffer && t !== a.prototype;
            }, a.compare = function(t, e) {
                if (H(t, Uint8Array) && (t = a.from(t, t.offset, t.byteLength)), H(e, Uint8Array) && (e = a.from(e, e.offset, e.byteLength)), !a.isBuffer(t) || !a.isBuffer(e)) throw new TypeError('The "buf1", "buf2" arguments must be one of type Buffer or Uint8Array');
                if (t === e) return 0;
                let i = t.length, n = e.length;
                for(let r = 0, s = Math.min(i, n); r < s; ++r)if (t[r] !== e[r]) {
                    i = t[r], n = e[r];
                    break;
                }
                return i < n ? -1 : n < i ? 1 : 0;
            }, a.isEncoding = function(t) {
                switch(String(t).toLowerCase()){
                    case "hex":
                    case "utf8":
                    case "utf-8":
                    case "ascii":
                    case "latin1":
                    case "binary":
                    case "base64":
                    case "ucs2":
                    case "ucs-2":
                    case "utf16le":
                    case "utf-16le":
                        return !0;
                    default:
                        return !1;
                }
            }, a.concat = function(t, e) {
                if (!Array.isArray(t)) throw new TypeError('"list" argument must be an Array of Buffers');
                if (0 === t.length) return a.alloc(0);
                let i;
                if (void 0 === e) for(e = 0, i = 0; i < t.length; ++i)e += t[i].length;
                const n = a.allocUnsafe(e);
                let r = 0;
                for(i = 0; i < t.length; ++i){
                    let e = t[i];
                    if (H(e, Uint8Array)) r + e.length > n.length ? (a.isBuffer(e) || (e = a.from(e)), e.copy(n, r)) : Uint8Array.prototype.set.call(n, e, r);
                    else {
                        if (!a.isBuffer(e)) throw new TypeError('"list" argument must be an Array of Buffers');
                        e.copy(n, r);
                    }
                    r += e.length;
                }
                return n;
            }, a.byteLength = y, a.prototype._isBuffer = !0, a.prototype.swap16 = function() {
                const t = this.length;
                if (t % 2 != 0) throw new RangeError("Buffer size must be a multiple of 16-bits");
                for(let e = 0; e < t; e += 2)g(this, e, e + 1);
                return this;
            }, a.prototype.swap32 = function() {
                const t = this.length;
                if (t % 4 != 0) throw new RangeError("Buffer size must be a multiple of 32-bits");
                for(let e = 0; e < t; e += 4)g(this, e, e + 3), g(this, e + 1, e + 2);
                return this;
            }, a.prototype.swap64 = function() {
                const t = this.length;
                if (t % 8 != 0) throw new RangeError("Buffer size must be a multiple of 64-bits");
                for(let e = 0; e < t; e += 8)g(this, e, e + 7), g(this, e + 1, e + 6), g(this, e + 2, e + 5), g(this, e + 3, e + 4);
                return this;
            }, a.prototype.toString = function() {
                const t = this.length;
                return 0 === t ? "" : 0 === arguments.length ? A(this, 0, t) : E.apply(this, arguments);
            }, a.prototype.toLocaleString = a.prototype.toString, a.prototype.equals = function(t) {
                if (!a.isBuffer(t)) throw new TypeError("Argument must be a Buffer");
                return this === t || 0 === a.compare(this, t);
            }, a.prototype.inspect = function() {
                let t = "";
                const i = e.INSPECT_MAX_BYTES;
                return t = this.toString("hex", 0, i).replace(/(.{2})/g, "$1 ").trim(), this.length > i && (t += " ... "), "<Buffer " + t + ">";
            }, s && (a.prototype[s] = a.prototype.inspect), a.prototype.compare = function(t, e, i, n, r) {
                if (H(t, Uint8Array) && (t = a.from(t, t.offset, t.byteLength)), !a.isBuffer(t)) throw new TypeError('The "target" argument must be one of type Buffer or Uint8Array. Received type ' + typeof t);
                if (void 0 === e && (e = 0), void 0 === i && (i = t ? t.length : 0), void 0 === n && (n = 0), void 0 === r && (r = this.length), e < 0 || i > t.length || n < 0 || r > this.length) throw new RangeError("out of range index");
                if (n >= r && e >= i) return 0;
                if (n >= r) return -1;
                if (e >= i) return 1;
                if (this === t) return 0;
                let s = (r >>>= 0) - (n >>>= 0), o = (i >>>= 0) - (e >>>= 0);
                const h = Math.min(s, o), c = this.slice(n, r), u = t.slice(e, i);
                for(let t = 0; t < h; ++t)if (c[t] !== u[t]) {
                    s = c[t], o = u[t];
                    break;
                }
                return s < o ? -1 : o < s ? 1 : 0;
            }, a.prototype.includes = function(t, e, i) {
                return -1 !== this.indexOf(t, e, i);
            }, a.prototype.indexOf = function(t, e, i) {
                return m(this, t, e, i, !0);
            }, a.prototype.lastIndexOf = function(t, e, i) {
                return m(this, t, e, i, !1);
            }, a.prototype.write = function(t, e, i, n) {
                if (void 0 === e) n = "utf8", i = this.length, e = 0;
                else if (void 0 === i && "string" == typeof e) n = e, i = this.length, e = 0;
                else {
                    if (!isFinite(e)) throw new Error("Buffer.write(string, encoding, offset[, length]) is no longer supported");
                    e >>>= 0, isFinite(i) ? (i >>>= 0, void 0 === n && (n = "utf8")) : (n = i, i = void 0);
                }
                const r = this.length - e;
                if ((void 0 === i || i > r) && (i = r), t.length > 0 && (i < 0 || e < 0) || e > this.length) throw new RangeError("Attempt to write outside buffer bounds");
                n || (n = "utf8");
                let s = !1;
                for(;;)switch(n){
                    case "hex":
                        return b(this, t, e, i);
                    case "utf8":
                    case "utf-8":
                        return T(this, t, e, i);
                    case "ascii":
                    case "latin1":
                    case "binary":
                        return w(this, t, e, i);
                    case "base64":
                        return _(this, t, e, i);
                    case "ucs2":
                    case "ucs-2":
                    case "utf16le":
                    case "utf-16le":
                        return S(this, t, e, i);
                    default:
                        if (s) throw new TypeError("Unknown encoding: " + n);
                        n = ("" + n).toLowerCase(), s = !0;
                }
            }, a.prototype.toJSON = function() {
                return {
                    type: "Buffer",
                    data: Array.prototype.slice.call(this._arr || this, 0)
                };
            };
            const O = 4096;
            function I(t, e, i) {
                let n = "";
                i = Math.min(t.length, i);
                for(let r = e; r < i; ++r)n += String.fromCharCode(127 & t[r]);
                return n;
            }
            function x(t, e, i) {
                let n = "";
                i = Math.min(t.length, i);
                for(let r = e; r < i; ++r)n += String.fromCharCode(t[r]);
                return n;
            }
            function k(t, e, i) {
                const n = t.length;
                (!e || e < 0) && (e = 0), (!i || i < 0 || i > n) && (i = n);
                let r = "";
                for(let n = e; n < i; ++n)r += q[t[n]];
                return r;
            }
            function C(t, e, i) {
                const n = t.slice(e, i);
                let r = "";
                for(let t = 0; t < n.length - 1; t += 2)r += String.fromCharCode(n[t] + 256 * n[t + 1]);
                return r;
            }
            function D(t, e, i) {
                if (t % 1 != 0 || t < 0) throw new RangeError("offset is not uint");
                if (t + e > i) throw new RangeError("Trying to access beyond buffer length");
            }
            function R(t, e, i, n, r, s) {
                if (!a.isBuffer(t)) throw new TypeError('"buffer" argument must be a Buffer instance');
                if (e > r || e < s) throw new RangeError('"value" argument is out of bounds');
                if (i + n > t.length) throw new RangeError("Index out of range");
            }
            function N(t, e, i, n, r) {
                z(e, n, r, t, i, 7);
                let s = Number(e & BigInt(4294967295));
                t[i++] = s, s >>= 8, t[i++] = s, s >>= 8, t[i++] = s, s >>= 8, t[i++] = s;
                let o = Number(e >> BigInt(32) & BigInt(4294967295));
                return t[i++] = o, o >>= 8, t[i++] = o, o >>= 8, t[i++] = o, o >>= 8, t[i++] = o, i;
            }
            function U(t, e, i, n, r) {
                z(e, n, r, t, i, 7);
                let s = Number(e & BigInt(4294967295));
                t[i + 7] = s, s >>= 8, t[i + 6] = s, s >>= 8, t[i + 5] = s, s >>= 8, t[i + 4] = s;
                let o = Number(e >> BigInt(32) & BigInt(4294967295));
                return t[i + 3] = o, o >>= 8, t[i + 2] = o, o >>= 8, t[i + 1] = o, o >>= 8, t[i] = o, i + 8;
            }
            function B(t, e, i, n, r, s) {
                if (i + n > t.length) throw new RangeError("Index out of range");
                if (i < 0) throw new RangeError("Index out of range");
            }
            function F(t, e, i, n, s) {
                return e = +e, i >>>= 0, s || B(t, 0, i, 4), r.write(t, e, i, n, 23, 4), i + 4;
            }
            function P(t, e, i, n, s) {
                return e = +e, i >>>= 0, s || B(t, 0, i, 8), r.write(t, e, i, n, 52, 8), i + 8;
            }
            a.prototype.slice = function(t, e) {
                const i = this.length;
                (t = ~~t) < 0 ? (t += i) < 0 && (t = 0) : t > i && (t = i), (e = void 0 === e ? i : ~~e) < 0 ? (e += i) < 0 && (e = 0) : e > i && (e = i), e < t && (e = t);
                const n = this.subarray(t, e);
                return Object.setPrototypeOf(n, a.prototype), n;
            }, a.prototype.readUintLE = a.prototype.readUIntLE = function(t, e, i) {
                t >>>= 0, e >>>= 0, i || D(t, e, this.length);
                let n = this[t], r = 1, s = 0;
                for(; ++s < e && (r *= 256);)n += this[t + s] * r;
                return n;
            }, a.prototype.readUintBE = a.prototype.readUIntBE = function(t, e, i) {
                t >>>= 0, e >>>= 0, i || D(t, e, this.length);
                let n = this[t + --e], r = 1;
                for(; e > 0 && (r *= 256);)n += this[t + --e] * r;
                return n;
            }, a.prototype.readUint8 = a.prototype.readUInt8 = function(t, e) {
                return t >>>= 0, e || D(t, 1, this.length), this[t];
            }, a.prototype.readUint16LE = a.prototype.readUInt16LE = function(t, e) {
                return t >>>= 0, e || D(t, 2, this.length), this[t] | this[t + 1] << 8;
            }, a.prototype.readUint16BE = a.prototype.readUInt16BE = function(t, e) {
                return t >>>= 0, e || D(t, 2, this.length), this[t] << 8 | this[t + 1];
            }, a.prototype.readUint32LE = a.prototype.readUInt32LE = function(t, e) {
                return t >>>= 0, e || D(t, 4, this.length), (this[t] | this[t + 1] << 8 | this[t + 2] << 16) + 16777216 * this[t + 3];
            }, a.prototype.readUint32BE = a.prototype.readUInt32BE = function(t, e) {
                return t >>>= 0, e || D(t, 4, this.length), 16777216 * this[t] + (this[t + 1] << 16 | this[t + 2] << 8 | this[t + 3]);
            }, a.prototype.readBigUInt64LE = Q(function(t) {
                G(t >>>= 0, "offset");
                const e = this[t], i = this[t + 7];
                void 0 !== e && void 0 !== i || K(t, this.length - 8);
                const n = e + 256 * this[++t] + 65536 * this[++t] + this[++t] * 2 ** 24, r = this[++t] + 256 * this[++t] + 65536 * this[++t] + i * 2 ** 24;
                return BigInt(n) + (BigInt(r) << BigInt(32));
            }), a.prototype.readBigUInt64BE = Q(function(t) {
                G(t >>>= 0, "offset");
                const e = this[t], i = this[t + 7];
                void 0 !== e && void 0 !== i || K(t, this.length - 8);
                const n = e * 2 ** 24 + 65536 * this[++t] + 256 * this[++t] + this[++t], r = this[++t] * 2 ** 24 + 65536 * this[++t] + 256 * this[++t] + i;
                return (BigInt(n) << BigInt(32)) + BigInt(r);
            }), a.prototype.readIntLE = function(t, e, i) {
                t >>>= 0, e >>>= 0, i || D(t, e, this.length);
                let n = this[t], r = 1, s = 0;
                for(; ++s < e && (r *= 256);)n += this[t + s] * r;
                return r *= 128, n >= r && (n -= Math.pow(2, 8 * e)), n;
            }, a.prototype.readIntBE = function(t, e, i) {
                t >>>= 0, e >>>= 0, i || D(t, e, this.length);
                let n = e, r = 1, s = this[t + --n];
                for(; n > 0 && (r *= 256);)s += this[t + --n] * r;
                return r *= 128, s >= r && (s -= Math.pow(2, 8 * e)), s;
            }, a.prototype.readInt8 = function(t, e) {
                return t >>>= 0, e || D(t, 1, this.length), 128 & this[t] ? -1 * (255 - this[t] + 1) : this[t];
            }, a.prototype.readInt16LE = function(t, e) {
                t >>>= 0, e || D(t, 2, this.length);
                const i = this[t] | this[t + 1] << 8;
                return 32768 & i ? 4294901760 | i : i;
            }, a.prototype.readInt16BE = function(t, e) {
                t >>>= 0, e || D(t, 2, this.length);
                const i = this[t + 1] | this[t] << 8;
                return 32768 & i ? 4294901760 | i : i;
            }, a.prototype.readInt32LE = function(t, e) {
                return t >>>= 0, e || D(t, 4, this.length), this[t] | this[t + 1] << 8 | this[t + 2] << 16 | this[t + 3] << 24;
            }, a.prototype.readInt32BE = function(t, e) {
                return t >>>= 0, e || D(t, 4, this.length), this[t] << 24 | this[t + 1] << 16 | this[t + 2] << 8 | this[t + 3];
            }, a.prototype.readBigInt64LE = Q(function(t) {
                G(t >>>= 0, "offset");
                const e = this[t], i = this[t + 7];
                void 0 !== e && void 0 !== i || K(t, this.length - 8);
                const n = this[t + 4] + 256 * this[t + 5] + 65536 * this[t + 6] + (i << 24);
                return (BigInt(n) << BigInt(32)) + BigInt(e + 256 * this[++t] + 65536 * this[++t] + this[++t] * 2 ** 24);
            }), a.prototype.readBigInt64BE = Q(function(t) {
                G(t >>>= 0, "offset");
                const e = this[t], i = this[t + 7];
                void 0 !== e && void 0 !== i || K(t, this.length - 8);
                const n = (e << 24) + 65536 * this[++t] + 256 * this[++t] + this[++t];
                return (BigInt(n) << BigInt(32)) + BigInt(this[++t] * 2 ** 24 + 65536 * this[++t] + 256 * this[++t] + i);
            }), a.prototype.readFloatLE = function(t, e) {
                return t >>>= 0, e || D(t, 4, this.length), r.read(this, t, !0, 23, 4);
            }, a.prototype.readFloatBE = function(t, e) {
                return t >>>= 0, e || D(t, 4, this.length), r.read(this, t, !1, 23, 4);
            }, a.prototype.readDoubleLE = function(t, e) {
                return t >>>= 0, e || D(t, 8, this.length), r.read(this, t, !0, 52, 8);
            }, a.prototype.readDoubleBE = function(t, e) {
                return t >>>= 0, e || D(t, 8, this.length), r.read(this, t, !1, 52, 8);
            }, a.prototype.writeUintLE = a.prototype.writeUIntLE = function(t, e, i, n) {
                t = +t, e >>>= 0, i >>>= 0, n || R(this, t, e, i, Math.pow(2, 8 * i) - 1, 0);
                let r = 1, s = 0;
                for(this[e] = 255 & t; ++s < i && (r *= 256);)this[e + s] = t / r & 255;
                return e + i;
            }, a.prototype.writeUintBE = a.prototype.writeUIntBE = function(t, e, i, n) {
                t = +t, e >>>= 0, i >>>= 0, n || R(this, t, e, i, Math.pow(2, 8 * i) - 1, 0);
                let r = i - 1, s = 1;
                for(this[e + r] = 255 & t; --r >= 0 && (s *= 256);)this[e + r] = t / s & 255;
                return e + i;
            }, a.prototype.writeUint8 = a.prototype.writeUInt8 = function(t, e, i) {
                return t = +t, e >>>= 0, i || R(this, t, e, 1, 255, 0), this[e] = 255 & t, e + 1;
            }, a.prototype.writeUint16LE = a.prototype.writeUInt16LE = function(t, e, i) {
                return t = +t, e >>>= 0, i || R(this, t, e, 2, 65535, 0), this[e] = 255 & t, this[e + 1] = t >>> 8, e + 2;
            }, a.prototype.writeUint16BE = a.prototype.writeUInt16BE = function(t, e, i) {
                return t = +t, e >>>= 0, i || R(this, t, e, 2, 65535, 0), this[e] = t >>> 8, this[e + 1] = 255 & t, e + 2;
            }, a.prototype.writeUint32LE = a.prototype.writeUInt32LE = function(t, e, i) {
                return t = +t, e >>>= 0, i || R(this, t, e, 4, 4294967295, 0), this[e + 3] = t >>> 24, this[e + 2] = t >>> 16, this[e + 1] = t >>> 8, this[e] = 255 & t, e + 4;
            }, a.prototype.writeUint32BE = a.prototype.writeUInt32BE = function(t, e, i) {
                return t = +t, e >>>= 0, i || R(this, t, e, 4, 4294967295, 0), this[e] = t >>> 24, this[e + 1] = t >>> 16, this[e + 2] = t >>> 8, this[e + 3] = 255 & t, e + 4;
            }, a.prototype.writeBigUInt64LE = Q(function(t, e = 0) {
                return N(this, t, e, BigInt(0), BigInt("0xffffffffffffffff"));
            }), a.prototype.writeBigUInt64BE = Q(function(t, e = 0) {
                return U(this, t, e, BigInt(0), BigInt("0xffffffffffffffff"));
            }), a.prototype.writeIntLE = function(t, e, i, n) {
                if (t = +t, e >>>= 0, !n) {
                    const n = Math.pow(2, 8 * i - 1);
                    R(this, t, e, i, n - 1, -n);
                }
                let r = 0, s = 1, o = 0;
                for(this[e] = 255 & t; ++r < i && (s *= 256);)t < 0 && 0 === o && 0 !== this[e + r - 1] && (o = 1), this[e + r] = (t / s >> 0) - o & 255;
                return e + i;
            }, a.prototype.writeIntBE = function(t, e, i, n) {
                if (t = +t, e >>>= 0, !n) {
                    const n = Math.pow(2, 8 * i - 1);
                    R(this, t, e, i, n - 1, -n);
                }
                let r = i - 1, s = 1, o = 0;
                for(this[e + r] = 255 & t; --r >= 0 && (s *= 256);)t < 0 && 0 === o && 0 !== this[e + r + 1] && (o = 1), this[e + r] = (t / s >> 0) - o & 255;
                return e + i;
            }, a.prototype.writeInt8 = function(t, e, i) {
                return t = +t, e >>>= 0, i || R(this, t, e, 1, 127, -128), t < 0 && (t = 255 + t + 1), this[e] = 255 & t, e + 1;
            }, a.prototype.writeInt16LE = function(t, e, i) {
                return t = +t, e >>>= 0, i || R(this, t, e, 2, 32767, -32768), this[e] = 255 & t, this[e + 1] = t >>> 8, e + 2;
            }, a.prototype.writeInt16BE = function(t, e, i) {
                return t = +t, e >>>= 0, i || R(this, t, e, 2, 32767, -32768), this[e] = t >>> 8, this[e + 1] = 255 & t, e + 2;
            }, a.prototype.writeInt32LE = function(t, e, i) {
                return t = +t, e >>>= 0, i || R(this, t, e, 4, 2147483647, -2147483648), this[e] = 255 & t, this[e + 1] = t >>> 8, this[e + 2] = t >>> 16, this[e + 3] = t >>> 24, e + 4;
            }, a.prototype.writeInt32BE = function(t, e, i) {
                return t = +t, e >>>= 0, i || R(this, t, e, 4, 2147483647, -2147483648), t < 0 && (t = 4294967295 + t + 1), this[e] = t >>> 24, this[e + 1] = t >>> 16, this[e + 2] = t >>> 8, this[e + 3] = 255 & t, e + 4;
            }, a.prototype.writeBigInt64LE = Q(function(t, e = 0) {
                return N(this, t, e, -BigInt("0x8000000000000000"), BigInt("0x7fffffffffffffff"));
            }), a.prototype.writeBigInt64BE = Q(function(t, e = 0) {
                return U(this, t, e, -BigInt("0x8000000000000000"), BigInt("0x7fffffffffffffff"));
            }), a.prototype.writeFloatLE = function(t, e, i) {
                return F(this, t, e, !0, i);
            }, a.prototype.writeFloatBE = function(t, e, i) {
                return F(this, t, e, !1, i);
            }, a.prototype.writeDoubleLE = function(t, e, i) {
                return P(this, t, e, !0, i);
            }, a.prototype.writeDoubleBE = function(t, e, i) {
                return P(this, t, e, !1, i);
            }, a.prototype.copy = function(t, e, i, n) {
                if (!a.isBuffer(t)) throw new TypeError("argument should be a Buffer");
                if (i || (i = 0), n || 0 === n || (n = this.length), e >= t.length && (e = t.length), e || (e = 0), n > 0 && n < i && (n = i), n === i) return 0;
                if (0 === t.length || 0 === this.length) return 0;
                if (e < 0) throw new RangeError("targetStart out of bounds");
                if (i < 0 || i >= this.length) throw new RangeError("Index out of range");
                if (n < 0) throw new RangeError("sourceEnd out of bounds");
                n > this.length && (n = this.length), t.length - e < n - i && (n = t.length - e + i);
                const r = n - i;
                return this === t && "function" == typeof Uint8Array.prototype.copyWithin ? this.copyWithin(e, i, n) : Uint8Array.prototype.set.call(t, this.subarray(i, n), e), r;
            }, a.prototype.fill = function(t, e, i, n) {
                if ("string" == typeof t) {
                    if ("string" == typeof e ? (n = e, e = 0, i = this.length) : "string" == typeof i && (n = i, i = this.length), void 0 !== n && "string" != typeof n) throw new TypeError("encoding must be a string");
                    if ("string" == typeof n && !a.isEncoding(n)) throw new TypeError("Unknown encoding: " + n);
                    if (1 === t.length) {
                        const e = t.charCodeAt(0);
                        ("utf8" === n && e < 128 || "latin1" === n) && (t = e);
                    }
                } else "number" == typeof t ? t &= 255 : "boolean" == typeof t && (t = Number(t));
                if (e < 0 || this.length < e || this.length < i) throw new RangeError("Out of range index");
                if (i <= e) return this;
                let r;
                if (e >>>= 0, i = void 0 === i ? this.length : i >>> 0, t || (t = 0), "number" == typeof t) for(r = e; r < i; ++r)this[r] = t;
                else {
                    const s = a.isBuffer(t) ? t : a.from(t, n), o = s.length;
                    if (0 === o) throw new TypeError('The value "' + t + '" is invalid for argument "value"');
                    for(r = 0; r < i - e; ++r)this[r + e] = s[r % o];
                }
                return this;
            };
            const M = {};
            function j(t, e, i) {
                M[t] = class extends i {
                    constructor(){
                        super(), Object.defineProperty(this, "message", {
                            value: e.apply(this, arguments),
                            writable: !0,
                            configurable: !0
                        }), this.name = `${this.name} [${t}]`, this.stack, delete this.name;
                    }
                    get code() {
                        return t;
                    }
                    set code(t) {
                        Object.defineProperty(this, "code", {
                            configurable: !0,
                            enumerable: !0,
                            value: t,
                            writable: !0
                        });
                    }
                    toString() {
                        return `${this.name} [${t}]: ${this.message}`;
                    }
                };
            }
            function V(t) {
                let e = "", i = t.length;
                const n = "-" === t[0] ? 1 : 0;
                for(; i >= n + 4; i -= 3)e = `_${t.slice(i - 3, i)}${e}`;
                return `${t.slice(0, i)}${e}`;
            }
            function z(t, e, i, n, r, s) {
                if (t > i || t < e) {
                    const n = "bigint" == typeof e ? "n" : "";
                    let r;
                    throw r = s > 3 ? 0 === e || e === BigInt(0) ? `>= 0${n} and < 2${n} ** ${8 * (s + 1)}${n}` : `>= -(2${n} ** ${8 * (s + 1) - 1}${n}) and < 2 ** ${8 * (s + 1) - 1}${n}` : `>= ${e}${n} and <= ${i}${n}`, new M.ERR_OUT_OF_RANGE("value", r, t);
                }
                !function(t, e, i) {
                    G(e, "offset"), void 0 !== t[e] && void 0 !== t[e + i] || K(e, t.length - (i + 1));
                }(n, r, s);
            }
            function G(t, e) {
                if ("number" != typeof t) throw new M.ERR_INVALID_ARG_TYPE(e, "number", t);
            }
            function K(t, e, i) {
                if (Math.floor(t) !== t) throw G(t, i), new M.ERR_OUT_OF_RANGE(i || "offset", "an integer", t);
                if (e < 0) throw new M.ERR_BUFFER_OUT_OF_BOUNDS;
                throw new M.ERR_OUT_OF_RANGE(i || "offset", `>= ${i ? 1 : 0} and <= ${e}`, t);
            }
            j("ERR_BUFFER_OUT_OF_BOUNDS", function(t) {
                return t ? `${t} is outside of buffer bounds` : "Attempt to access memory outside buffer bounds";
            }, RangeError), j("ERR_INVALID_ARG_TYPE", function(t, e) {
                return `The "${t}" argument must be of type number. Received type ${typeof e}`;
            }, TypeError), j("ERR_OUT_OF_RANGE", function(t, e, i) {
                let n = `The value of "${t}" is out of range.`, r = i;
                return Number.isInteger(i) && Math.abs(i) > 2 ** 32 ? r = V(String(i)) : "bigint" == typeof i && (r = String(i), (i > BigInt(2) ** BigInt(32) || i < -(BigInt(2) ** BigInt(32))) && (r = V(r)), r += "n"), n += ` It must be ${e}. Received ${r}`, n;
            }, RangeError);
            const W = /[^+/0-9A-Za-z-_]/g;
            function $(t, e) {
                let i;
                e = e || 1 / 0;
                const n = t.length;
                let r = null;
                const s = [];
                for(let o = 0; o < n; ++o){
                    if (i = t.charCodeAt(o), i > 55295 && i < 57344) {
                        if (!r) {
                            if (i > 56319) {
                                (e -= 3) > -1 && s.push(239, 191, 189);
                                continue;
                            }
                            if (o + 1 === n) {
                                (e -= 3) > -1 && s.push(239, 191, 189);
                                continue;
                            }
                            r = i;
                            continue;
                        }
                        if (i < 56320) {
                            (e -= 3) > -1 && s.push(239, 191, 189), r = i;
                            continue;
                        }
                        i = 65536 + (r - 55296 << 10 | i - 56320);
                    } else r && (e -= 3) > -1 && s.push(239, 191, 189);
                    if (r = null, i < 128) {
                        if ((e -= 1) < 0) break;
                        s.push(i);
                    } else if (i < 2048) {
                        if ((e -= 2) < 0) break;
                        s.push(i >> 6 | 192, 63 & i | 128);
                    } else if (i < 65536) {
                        if ((e -= 3) < 0) break;
                        s.push(i >> 12 | 224, i >> 6 & 63 | 128, 63 & i | 128);
                    } else {
                        if (!(i < 1114112)) throw new Error("Invalid code point");
                        if ((e -= 4) < 0) break;
                        s.push(i >> 18 | 240, i >> 12 & 63 | 128, i >> 6 & 63 | 128, 63 & i | 128);
                    }
                }
                return s;
            }
            function Y(t) {
                return n.toByteArray(function(t) {
                    if ((t = (t = t.split("=")[0]).trim().replace(W, "")).length < 2) return "";
                    for(; t.length % 4 != 0;)t += "=";
                    return t;
                }(t));
            }
            function X(t, e, i, n) {
                let r;
                for(r = 0; r < n && !(r + i >= e.length || r >= t.length); ++r)e[r + i] = t[r];
                return r;
            }
            function H(t, e) {
                return t instanceof e || null != t && null != t.constructor && null != t.constructor.name && t.constructor.name === e.name;
            }
            function J(t) {
                return t != t;
            }
            const q = function() {
                const t = "0123456789abcdef", e = new Array(256);
                for(let i = 0; i < 16; ++i){
                    const n = 16 * i;
                    for(let r = 0; r < 16; ++r)e[n + r] = t[i] + t[r];
                }
                return e;
            }();
            function Q(t) {
                return "undefined" == typeof BigInt ? Z : t;
            }
            function Z() {
                throw new Error("BigInt not supported");
            }
        },
        957: (t, e, i)=>{
            "use strict";
            var n = i(886);
            e.fromCallback = function(t, e) {
                if (void 0 === t) {
                    var i = new Promise(function(e, i) {
                        t = function(t, n) {
                            t ? i(t) : e(n);
                        };
                    });
                    t[void 0 !== e ? e : "promise"] = i;
                } else if ("function" != typeof t) throw new TypeError("Callback must be a function");
                return t;
            }, e.fromPromise = function(t, e) {
                if (void 0 === e) return t;
                t.then(function(t) {
                    n(()=>e(null, t));
                }).catch(function(t) {
                    n(()=>e(t));
                });
            };
        },
        886: (t)=>{
            t.exports = "function" == typeof queueMicrotask ? queueMicrotask : (t)=>Promise.resolve().then(t);
        },
        840: (t, e)=>{
            function i(t) {
                for(var e = "", i = 0; i < t.length; i++){
                    var n = t[i];
                    isNaN(Number(n)) || " " === n ? "-" !== n && (e += n) : e += String(9 - Number(n));
                }
                return e;
            }
            e.encode = function(t) {
                if (isNaN(t)) return "DaN";
                if (0 === t) return "FE  0M0";
                if (t === 1 / 0) return "FF";
                if (t === -1 / 0) return "DD";
                var e, n = t.toExponential().split("e"), r = Number(n[1]) + 500, s = n[0] + (-1 === n[0].indexOf(".") ? "." : "") + "0".repeat(20), o = "E" + (e = String(r), " ".repeat(3 - e.length).substr(0, 3) + e + "M") + String(s);
                return t > 0 ? "F" + o : "D" + i(o);
            }, e.decode = function(t) {
                if ("DaN" === t) return NaN;
                if ("FF" === t) return 1 / 0;
                if ("DD" === t) return -1 / 0;
                var e = "D" === t[0], n = (e ? i(t) : t).slice(2).split("M");
                return Number((e ? "-" : "") + n[1] + "e" + String(Number(n[0]) - 500));
            };
        },
        833: (t, e)=>{
            var i = {
                "?": "?@",
                "!": "??",
                '"': "?%"
            }, n = {
                "?@": "?",
                "??": "!",
                "?%": '"'
            };
            e.factory = function(t) {
                return {
                    encode: e,
                    decode: function(e) {
                        if ("A" === e) return null;
                        if ("K!" === e) return [];
                        for(var i, r = e.split('"'), s = [
                            []
                        ], o = 0, h = r.length, a = 0; a < h; a++){
                            for(var c = r[a], u = c.length, l = 0; "K" == c[l];)l++;
                            for(var f = 0; "!" == c[u - f - 1];)f++;
                            for(var d = c.slice(l, u - f), p = o + l, y = o; y < p; y++)s[y + 1] = [], s[y].push(s[y + 1]), i = s[o = p];
                            for(0 !== d.length && i.push(t.decode(/\?[%\?@]/.test(E = d) ? E.replace(/\?[%\?@]/g, function(t) {
                                return n[t];
                            }) : E)), y = p = o - f; y < o; y++)s[y + 1] = [], i = s[o = p];
                        }
                        var E;
                        return s[0][0];
                    }
                };
                function e(t) {
                    if (null === t) return "A";
                    if (!Array.isArray(t)) throw new Error("can only encode arrays");
                    var e = t.length;
                    if (0 == e) return "K!";
                    for(var i = r(t[0]), n = 1; n < e; n++)i += '"' + r(t[n]);
                    return "K" + i + "!";
                }
                function r(n) {
                    var r;
                    return "object" == typeof n ? e(n) : (r = t.encode(n), /[!"]/.test(r) ? r.replace(/[\?!"]/g, function(t) {
                        return i[t];
                    }) : r);
                }
            };
        },
        483: (t, e, i)=>{
            var n = i(840), r = i(833);
            e.flip = function(t) {
                var e = t.toString(), i = "";
                for(var n in e)i += "." == e[n] ? "." : 9 - +e[n];
                return i;
            }, e.number = n, e.string = {
                encode: function(t) {
                    return /\x00|\x01/.test(t) ? "J" + t.replace(/\x01/g, "\x01\x01").replace(/\x00/g, "\x01") : "J" + t;
                },
                decode: function(t) {
                    if ("J" === t[0]) return t.substring(1);
                }
            }, e.encode = function(t) {
                return e[typeof t].encode(t);
            }, e.decode = function(t) {
                if ("" === t) return t;
                if (!s[t[0]]) throw new Error("no decoder for:" + JSON.stringify(t));
                return s[t[0]](t);
            }, e.object = r.factory(e), e.boolean = {
                encode: function(t) {
                    return t ? "C" : "B";
                },
                decode: function(t) {
                    return "C" === t;
                }
            }, e.undefined = {
                encode: function(t) {
                    return "L";
                },
                decode: function() {}
            };
            var s = {
                A: e.object.decode,
                B: e.boolean.decode,
                C: e.boolean.decode,
                D: e.number.decode,
                F: e.number.decode,
                J: e.string.decode,
                K: e.object.decode,
                L: e.undefined.decode
            };
            e.buffer = !1, e.type = "charwise";
        },
        729: (t)=>{
            "use strict";
            var e = Object.prototype.hasOwnProperty, i = "~";
            function n() {}
            function r(t, e, i) {
                this.fn = t, this.context = e, this.once = i || !1;
            }
            function s(t, e, n, s, o) {
                if ("function" != typeof n) throw new TypeError("The listener must be a function");
                var h = new r(n, s || t, o), a = i ? i + e : e;
                return t._events[a] ? t._events[a].fn ? t._events[a] = [
                    t._events[a],
                    h
                ] : t._events[a].push(h) : (t._events[a] = h, t._eventsCount++), t;
            }
            function o(t, e) {
                0 == --t._eventsCount ? t._events = new n : delete t._events[e];
            }
            function h() {
                this._events = new n, this._eventsCount = 0;
            }
            Object.create && (n.prototype = Object.create(null), (new n).__proto__ || (i = !1)), h.prototype.eventNames = function() {
                var t, n, r = [];
                if (0 === this._eventsCount) return r;
                for(n in t = this._events)e.call(t, n) && r.push(i ? n.slice(1) : n);
                return Object.getOwnPropertySymbols ? r.concat(Object.getOwnPropertySymbols(t)) : r;
            }, h.prototype.listeners = function(t) {
                var e = i ? i + t : t, n = this._events[e];
                if (!n) return [];
                if (n.fn) return [
                    n.fn
                ];
                for(var r = 0, s = n.length, o = new Array(s); r < s; r++)o[r] = n[r].fn;
                return o;
            }, h.prototype.listenerCount = function(t) {
                var e = i ? i + t : t, n = this._events[e];
                return n ? n.fn ? 1 : n.length : 0;
            }, h.prototype.emit = function(t, e, n, r, s, o) {
                var h = i ? i + t : t;
                if (!this._events[h]) return !1;
                var a, c, u = this._events[h], l = arguments.length;
                if (u.fn) {
                    switch(u.once && this.removeListener(t, u.fn, void 0, !0), l){
                        case 1:
                            return u.fn.call(u.context), !0;
                        case 2:
                            return u.fn.call(u.context, e), !0;
                        case 3:
                            return u.fn.call(u.context, e, n), !0;
                        case 4:
                            return u.fn.call(u.context, e, n, r), !0;
                        case 5:
                            return u.fn.call(u.context, e, n, r, s), !0;
                        case 6:
                            return u.fn.call(u.context, e, n, r, s, o), !0;
                    }
                    for(c = 1, a = new Array(l - 1); c < l; c++)a[c - 1] = arguments[c];
                    u.fn.apply(u.context, a);
                } else {
                    var f, d = u.length;
                    for(c = 0; c < d; c++)switch(u[c].once && this.removeListener(t, u[c].fn, void 0, !0), l){
                        case 1:
                            u[c].fn.call(u[c].context);
                            break;
                        case 2:
                            u[c].fn.call(u[c].context, e);
                            break;
                        case 3:
                            u[c].fn.call(u[c].context, e, n);
                            break;
                        case 4:
                            u[c].fn.call(u[c].context, e, n, r);
                            break;
                        default:
                            if (!a) for(f = 1, a = new Array(l - 1); f < l; f++)a[f - 1] = arguments[f];
                            u[c].fn.apply(u[c].context, a);
                    }
                }
                return !0;
            }, h.prototype.on = function(t, e, i) {
                return s(this, t, e, i, !1);
            }, h.prototype.once = function(t, e, i) {
                return s(this, t, e, i, !0);
            }, h.prototype.removeListener = function(t, e, n, r) {
                var s = i ? i + t : t;
                if (!this._events[s]) return this;
                if (!e) return o(this, s), this;
                var h = this._events[s];
                if (h.fn) h.fn !== e || r && !h.once || n && h.context !== n || o(this, s);
                else {
                    for(var a = 0, c = [], u = h.length; a < u; a++)(h[a].fn !== e || r && !h[a].once || n && h[a].context !== n) && c.push(h[a]);
                    c.length ? this._events[s] = 1 === c.length ? c[0] : c : o(this, s);
                }
                return this;
            }, h.prototype.removeAllListeners = function(t) {
                var e;
                return t ? (e = i ? i + t : t, this._events[e] && o(this, e)) : (this._events = new n, this._eventsCount = 0), this;
            }, h.prototype.off = h.prototype.removeListener, h.prototype.addListener = h.prototype.on, h.prefixed = i, h.EventEmitter = h, t.exports = h;
        },
        187: (t)=>{
            "use strict";
            var e, i = "object" == typeof Reflect ? Reflect : null, n = i && "function" == typeof i.apply ? i.apply : function(t, e, i) {
                return Function.prototype.apply.call(t, e, i);
            };
            e = i && "function" == typeof i.ownKeys ? i.ownKeys : Object.getOwnPropertySymbols ? function(t) {
                return Object.getOwnPropertyNames(t).concat(Object.getOwnPropertySymbols(t));
            } : function(t) {
                return Object.getOwnPropertyNames(t);
            };
            var r = Number.isNaN || function(t) {
                return t != t;
            };
            function s() {
                s.init.call(this);
            }
            t.exports = s, t.exports.once = function(t, e) {
                return new Promise(function(i, n) {
                    function r(i) {
                        t.removeListener(e, s), n(i);
                    }
                    function s() {
                        "function" == typeof t.removeListener && t.removeListener("error", r), i([].slice.call(arguments));
                    }
                    y(t, e, s, {
                        once: !0
                    }), "error" !== e && function(t, e, i) {
                        "function" == typeof t.on && y(t, "error", e, {
                            once: !0
                        });
                    }(t, r);
                });
            }, s.EventEmitter = s, s.prototype._events = void 0, s.prototype._eventsCount = 0, s.prototype._maxListeners = void 0;
            var o = 10;
            function h(t) {
                if ("function" != typeof t) throw new TypeError('The "listener" argument must be of type Function. Received type ' + typeof t);
            }
            function a(t) {
                return void 0 === t._maxListeners ? s.defaultMaxListeners : t._maxListeners;
            }
            function c(t, e, i, n) {
                var r, s, o, c;
                if (h(i), void 0 === (s = t._events) ? (s = t._events = Object.create(null), t._eventsCount = 0) : (void 0 !== s.newListener && (t.emit("newListener", e, i.listener ? i.listener : i), s = t._events), o = s[e]), void 0 === o) o = s[e] = i, ++t._eventsCount;
                else if ("function" == typeof o ? o = s[e] = n ? [
                    i,
                    o
                ] : [
                    o,
                    i
                ] : n ? o.unshift(i) : o.push(i), (r = a(t)) > 0 && o.length > r && !o.warned) {
                    o.warned = !0;
                    var u = new Error("Possible EventEmitter memory leak detected. " + o.length + " " + String(e) + " listeners added. Use emitter.setMaxListeners() to increase limit");
                    u.name = "MaxListenersExceededWarning", u.emitter = t, u.type = e, u.count = o.length, c = u, console && console.warn && console.warn(c);
                }
                return t;
            }
            function u() {
                if (!this.fired) return this.target.removeListener(this.type, this.wrapFn), this.fired = !0, 0 === arguments.length ? this.listener.call(this.target) : this.listener.apply(this.target, arguments);
            }
            function l(t, e, i) {
                var n = {
                    fired: !1,
                    wrapFn: void 0,
                    target: t,
                    type: e,
                    listener: i
                }, r = u.bind(n);
                return r.listener = i, n.wrapFn = r, r;
            }
            function f(t, e, i) {
                var n = t._events;
                if (void 0 === n) return [];
                var r = n[e];
                return void 0 === r ? [] : "function" == typeof r ? i ? [
                    r.listener || r
                ] : [
                    r
                ] : i ? function(t) {
                    for(var e = new Array(t.length), i = 0; i < e.length; ++i)e[i] = t[i].listener || t[i];
                    return e;
                }(r) : p(r, r.length);
            }
            function d(t) {
                var e = this._events;
                if (void 0 !== e) {
                    var i = e[t];
                    if ("function" == typeof i) return 1;
                    if (void 0 !== i) return i.length;
                }
                return 0;
            }
            function p(t, e) {
                for(var i = new Array(e), n = 0; n < e; ++n)i[n] = t[n];
                return i;
            }
            function y(t, e, i, n) {
                if ("function" == typeof t.on) n.once ? t.once(e, i) : t.on(e, i);
                else {
                    if ("function" != typeof t.addEventListener) throw new TypeError('The "emitter" argument must be of type EventEmitter. Received type ' + typeof t);
                    t.addEventListener(e, function r(s) {
                        n.once && t.removeEventListener(e, r), i(s);
                    });
                }
            }
            Object.defineProperty(s, "defaultMaxListeners", {
                enumerable: !0,
                get: function() {
                    return o;
                },
                set: function(t) {
                    if ("number" != typeof t || t < 0 || r(t)) throw new RangeError('The value of "defaultMaxListeners" is out of range. It must be a non-negative number. Received ' + t + ".");
                    o = t;
                }
            }), s.init = function() {
                void 0 !== this._events && this._events !== Object.getPrototypeOf(this)._events || (this._events = Object.create(null), this._eventsCount = 0), this._maxListeners = this._maxListeners || void 0;
            }, s.prototype.setMaxListeners = function(t) {
                if ("number" != typeof t || t < 0 || r(t)) throw new RangeError('The value of "n" is out of range. It must be a non-negative number. Received ' + t + ".");
                return this._maxListeners = t, this;
            }, s.prototype.getMaxListeners = function() {
                return a(this);
            }, s.prototype.emit = function(t) {
                for(var e = [], i = 1; i < arguments.length; i++)e.push(arguments[i]);
                var r = "error" === t, s = this._events;
                if (void 0 !== s) r = r && void 0 === s.error;
                else if (!r) return !1;
                if (r) {
                    var o;
                    if (e.length > 0 && (o = e[0]), o instanceof Error) throw o;
                    var h = new Error("Unhandled error." + (o ? " (" + o.message + ")" : ""));
                    throw h.context = o, h;
                }
                var a = s[t];
                if (void 0 === a) return !1;
                if ("function" == typeof a) n(a, this, e);
                else {
                    var c = a.length, u = p(a, c);
                    for(i = 0; i < c; ++i)n(u[i], this, e);
                }
                return !0;
            }, s.prototype.addListener = function(t, e) {
                return c(this, t, e, !1);
            }, s.prototype.on = s.prototype.addListener, s.prototype.prependListener = function(t, e) {
                return c(this, t, e, !0);
            }, s.prototype.once = function(t, e) {
                return h(e), this.on(t, l(this, t, e)), this;
            }, s.prototype.prependOnceListener = function(t, e) {
                return h(e), this.prependListener(t, l(this, t, e)), this;
            }, s.prototype.removeListener = function(t, e) {
                var i, n, r, s, o;
                if (h(e), void 0 === (n = this._events)) return this;
                if (void 0 === (i = n[t])) return this;
                if (i === e || i.listener === e) 0 == --this._eventsCount ? this._events = Object.create(null) : (delete n[t], n.removeListener && this.emit("removeListener", t, i.listener || e));
                else if ("function" != typeof i) {
                    for(r = -1, s = i.length - 1; s >= 0; s--)if (i[s] === e || i[s].listener === e) {
                        o = i[s].listener, r = s;
                        break;
                    }
                    if (r < 0) return this;
                    0 === r ? i.shift() : function(t, e) {
                        for(; e + 1 < t.length; e++)t[e] = t[e + 1];
                        t.pop();
                    }(i, r), 1 === i.length && (n[t] = i[0]), void 0 !== n.removeListener && this.emit("removeListener", t, o || e);
                }
                return this;
            }, s.prototype.off = s.prototype.removeListener, s.prototype.removeAllListeners = function(t) {
                var e, i, n;
                if (void 0 === (i = this._events)) return this;
                if (void 0 === i.removeListener) return 0 === arguments.length ? (this._events = Object.create(null), this._eventsCount = 0) : void 0 !== i[t] && (0 == --this._eventsCount ? this._events = Object.create(null) : delete i[t]), this;
                if (0 === arguments.length) {
                    var r, s = Object.keys(i);
                    for(n = 0; n < s.length; ++n)"removeListener" !== (r = s[n]) && this.removeAllListeners(r);
                    return this.removeAllListeners("removeListener"), this._events = Object.create(null), this._eventsCount = 0, this;
                }
                if ("function" == typeof (e = i[t])) this.removeListener(t, e);
                else if (void 0 !== e) for(n = e.length - 1; n >= 0; n--)this.removeListener(t, e[n]);
                return this;
            }, s.prototype.listeners = function(t) {
                return f(this, t, !0);
            }, s.prototype.rawListeners = function(t) {
                return f(this, t, !1);
            }, s.listenerCount = function(t, e) {
                return "function" == typeof t.listenerCount ? t.listenerCount(e) : d.call(t, e);
            }, s.prototype.listenerCount = d, s.prototype.eventNames = function() {
                return this._eventsCount > 0 ? e(this._events) : [];
            };
        },
        645: (t, e)=>{
            e.read = function(t, e, i, n, r) {
                var s, o, h = 8 * r - n - 1, a = (1 << h) - 1, c = a >> 1, u = -7, l = i ? r - 1 : 0, f = i ? -1 : 1, d = t[e + l];
                for(l += f, s = d & (1 << -u) - 1, d >>= -u, u += h; u > 0; s = 256 * s + t[e + l], l += f, u -= 8);
                for(o = s & (1 << -u) - 1, s >>= -u, u += n; u > 0; o = 256 * o + t[e + l], l += f, u -= 8);
                if (0 === s) s = 1 - c;
                else {
                    if (s === a) return o ? NaN : 1 / 0 * (d ? -1 : 1);
                    o += Math.pow(2, n), s -= c;
                }
                return (d ? -1 : 1) * o * Math.pow(2, s - n);
            }, e.write = function(t, e, i, n, r, s) {
                var o, h, a, c = 8 * s - r - 1, u = (1 << c) - 1, l = u >> 1, f = 23 === r ? Math.pow(2, -24) - Math.pow(2, -77) : 0, d = n ? 0 : s - 1, p = n ? 1 : -1, y = e < 0 || 0 === e && 1 / e < 0 ? 1 : 0;
                for(e = Math.abs(e), isNaN(e) || e === 1 / 0 ? (h = isNaN(e) ? 1 : 0, o = u) : (o = Math.floor(Math.log(e) / Math.LN2), e * (a = Math.pow(2, -o)) < 1 && (o--, a *= 2), (e += o + l >= 1 ? f / a : f * Math.pow(2, 1 - l)) * a >= 2 && (o++, a /= 2), o + l >= u ? (h = 0, o = u) : o + l >= 1 ? (h = (e * a - 1) * Math.pow(2, r), o += l) : (h = e * Math.pow(2, l - 1) * Math.pow(2, r), o = 0)); r >= 8; t[i + d] = 255 & h, d += p, h /= 256, r -= 8);
                for(o = o << r | h, c += r; c > 0; t[i + d] = 255 & o, d += p, o /= 256, c -= 8);
                t[i + d - p] |= 128 * y;
            };
        },
        499: (t, e, i)=>{
            "use strict";
            const n = i(473), r = i(2), { Encoding: s } = i(266), { BufferFormat: o, ViewFormat: h, UTF8Format: a } = i(376), c = Symbol("formats"), u = Symbol("encodings"), l = new Set([
                "buffer",
                "view",
                "utf8"
            ]);
            e.Transcoder = class {
                constructor(t){
                    if (!Array.isArray(t)) throw new TypeError("The first argument 'formats' must be an array");
                    if (!t.every((t)=>l.has(t))) throw new TypeError("Format must be one of 'buffer', 'view', 'utf8'");
                    this[u] = new Map, this[c] = new Set(t);
                    for(const t in r)try {
                        this.encoding(t);
                    } catch (t) {
                        if ("LEVEL_ENCODING_NOT_SUPPORTED" !== t.code) throw t;
                    }
                }
                encodings() {
                    return Array.from(new Set(this[u].values()));
                }
                encoding(t) {
                    let e = this[u].get(t);
                    if (void 0 === e) {
                        if ("string" == typeof t && "" !== t) {
                            if (e = d[t], !e) throw new n(`Encoding '${t}' is not found`, {
                                code: "LEVEL_ENCODING_NOT_FOUND"
                            });
                        } else {
                            if ("object" != typeof t || null === t) throw new TypeError("First argument 'encoding' must be a string or object");
                            e = function(t) {
                                if (t instanceof s) return t;
                                const e = "type" in t && "string" == typeof t.type ? t.type : void 0, i = t.name || e || "anonymous-" + p++;
                                switch(function(t) {
                                    return "format" in t && void 0 !== t.format ? t.format : "buffer" in t && "boolean" == typeof t.buffer ? t.buffer ? "buffer" : "utf8" : "code" in t && Number.isInteger(t.code) ? "view" : "buffer";
                                }(t)){
                                    case "view":
                                        return new h({
                                            ...t,
                                            name: i
                                        });
                                    case "utf8":
                                        return new a({
                                            ...t,
                                            name: i
                                        });
                                    case "buffer":
                                        return new o({
                                            ...t,
                                            name: i
                                        });
                                    default:
                                        throw new TypeError("Format must be one of 'buffer', 'view', 'utf8'");
                                }
                            }(t);
                        }
                        const { name: i, format: r } = e;
                        if (!this[c].has(r)) {
                            if (this[c].has("view")) e = e.createViewTranscoder();
                            else if (this[c].has("buffer")) e = e.createBufferTranscoder();
                            else {
                                if (!this[c].has("utf8")) throw new n(`Encoding '${i}' cannot be transcoded`, {
                                    code: "LEVEL_ENCODING_NOT_SUPPORTED"
                                });
                                e = e.createUTF8Transcoder();
                            }
                        }
                        for (const n of [
                            t,
                            i,
                            e.name,
                            e.commonName
                        ])this[u].set(n, e);
                    }
                    return e;
                }
            };
            const f = {
                binary: r.buffer,
                "utf-8": r.utf8
            }, d = {
                ...r,
                ...f
            };
            let p = 0;
        },
        266: (t, e, i)=>{
            "use strict";
            const n = i(473), r = new Set([
                "buffer",
                "view",
                "utf8"
            ]);
            e.Encoding = class {
                constructor(t){
                    if (this.encode = t.encode || this.encode, this.decode = t.decode || this.decode, this.name = t.name || this.name, this.format = t.format || this.format, "function" != typeof this.encode) throw new TypeError("The 'encode' property must be a function");
                    if ("function" != typeof this.decode) throw new TypeError("The 'decode' property must be a function");
                    if (this.encode = this.encode.bind(this), this.decode = this.decode.bind(this), "string" != typeof this.name || "" === this.name) throw new TypeError("The 'name' property must be a string");
                    if ("string" != typeof this.format || !r.has(this.format)) throw new TypeError("The 'format' property must be one of 'buffer', 'view', 'utf8'");
                    t.createViewTranscoder && (this.createViewTranscoder = t.createViewTranscoder), t.createBufferTranscoder && (this.createBufferTranscoder = t.createBufferTranscoder), t.createUTF8Transcoder && (this.createUTF8Transcoder = t.createUTF8Transcoder);
                }
                get commonName() {
                    return this.name.split("+")[0];
                }
                createBufferTranscoder() {
                    throw new n(`Encoding '${this.name}' cannot be transcoded to 'buffer'`, {
                        code: "LEVEL_ENCODING_NOT_SUPPORTED"
                    });
                }
                createViewTranscoder() {
                    throw new n(`Encoding '${this.name}' cannot be transcoded to 'view'`, {
                        code: "LEVEL_ENCODING_NOT_SUPPORTED"
                    });
                }
                createUTF8Transcoder() {
                    throw new n(`Encoding '${this.name}' cannot be transcoded to 'utf8'`, {
                        code: "LEVEL_ENCODING_NOT_SUPPORTED"
                    });
                }
            };
        },
        2: (t, e, i)=>{
            "use strict";
            const { Buffer: n } = i(764) || {
                Buffer: {
                    isBuffer: ()=>!1
                }
            }, { textEncoder: r, textDecoder: s } = i(850)(), { BufferFormat: o, ViewFormat: h, UTF8Format: a } = i(376), c = (t)=>t;
            e.utf8 = new a({
                encode: function(t) {
                    return n.isBuffer(t) ? t.toString("utf8") : ArrayBuffer.isView(t) ? s.decode(t) : String(t);
                },
                decode: c,
                name: "utf8",
                createViewTranscoder () {
                    return new h({
                        encode: function(t) {
                            return ArrayBuffer.isView(t) ? t : r.encode(t);
                        },
                        decode: function(t) {
                            return s.decode(t);
                        },
                        name: `${this.name}+view`
                    });
                },
                createBufferTranscoder () {
                    return new o({
                        encode: function(t) {
                            return n.isBuffer(t) ? t : ArrayBuffer.isView(t) ? n.from(t.buffer, t.byteOffset, t.byteLength) : n.from(String(t), "utf8");
                        },
                        decode: function(t) {
                            return t.toString("utf8");
                        },
                        name: `${this.name}+buffer`
                    });
                }
            }), e.json = new a({
                encode: JSON.stringify,
                decode: JSON.parse,
                name: "json"
            }), e.buffer = new o({
                encode: function(t) {
                    return n.isBuffer(t) ? t : ArrayBuffer.isView(t) ? n.from(t.buffer, t.byteOffset, t.byteLength) : n.from(String(t), "utf8");
                },
                decode: c,
                name: "buffer",
                createViewTranscoder () {
                    return new h({
                        encode: function(t) {
                            return ArrayBuffer.isView(t) ? t : n.from(String(t), "utf8");
                        },
                        decode: function(t) {
                            return n.from(t.buffer, t.byteOffset, t.byteLength);
                        },
                        name: `${this.name}+view`
                    });
                }
            }), e.view = new h({
                encode: function(t) {
                    return ArrayBuffer.isView(t) ? t : r.encode(t);
                },
                decode: c,
                name: "view",
                createBufferTranscoder () {
                    return new o({
                        encode: function(t) {
                            return n.isBuffer(t) ? t : ArrayBuffer.isView(t) ? n.from(t.buffer, t.byteOffset, t.byteLength) : n.from(String(t), "utf8");
                        },
                        decode: c,
                        name: `${this.name}+buffer`
                    });
                }
            }), e.hex = new o({
                encode: function(t) {
                    return n.isBuffer(t) ? t : n.from(String(t), "hex");
                },
                decode: function(t) {
                    return t.toString("hex");
                },
                name: "hex"
            }), e.base64 = new o({
                encode: function(t) {
                    return n.isBuffer(t) ? t : n.from(String(t), "base64");
                },
                decode: function(t) {
                    return t.toString("base64");
                },
                name: "base64"
            });
        },
        376: (t, e, i)=>{
            "use strict";
            const { Buffer: n } = i(764) || {}, { Encoding: r } = i(266), s = i(850);
            class o extends r {
                constructor(t){
                    super({
                        ...t,
                        format: "buffer"
                    });
                }
                createViewTranscoder() {
                    return new h({
                        encode: this.encode,
                        decode: (t)=>this.decode(n.from(t.buffer, t.byteOffset, t.byteLength)),
                        name: `${this.name}+view`
                    });
                }
                createBufferTranscoder() {
                    return this;
                }
            }
            class h extends r {
                constructor(t){
                    super({
                        ...t,
                        format: "view"
                    });
                }
                createBufferTranscoder() {
                    return new o({
                        encode: (t)=>{
                            const e = this.encode(t);
                            return n.from(e.buffer, e.byteOffset, e.byteLength);
                        },
                        decode: this.decode,
                        name: `${this.name}+buffer`
                    });
                }
                createViewTranscoder() {
                    return this;
                }
            }
            e.BufferFormat = o, e.ViewFormat = h, e.UTF8Format = class extends r {
                constructor(t){
                    super({
                        ...t,
                        format: "utf8"
                    });
                }
                createBufferTranscoder() {
                    return new o({
                        encode: (t)=>n.from(this.encode(t), "utf8"),
                        decode: (t)=>this.decode(t.toString("utf8")),
                        name: `${this.name}+buffer`
                    });
                }
                createViewTranscoder() {
                    const { textEncoder: t, textDecoder: e } = s();
                    return new h({
                        encode: (e)=>t.encode(this.encode(e)),
                        decode: (t)=>this.decode(e.decode(t)),
                        name: `${this.name}+view`
                    });
                }
                createUTF8Transcoder() {
                    return this;
                }
            };
        },
        850: (t)=>{
            "use strict";
            let e = null;
            t.exports = function() {
                return null === e && (e = {
                    textEncoder: new TextEncoder,
                    textDecoder: new TextDecoder
                }), e;
            };
        },
        473: (t)=>{
            "use strict";
            t.exports = class extends Error {
                constructor(t, e){
                    super(t || ""), "object" == typeof e && null !== e && (e.code && (this.code = String(e.code)), e.expected && (this.expected = !0), e.transient && (this.transient = !0), e.cause && (this.cause = e.cause)), Error.captureStackTrace && Error.captureStackTrace(this, this.constructor);
                }
            };
        },
        349: function(t, e) {
            !function(t) {
                "use strict";
                const e = function(t, e) {
                    return t.slice(e - 1).map((i, n)=>t.slice(n, n + e));
                };
                t.ngraminator = function(t, i) {
                    return i.reduce((i, n)=>[
                            ...e(t, n),
                            ...i
                        ], []).sort();
                }, Object.defineProperty(t, "__esModule", {
                    value: !0
                });
            }(e);
        },
        155: (t)=>{
            var e, i, n = t.exports = {};
            function r() {
                throw new Error("setTimeout has not been defined");
            }
            function s() {
                throw new Error("clearTimeout has not been defined");
            }
            function o(t) {
                if (e === setTimeout) return setTimeout(t, 0);
                if ((e === r || !e) && setTimeout) return e = setTimeout, setTimeout(t, 0);
                try {
                    return e(t, 0);
                } catch (i) {
                    try {
                        return e.call(null, t, 0);
                    } catch (i) {
                        return e.call(this, t, 0);
                    }
                }
            }
            !function() {
                try {
                    e = "function" == typeof setTimeout ? setTimeout : r;
                } catch (t) {
                    e = r;
                }
                try {
                    i = "function" == typeof clearTimeout ? clearTimeout : s;
                } catch (t) {
                    i = s;
                }
            }();
            var h, a = [], c = !1, u = -1;
            function l() {
                c && h && (c = !1, h.length ? a = h.concat(a) : u = -1, a.length && f());
            }
            function f() {
                if (!c) {
                    var t = o(l);
                    c = !0;
                    for(var e = a.length; e;){
                        for(h = a, a = []; ++u < e;)h && h[u].run();
                        u = -1, e = a.length;
                    }
                    h = null, c = !1, function(t) {
                        if (i === clearTimeout) return clearTimeout(t);
                        if ((i === s || !i) && clearTimeout) return i = clearTimeout, clearTimeout(t);
                        try {
                            return i(t);
                        } catch (e) {
                            try {
                                return i.call(null, t);
                            } catch (e) {
                                return i.call(this, t);
                            }
                        }
                    }(t);
                }
            }
            function d(t, e) {
                this.fun = t, this.array = e;
            }
            function p() {}
            n.nextTick = function(t) {
                var e = new Array(arguments.length - 1);
                if (arguments.length > 1) for(var i = 1; i < arguments.length; i++)e[i - 1] = arguments[i];
                a.push(new d(t, e)), 1 !== a.length || c || o(f);
            }, d.prototype.run = function() {
                this.fun.apply(null, this.array);
            }, n.title = "browser", n.browser = !0, n.env = {}, n.argv = [], n.version = "", n.versions = {}, n.on = p, n.addListener = p, n.once = p, n.off = p, n.removeListener = p, n.removeAllListeners = p, n.emit = p, n.prependListener = p, n.prependOnceListener = p, n.listeners = function(t) {
                return [];
            }, n.binding = function(t) {
                throw new Error("process.binding is not supported");
            }, n.cwd = function() {
                return "/";
            }, n.chdir = function(t) {
                throw new Error("process.chdir is not supported");
            }, n.umask = function() {
                return 0;
            };
        },
        375: (t, e, i)=>{
            let n;
            t.exports = "function" == typeof queueMicrotask ? queueMicrotask.bind("undefined" != typeof window ? window : i.g) : (t)=>(n || (n = Promise.resolve())).then(t).catch((t)=>setTimeout(()=>{
                        throw t;
                    }, 0));
        },
        967: (t, e, i)=>{
            t.exports = function(t, e, i) {
                if ("number" != typeof e) throw new Error("second argument must be a Number");
                let r, s, o, h, a, c, u = !0;
                function l(t) {
                    function e() {
                        i && i(t, r), i = null;
                    }
                    u ? n(e) : e();
                }
                function f(e, i, n) {
                    if (r[e] = n, i && (a = !0), 0 == --o || i) l(i);
                    else if (!a && c < s) {
                        let e;
                        h ? (e = h[c], c += 1, t[e](function(t, i) {
                            f(e, t, i);
                        })) : (e = c, c += 1, t[e](function(t, i) {
                            f(e, t, i);
                        }));
                    }
                }
                Array.isArray(t) ? (r = [], o = s = t.length) : (h = Object.keys(t), r = {}, o = s = h.length), c = e, o ? h ? h.some(function(i, n) {
                    return t[i](function(t, e) {
                        f(i, t, e);
                    }), n === e - 1;
                }) : t.some(function(t, i) {
                    return t(function(t, e) {
                        f(i, t, e);
                    }), i === e - 1;
                }) : l(null), u = !1;
            };
            const n = i(375);
        },
        971: (t)=>{
            t.exports = function(t, e) {
                e = Object.assign({}, {
                    ngramLengths: [
                        1
                    ]
                }, e);
                const i = t.reduce((t, i, n, r)=>(e.ngramLengths.forEach((e)=>{
                        var s = r.slice(n, n + e);
                        s.length === e && (i = JSON.stringify(s), t[i] = t[i] || [], t[i].push(n));
                    }), t), {});
                return Object.keys(i).map((t)=>({
                        term: JSON.parse(t),
                        positions: i[t]
                    })).sort((t, e)=>t.term[0] > e.term[0]);
            };
        },
        692: (t)=>{
            "use strict";
            function e(t) {
                return Object.prototype.toString.call(t);
            }
            var i = Array.isArray || function(t) {
                return "[object Array]" === Object.prototype.toString.call(t);
            };
            function n(t, e) {
                if (t.forEach) return t.forEach(e);
                for(var i = 0; i < t.length; i++)e(t[i], i, t);
            }
            var r = Object.keys || function(t) {
                var e = [];
                for(var i in t)e.push(i);
                return e;
            }, s = Object.prototype.hasOwnProperty || function(t, e) {
                return e in t;
            };
            function o(t) {
                if ("object" == typeof t && null !== t) {
                    var s;
                    if (i(t)) s = [];
                    else if ("[object Date]" === e(t)) s = new Date(t.getTime ? t.getTime() : t);
                    else if ("[object RegExp]" === e(t)) s = new RegExp(t);
                    else if (function(t) {
                        return "[object Error]" === e(t);
                    }(t)) s = {
                        message: t.message
                    };
                    else if (function(t) {
                        return "[object Boolean]" === e(t);
                    }(t) || function(t) {
                        return "[object Number]" === e(t);
                    }(t) || function(t) {
                        return "[object String]" === e(t);
                    }(t)) s = Object(t);
                    else if (Object.create && Object.getPrototypeOf) s = Object.create(Object.getPrototypeOf(t));
                    else if (t.constructor === Object) s = {};
                    else {
                        var o = t.constructor && t.constructor.prototype || t.__proto__ || {}, h = function() {};
                        h.prototype = o, s = new h;
                    }
                    return n(r(t), function(e) {
                        s[e] = t[e];
                    }), s;
                }
                return t;
            }
            function h(t, e, h) {
                var a = [], c = [], u = !0;
                return function t(l) {
                    var f = h ? o(l) : l, d = {}, p = !0, y = {
                        node: f,
                        node_: l,
                        path: [].concat(a),
                        parent: c[c.length - 1],
                        parents: c,
                        key: a[a.length - 1],
                        isRoot: 0 === a.length,
                        level: a.length,
                        circular: null,
                        update: function(t, e) {
                            y.isRoot || (y.parent.node[y.key] = t), y.node = t, e && (p = !1);
                        },
                        delete: function(t) {
                            delete y.parent.node[y.key], t && (p = !1);
                        },
                        remove: function(t) {
                            i(y.parent.node) ? y.parent.node.splice(y.key, 1) : delete y.parent.node[y.key], t && (p = !1);
                        },
                        keys: null,
                        before: function(t) {
                            d.before = t;
                        },
                        after: function(t) {
                            d.after = t;
                        },
                        pre: function(t) {
                            d.pre = t;
                        },
                        post: function(t) {
                            d.post = t;
                        },
                        stop: function() {
                            u = !1;
                        },
                        block: function() {
                            p = !1;
                        }
                    };
                    if (!u) return y;
                    function E() {
                        if ("object" == typeof y.node && null !== y.node) {
                            y.keys && y.node_ === y.node || (y.keys = r(y.node)), y.isLeaf = 0 === y.keys.length;
                            for(var t = 0; t < c.length; t++)if (c[t].node_ === l) {
                                y.circular = c[t];
                                break;
                            }
                        } else y.isLeaf = !0, y.keys = null;
                        y.notLeaf = !y.isLeaf, y.notRoot = !y.isRoot;
                    }
                    E();
                    var g = e.call(y, y.node);
                    return void 0 !== g && y.update && y.update(g), d.before && d.before.call(y, y.node), p ? ("object" != typeof y.node || null === y.node || y.circular || (c.push(y), E(), n(y.keys, function(e, i) {
                        a.push(e), d.pre && d.pre.call(y, y.node[e], e);
                        var n = t(y.node[e]);
                        h && s.call(y.node, e) && (y.node[e] = n.node), n.isLast = i === y.keys.length - 1, n.isFirst = 0 === i, d.post && d.post.call(y, n), a.pop();
                    }), c.pop()), d.after && d.after.call(y, y.node), y) : y;
                }(t).node;
            }
            function a(t) {
                this.value = t;
            }
            function c(t) {
                return new a(t);
            }
            a.prototype.get = function(t) {
                for(var e = this.value, i = 0; i < t.length; i++){
                    var n = t[i];
                    if (!e || !s.call(e, n)) return;
                    e = e[n];
                }
                return e;
            }, a.prototype.has = function(t) {
                for(var e = this.value, i = 0; i < t.length; i++){
                    var n = t[i];
                    if (!e || !s.call(e, n)) return !1;
                    e = e[n];
                }
                return !0;
            }, a.prototype.set = function(t, e) {
                for(var i = this.value, n = 0; n < t.length - 1; n++){
                    var r = t[n];
                    s.call(i, r) || (i[r] = {}), i = i[r];
                }
                return i[t[n]] = e, e;
            }, a.prototype.map = function(t) {
                return h(this.value, t, !0);
            }, a.prototype.forEach = function(t) {
                return this.value = h(this.value, t, !1), this.value;
            }, a.prototype.reduce = function(t, e) {
                var i = 1 === arguments.length, n = i ? this.value : e;
                return this.forEach(function(e) {
                    this.isRoot && i || (n = t.call(this, n, e));
                }), n;
            }, a.prototype.paths = function() {
                var t = [];
                return this.forEach(function() {
                    t.push(this.path);
                }), t;
            }, a.prototype.nodes = function() {
                var t = [];
                return this.forEach(function() {
                    t.push(this.node);
                }), t;
            }, a.prototype.clone = function() {
                var t = [], e = [];
                return function i(s) {
                    for(var h = 0; h < t.length; h++)if (t[h] === s) return e[h];
                    if ("object" == typeof s && null !== s) {
                        var a = o(s);
                        return t.push(s), e.push(a), n(r(s), function(t) {
                            a[t] = i(s[t]);
                        }), t.pop(), e.pop(), a;
                    }
                    return s;
                }(this.value);
            }, n(r(a.prototype), function(t) {
                c[t] = function(e) {
                    var i = [].slice.call(arguments, 1), n = new a(e);
                    return n[t].apply(n, i);
                };
            }), t.exports = c;
        }
    }, e = {};
    function i(n) {
        var r = e[n];
        if (void 0 !== r) return r.exports;
        var s = e[n] = {
            exports: {}
        };
        return t[n].call(s.exports, s, s.exports, i), s.exports;
    }
    i.d = (t, e)=>{
        for(var n in e)i.o(e, n) && !i.o(t, n) && Object.defineProperty(t, n, {
            enumerable: !0,
            get: e[n]
        });
    }, i.g = function() {
        if ("object" == typeof globalThis) return globalThis;
        try {
            return this || new Function("return this")();
        } catch (t) {
            if ("object" == typeof window) return window;
        }
    }(), i.o = (t, e)=>Object.prototype.hasOwnProperty.call(t, e), i.r = (t)=>{
        "undefined" != typeof Symbol && Symbol.toStringTag && Object.defineProperty(t, Symbol.toStringTag, {
            value: "Module"
        }), Object.defineProperty(t, "__esModule", {
            value: !0
        });
    };
    var n = {};
    (()=>{
        "use strict";
        i.r(n), i.d(n, {
            SearchIndex: ()=>bt
        });
        var t = i(971), e = i(349);
        const r = ([t, e, i])=>Promise.resolve([
                t.match(i.tokenSplitRegex) || [],
                e,
                i
            ]), s = ([t, e, i])=>[
                i.skipFields.includes(e) ? [] : t,
                e,
                i
            ], o = ([t, e, i])=>Promise.resolve([
                t.map((t)=>i.caseSensitive ? t : t.toLowerCase()),
                e,
                i
            ]), h = ([t, e, i])=>{
            const { fields: n, values: r } = i.replace, s = ()=>t.reduce((t, e)=>[
                        e,
                        ...t,
                        ...r[e] || []
                    ], []);
            return r ? n ? n.includes(e) ? Promise.resolve([
                s(),
                e,
                i
            ]) : Promise.resolve([
                t,
                e,
                i
            ]) : Promise.resolve([
                s(),
                e,
                i
            ]) : Promise.resolve([
                t,
                e,
                i
            ]);
        }, a = ([t, i, n])=>{
            let { fields: r, lengths: s, join: o = " " } = n.ngrams;
            return r || (r = [
                i
            ]), s && r.includes(i) ? [
                (0, e.ngraminator)(t.filter((t)=>null !== t), s).map((t)=>t.join(o)),
                i,
                n
            ] : Promise.resolve([
                t,
                i,
                n
            ]);
        }, c = ([t, e, i])=>[
                t.filter((t)=>!i.stopwords.includes(t.toLowerCase())),
                e,
                i
            ], u = ([e, i, n])=>{
            const r = t(e), s = r.reduce((t, e)=>Math.max(e.positions.length, t), 0);
            return Promise.resolve([
                r.map((t)=>[
                        t.term[0],
                        (t.positions.length / s).toFixed(2)
                    ]).sort(),
                i,
                n
            ]);
        }, l = (t, e, i)=>r([
                t,
                e,
                i
            ]).then(s).then(o).then(h).then(a).then(c).then(u).then(([t, e, i])=>t);
        var f = i(708), d = i(483);
        function p(t, e) {
            const i = (t)=>"string" == typeof t, n = async (i, n = (t)=>new Promise((e)=>e(t)))=>new Promise(async (s, h)=>{
                    const a = (t)=>void 0 === t ? s(void 0) : t instanceof Promise ? s(t) : void 0;
                    try {
                        a(i), i = e.parse(i), i = await ((e)=>e.VALUE.GTE === e.VALUE.LTE && t.stopwords.includes(e.VALUE.GTE) ? void 0 : e)(i), a(i = await ((e)=>void 0 === e ? o(void 0) : e.VALUE.GTE === e.VALUE.LTE && t.queryReplace && t.queryReplace[e.VALUE.GTE] ? r(t.queryReplace[e.VALUE.GTE].map((t)=>({
                                    FIELD: e.FIELD,
                                    VALUE: {
                                        GTE: t,
                                        LTE: t
                                    }
                                }))).then((t)=>t.union) : e)(i)), a(i = await n(i));
                    } catch (t) {
                        return h(t);
                    }
                    return Array.isArray(i) ? s(i) : s(o(i));
                }), r = async (t, e)=>Promise.all(t.map((t)=>n(t, e))).then((t)=>{
                    const e = t.flat(1 / 0).reduce((t, e)=>(e && t.set(e._id, [
                            ...t.get(e._id) || [],
                            e._match
                        ]), t), new Map);
                    return {
                        sumTokensMinusStopwords: t.filter((t)=>t).length,
                        union: Array.from(e.keys()).map((t)=>({
                                _id: t,
                                _match: e.get(t)
                            }))
                    };
                }), s = (t, e, i)=>{
                const n = [];
                return void 0 === e && "number" != typeof e || n.push(e), i && n.push(d.HI), [
                    "IDX",
                    t,
                    n
                ];
            }, o = (t)=>new Promise((e)=>{
                    if (void 0 === t) return e(void 0);
                    const i = new Map;
                    Promise.all(t.FIELD.map((e)=>c({
                            gte: s(e, t.VALUE.GTE),
                            lte: s(e, t.VALUE.LTE, !0),
                            limit: t.LIMIT,
                            reverse: t.REVERSE
                        }).then((t)=>t.forEach((t)=>t.value.forEach((e)=>i.set(e, [
                                        ...i.get(e) || [],
                                        JSON.stringify({
                                            FIELD: t.key[1],
                                            VALUE: t.key[2][0],
                                            SCORE: t.key[2][1]
                                        })
                                    ])))))).then(()=>e(Array.from(i.keys()).map((t)=>({
                                _id: t,
                                _match: i.get(t)
                            }))));
                }), h = (t, e)=>e && 0 !== e.length ? (e = new Set(e.map((t)=>t._id)), t.map((t)=>Object.assign(t, {
                        _id: [
                            ...new Set([
                                ...t._id
                            ].filter((t)=>e.has(t)))
                        ]
                    }))) : t, a = (t)=>(t = e.parse(t), n(t).then((e)=>({
                        _id: [
                            ...e.reduce((t, e)=>t.add(e._id), new Set)
                        ].sort(),
                        VALUE: t.VALUE,
                        ...t
                    }))), c = (e)=>t.db.iterator(e).all().then((t)=>t.map(([t, e])=>({
                            key: t,
                            value: e
                        }))), u = (t, i)=>o({
                    ...e.parse(t),
                    LIMIT: 1,
                    REVERSE: i
                }).then((t)=>t.length ? JSON.parse(t.pop()._match.pop()).VALUE : null), l = (t)=>(t = e.parse(t), Promise.all(t.FIELD.map((e)=>{
                    let i = t.VALUE.LTE;
                    void 0 !== t.VALUE.LTE && "number" != typeof t.VALUE.LTE && (i += "￮");
                    let n = t.VALUE.GTE;
                    return t.VALUE.GTE && "number" != typeof t.VALUE.GTE && (n += " "), c({
                        gte: s(e, n),
                        lte: s(e, i, !0),
                        keys: !0,
                        values: !1
                    }).then((t)=>t.map(({ key: t })=>({
                                FIELD: t[1],
                                VALUE: t[2][0]
                            })));
                })).then((t)=>t.flat())), f = (t)=>(t = e.parse(t), Promise.all(t.FIELD.map((e)=>c({
                        gte: s(e, t.VALUE.GTE),
                        lte: s(e, t.VALUE.LTE, !0)
                    }).then((t)=>t.map((t)=>({
                                FIELD: t.key[1],
                                VALUE: t.key[2][0],
                                _id: t.value
                            }))))).then((t)=>t.flat())), p = new Intl.Collator("en", {
                numeric: !0,
                sensitivity: "base"
            });
            return {
                AGGREGATE: ({ BUCKETS: t, FACETS: e, QUERY: i })=>Promise.all([
                        t,
                        e,
                        i
                    ]).then(([t = [], e = [], i = []])=>({
                            BUCKETS: h(t.flat(), i),
                            FACETS: h(e.flat(), i),
                            RESULT: i
                        })),
                AGGREGATION_FILTER: h,
                BUCKET: a,
                BUCKETS: (...t)=>Promise.all(t.map(a)),
                CREATED: ()=>t.db.get([
                        "~CREATED"
                    ]),
                DISTINCT: (...t)=>Promise.all(t.length ? t.map(l) : [
                        l({})
                    ]).then((t)=>[
                            ...t.flat().reduce((t, e)=>t.add(JSON.stringify(e)), new Set)
                        ].map(JSON.parse)),
                EXIST: (...e)=>Promise.all(e.map((e)=>t.db.get([
                            t.docExistsSpace,
                            e
                        ]).catch((t)=>null))).then((t)=>t.reduce((t, i, n)=>(null != i && t.push(e[n]), t), [])),
                EXPORT: c,
                FACETS: (...t)=>Promise.all(t.length ? t.map(f) : [
                        f({})
                    ]).then((t)=>[
                            ...t.flat().reduce((t, e)=>t.add(JSON.stringify(e)), new Set)
                        ].map(JSON.parse)),
                FIELDS: ()=>c({
                        gte: [
                            "FIELD",
                            d.LO
                        ],
                        lte: [
                            "FIELD",
                            d.HI
                        ]
                    }).then((t)=>t.map((t)=>t.key[1])),
                GET: n,
                INTERSECTION: (t, e)=>r(t, e).then((t)=>t.union.filter((e)=>e._match.length === t.sumTokensMinusStopwords)),
                LAST_UPDATED: ()=>t.db.get([
                        "~LAST_UPDATED"
                    ]),
                MAX: (t)=>u(t, !0),
                MIN: u,
                OBJECT: (e)=>Promise.all(e.map((e)=>t.db.get([
                            "DOC",
                            e._id
                        ]).catch((t)=>null))).then((t)=>e.map((e, i)=>(e._object = t[i], e))),
                SET_SUBTRACTION: (t, e)=>Promise.all([
                        i(t) ? n(t) : t,
                        i(e) ? n(e) : e
                    ]).then(([t, e])=>t.filter((t)=>-1 === e.map((t)=>t._id).indexOf(t._id))),
                SORT: (t)=>new Promise((e)=>e(t.sort((t, e)=>p.compare(t._id, e._id)))),
                UNION: r
            };
        }
        d.LO = null, d.HI = void 0;
        var y = i(692);
        const E = null, g = void 0;
        class m {
            availableFields = [];
            #t;
            constructor(t){
                this.#t = t;
            }
            setAvailableFields = (t)=>{
                this.availableFields = t;
            };
            #e = (t)=>{
                const e = (t)=>this.#t || "string" != typeof t ? t : t.toLowerCase();
                return {
                    FIELD: t.FIELD.map(e),
                    VALUE: {
                        GTE: e(t.VALUE.GTE),
                        LTE: e(t.VALUE.LTE)
                    }
                };
            };
            parse(t) {
                if (Array.isArray(t)) throw new Error("token cannot be Array");
                if (void 0 === t && (t = {}), "string" == typeof t) {
                    if (-1 === t.indexOf(":")) return this.#e({
                        FIELD: this.availableFields,
                        VALUE: {
                            GTE: t,
                            LTE: t
                        }
                    });
                    const [e, ...i] = t.split(":");
                    return this.#e({
                        FIELD: [
                            e
                        ],
                        VALUE: {
                            GTE: i.join(":"),
                            LTE: i.join(":")
                        }
                    });
                }
                return "number" == typeof t && (t = {
                    VALUE: {
                        GTE: t,
                        LTE: t
                    }
                }), null === t.VALUE && (t.VALUE = {
                    GTE: null,
                    LTE: null
                }), "string" != typeof t.VALUE && "number" != typeof t.VALUE || (t.VALUE = {
                    GTE: t.VALUE,
                    LTE: t.VALUE
                }), void 0 !== t.VALUE && Object.keys(t.VALUE).length || (t.VALUE = {
                    GTE: E,
                    LTE: g
                }), void 0 === t.VALUE.GTE && (t.VALUE.GTE = E), void 0 === t.VALUE.LTE && (t.VALUE.LTE = g), t.VALUE = Object.assign(t.VALUE, {
                    GTE: t.VALUE.GTE,
                    LTE: t.VALUE.LTE
                }), void 0 === t.FIELD ? this.#e({
                    FIELD: this.availableFields,
                    ...t
                }) : (t.FIELD = [
                    t.FIELD
                ].flat(), this.#e(t));
            }
        }
        class v {
            constructor(t = {}){
                t = {
                    caseSensitive: !0,
                    isLeaf: (t)=>"string" == typeof t || "number" == typeof t,
                    stopwords: [],
                    doNotIndexField: [],
                    storeVectors: !0,
                    docExistsSpace: "DOC",
                    db: new t.Level(t.name, {
                        keyEncoding: d,
                        valueEncoding: "json"
                    }),
                    ...t
                };
                const e = new m(t.caseSensitive), i = p(t, e), n = function(t, e) {
                    let i = 0;
                    const n = (e, i)=>{
                        if (null == e._object) return {
                            _id: e._id,
                            keys: []
                        };
                        const n = [];
                        return y(e._object).forEach(function(e) {
                            const r = this.path.filter((t)=>!Number.isInteger(+t)).join(".");
                            if ("_id" !== r && !i.doNotIndexField.filter((t)=>r.startsWith(t)).length && t.isLeaf(this.node)) {
                                if (!t.stopwords.includes(this.node)) {
                                    const e = JSON.stringify([
                                        r,
                                        [
                                            this.node
                                        ].flat(1 / 0).map((e)=>"string" != typeof e || t.caseSensitive ? e : e.toLowerCase())
                                    ]);
                                    n.push(t.caseSensitive ? e : e.toLowerCase());
                                }
                                this.update(this.node, !0);
                            }
                        }), {
                            _id: e._id,
                            keys: n
                        };
                    }, r = (t, e)=>(e.keys.forEach((i)=>{
                            t[i] = t[i] || [], t[i].push(e._id);
                        }), t), s = (e, s, o, h, a)=>new Promise((c)=>{
                            e = e.map((t)=>{
                                var e;
                                return t._id = void 0 === (e = t._id) ? ++i : "string" == typeof e || "number" == typeof e ? e : void 0, t._object && (t._object._id = t._id), t;
                            }), a = Object.assign(t, a), p(t).EXIST(...e.map((t)=>t._id)).then((t)=>((t, e, i)=>{
                                    const n = Object.keys(t);
                                    return Promise.all(n.map((t)=>[
                                            "IDX",
                                            ...JSON.parse(t)
                                        ]).map((t)=>new Promise((i, n)=>e.get(t).then(i).catch((t)=>i([]))))).then((e)=>e.map((e, r)=>{
                                            const s = new Set(e), o = new Set(t[n[r]]);
                                            if ("put" === i) return {
                                                key: [
                                                    "IDX",
                                                    ...JSON.parse(n[r])
                                                ],
                                                type: i,
                                                value: [
                                                    ...new Set([
                                                        ...s,
                                                        ...o
                                                    ])
                                                ].sort()
                                            };
                                            if ("del" === i) {
                                                const t = [
                                                    ...new Set([
                                                        ...s
                                                    ].filter((t)=>!o.has(t)))
                                                ];
                                                return {
                                                    key: [
                                                        "IDX",
                                                        ...JSON.parse(n[r])
                                                    ],
                                                    type: 0 === t.length ? "del" : "put",
                                                    value: t
                                                };
                                            }
                                            return e;
                                        }));
                                })(((t, e)=>t.map((t)=>n(t, e)).reduce(r, {}))(e, a), s, o).then((i)=>s.batch(i.concat(a.storeVectors ? ((t, e)=>t.map((t)=>({
                                                key: [
                                                    "DOC",
                                                    t._id
                                                ],
                                                type: e,
                                                value: t._object
                                            })))(e, o) : []).concat(((t)=>[
                                            ...new Set(t.map((t)=>t.key[1].split(":")[0]))
                                        ].map((t)=>({
                                                type: "put",
                                                key: [
                                                    "FIELD",
                                                    t
                                                ],
                                                value: t
                                            })))(i)), (i)=>c(e.map((e)=>{
                                            let i;
                                            return "put" === o ? i = t.includes(e._id) ? "UPDATED" : "CREATED" : "del" === o && (i = null === e._object ? "FAILED" : "DELETED"), {
                                                _id: e._id,
                                                operation: h,
                                                status: i
                                            };
                                        })))));
                        }), o = (e)=>t.db.put([
                            "~LAST_UPDATED"
                        ], Date.now()).then(()=>e);
                    return {
                        DELETE: (e)=>p(t).OBJECT(e.map((t)=>({
                                    _id: t
                                }))).then((e)=>s(e, t.db, "del", "DELETE", {})).then(o),
                        IMPORT: (i)=>t.db.clear().then(()=>t.db.batch(i.map((t)=>Object.assign(t, {
                                        type: "put"
                                    })))).then(()=>p(t).FIELDS()).then((t)=>e.setAvailableFields(t)),
                        PUT: (i, n = {})=>s(i.map((t)=>({
                                    _id: t._id,
                                    _object: t
                                })), t.db, "put", "PUT", n).then(o).then(async (i)=>(e.setAvailableFields(await p(t).FIELDS()), i)),
                        TIMESTAMP: ()=>t.db.get([
                                "~CREATED"
                            ]).then().catch((e)=>t.db.put([
                                    "~CREATED"
                                ], Date.now()).then(o)),
                        TIMESTAMP_LAST_UPDATED: o
                    };
                }(t, e);
                n.TIMESTAMP(), this.AGGREGATION_FILTER = i.AGGREGATION_FILTER, this.AND = (t, e)=>i.INTERSECTION(t, e).then(this.flattenMatchArrayInResults), this.BUCKET = i.BUCKET, this.BUCKETS = i.BUCKETS, this.CREATED = i.CREATED, this.DELETE = n.DELETE, this.DISTINCT = i.DISTINCT, this.EXIST = i.EXIST, this.EXPORT = i.EXPORT, this.FACETS = i.FACETS, this.FIELDS = i.FIELDS, this.GET = (t, e)=>i.GET(t, e).then(this.flattenMatchArrayInResults), this.IMPORT = n.IMPORT, this.LAST_UPDATED = i.LAST_UPDATED, this.MAX = i.MAX, this.MIN = i.MIN, this.NOT = (...t)=>i.SET_SUBTRACTION(...t).then(this.flattenMatchArrayInResults), this.OBJECT = i.OBJECT, this.OR = (t, e)=>i.UNION(t, e).then((t)=>t.union).then(this.flattenMatchArrayInResults), this.PUT = n.PUT, this.SORT = i.SORT, this.STORE = t.db, this.TIMESTAMP_LAST_UPDATED = n.TIMESTAMP_LAST_UPDATED, this.TOKEN_PARSER = e;
            }
            flattenMatchArrayInResults(t) {
                return void 0 === t ? void 0 : t.map((t)=>(t._match = t._match.flat(1 / 0).map((t)=>"string" == typeof t ? JSON.parse(t) : t).sort((t, e)=>t.FIELD < e.FIELD ? -1 : t.FIELD > e.FIELD ? 1 : t.VALUE < e.VALUE ? -1 : t.VALUE > e.VALUE ? 1 : t.SCORE < e.SCORE ? -1 : t.SCORE > e.SCORE ? 1 : 0), t));
            }
        }
        class b {
            constructor(t = {}){
                return new v({
                    Level: f.v,
                    ...t
                });
            }
        }
        var T = i(155);
        const w = "object" == typeof performance && performance && "function" == typeof performance.now ? performance : Date, _ = new Set, S = "object" == typeof T && T ? T : {}, L = (t, e, i, n)=>{
            "function" == typeof S.emitWarning ? S.emitWarning(t, e, i, n) : console.error(`[${i}] ${e}: ${t}`);
        };
        let A = globalThis.AbortController, O = globalThis.AbortSignal;
        if (void 0 === A) {
            O = class {
                onabort;
                _onabort = [];
                reason;
                aborted = !1;
                addEventListener(t, e) {
                    this._onabort.push(e);
                }
            }, A = class {
                constructor(){
                    e();
                }
                signal = new O;
                abort(t) {
                    if (!this.signal.aborted) {
                        this.signal.reason = t, this.signal.aborted = !0;
                        for (const e of this.signal._onabort)e(t);
                        this.signal.onabort?.(t);
                    }
                }
            };
            let t = "1" !== S.env?.LRU_CACHE_IGNORE_AC_WARNING;
            const e = ()=>{
                t && (t = !1, L("AbortController is not defined. If using lru-cache in node 14, load an AbortController polyfill from the `node-abort-controller` package. A minimal polyfill is provided for use by LRUCache.fetch(), but it should not be relied upon in other contexts (eg, passing it to other APIs that use AbortController/AbortSignal might have undesirable effects). You may disable this with LRU_CACHE_IGNORE_AC_WARNING=1 in the env.", "NO_ABORT_CONTROLLER", "ENOTSUP", e));
            };
        }
        Symbol("type");
        const I = (t)=>t && t === Math.floor(t) && t > 0 && isFinite(t), x = (t)=>I(t) ? t <= Math.pow(2, 8) ? Uint8Array : t <= Math.pow(2, 16) ? Uint16Array : t <= Math.pow(2, 32) ? Uint32Array : t <= Number.MAX_SAFE_INTEGER ? k : null : null;
        class k extends Array {
            constructor(t){
                super(t), this.fill(0);
            }
        }
        class C {
            heap;
            length;
            static #i = !1;
            static create(t) {
                const e = x(t);
                if (!e) return [];
                C.#i = !0;
                const i = new C(t, e);
                return C.#i = !1, i;
            }
            constructor(t, e){
                if (!C.#i) throw new TypeError("instantiate Stack using Stack.create(n)");
                this.heap = new e(t), this.length = 0;
            }
            push(t) {
                this.heap[this.length++] = t;
            }
            pop() {
                return this.heap[--this.length];
            }
        }
        class D {
            #n;
            #r;
            #s;
            #o;
            #h;
            ttl;
            ttlResolution;
            ttlAutopurge;
            updateAgeOnGet;
            updateAgeOnHas;
            allowStale;
            noDisposeOnSet;
            noUpdateTTL;
            maxEntrySize;
            sizeCalculation;
            noDeleteOnFetchRejection;
            noDeleteOnStaleGet;
            allowStaleOnFetchAbort;
            allowStaleOnFetchRejection;
            ignoreFetchAbort;
            #a;
            #c;
            #u;
            #l;
            #f;
            #d;
            #p;
            #y;
            #E;
            #g;
            #m;
            #v;
            #b;
            #T;
            #w;
            #_;
            #S;
            static unsafeExposeInternals(t) {
                return {
                    starts: t.#b,
                    ttls: t.#T,
                    sizes: t.#v,
                    keyMap: t.#u,
                    keyList: t.#l,
                    valList: t.#f,
                    next: t.#d,
                    prev: t.#p,
                    get head () {
                        return t.#y;
                    },
                    get tail () {
                        return t.#E;
                    },
                    free: t.#g,
                    isBackgroundFetch: (e)=>t.#L(e),
                    backgroundFetch: (e, i, n, r)=>t.#A(e, i, n, r),
                    moveToTail: (e)=>t.#O(e),
                    indexes: (e)=>t.#I(e),
                    rindexes: (e)=>t.#x(e),
                    isStale: (e)=>t.#k(e)
                };
            }
            get max() {
                return this.#n;
            }
            get maxSize() {
                return this.#r;
            }
            get calculatedSize() {
                return this.#c;
            }
            get size() {
                return this.#a;
            }
            get fetchMethod() {
                return this.#h;
            }
            get dispose() {
                return this.#s;
            }
            get disposeAfter() {
                return this.#o;
            }
            constructor(t){
                const { max: e = 0, ttl: i, ttlResolution: n = 1, ttlAutopurge: r, updateAgeOnGet: s, updateAgeOnHas: o, allowStale: h, dispose: a, disposeAfter: c, noDisposeOnSet: u, noUpdateTTL: l, maxSize: f = 0, maxEntrySize: d = 0, sizeCalculation: p, fetchMethod: y, noDeleteOnFetchRejection: E, noDeleteOnStaleGet: g, allowStaleOnFetchRejection: m, allowStaleOnFetchAbort: v, ignoreFetchAbort: b } = t;
                if (0 !== e && !I(e)) throw new TypeError("max option must be a nonnegative integer");
                const T = e ? x(e) : Array;
                if (!T) throw new Error("invalid max value: " + e);
                if (this.#n = e, this.#r = f, this.maxEntrySize = d || this.#r, this.sizeCalculation = p, this.sizeCalculation) {
                    if (!this.#r && !this.maxEntrySize) throw new TypeError("cannot set sizeCalculation without setting maxSize or maxEntrySize");
                    if ("function" != typeof this.sizeCalculation) throw new TypeError("sizeCalculation set to non-function");
                }
                if (void 0 !== y && "function" != typeof y) throw new TypeError("fetchMethod must be a function if specified");
                if (this.#h = y, this.#_ = !!y, this.#u = new Map, this.#l = new Array(e).fill(void 0), this.#f = new Array(e).fill(void 0), this.#d = new T(e), this.#p = new T(e), this.#y = 0, this.#E = 0, this.#g = C.create(e), this.#a = 0, this.#c = 0, "function" == typeof a && (this.#s = a), "function" == typeof c ? (this.#o = c, this.#m = []) : (this.#o = void 0, this.#m = void 0), this.#w = !!this.#s, this.#S = !!this.#o, this.noDisposeOnSet = !!u, this.noUpdateTTL = !!l, this.noDeleteOnFetchRejection = !!E, this.allowStaleOnFetchRejection = !!m, this.allowStaleOnFetchAbort = !!v, this.ignoreFetchAbort = !!b, 0 !== this.maxEntrySize) {
                    if (0 !== this.#r && !I(this.#r)) throw new TypeError("maxSize must be a positive integer if specified");
                    if (!I(this.maxEntrySize)) throw new TypeError("maxEntrySize must be a positive integer if specified");
                    this.#C();
                }
                if (this.allowStale = !!h, this.noDeleteOnStaleGet = !!g, this.updateAgeOnGet = !!s, this.updateAgeOnHas = !!o, this.ttlResolution = I(n) || 0 === n ? n : 1, this.ttlAutopurge = !!r, this.ttl = i || 0, this.ttl) {
                    if (!I(this.ttl)) throw new TypeError("ttl must be a positive integer if specified");
                    this.#D();
                }
                if (0 === this.#n && 0 === this.ttl && 0 === this.#r) throw new TypeError("At least one of max, maxSize, or ttl is required");
                if (!this.ttlAutopurge && !this.#n && !this.#r) {
                    const t = "LRU_CACHE_UNBOUNDED";
                    ((t)=>!_.has(t))(t) && (_.add(t), L("TTL caching without ttlAutopurge, max, or maxSize can result in unbounded memory consumption.", "UnboundedCacheWarning", t, D));
                }
            }
            getRemainingTTL(t) {
                return this.#u.has(t) ? 1 / 0 : 0;
            }
            #D() {
                const t = new k(this.#n), e = new k(this.#n);
                this.#T = t, this.#b = e, this.#R = (i, n, r = w.now())=>{
                    if (e[i] = 0 !== n ? r : 0, t[i] = n, 0 !== n && this.ttlAutopurge) {
                        const t = setTimeout(()=>{
                            this.#k(i) && this.delete(this.#l[i]);
                        }, n + 1);
                        t.unref && t.unref();
                    }
                }, this.#N = (i)=>{
                    e[i] = 0 !== t[i] ? w.now() : 0;
                }, this.#U = (r, s)=>{
                    if (t[s]) {
                        const o = t[s], h = e[s];
                        r.ttl = o, r.start = h, r.now = i || n();
                        const a = r.now - h;
                        r.remainingTTL = o - a;
                    }
                };
                let i = 0;
                const n = ()=>{
                    const t = w.now();
                    if (this.ttlResolution > 0) {
                        i = t;
                        const e = setTimeout(()=>i = 0, this.ttlResolution);
                        e.unref && e.unref();
                    }
                    return t;
                };
                this.getRemainingTTL = (r)=>{
                    const s = this.#u.get(r);
                    if (void 0 === s) return 0;
                    const o = t[s], h = e[s];
                    return 0 === o || 0 === h ? 1 / 0 : o - ((i || n()) - h);
                }, this.#k = (r)=>0 !== t[r] && 0 !== e[r] && (i || n()) - e[r] > t[r];
            }
            #N = ()=>{};
            #U = ()=>{};
            #R = ()=>{};
            #k = ()=>!1;
            #C() {
                const t = new k(this.#n);
                this.#c = 0, this.#v = t, this.#B = (e)=>{
                    this.#c -= t[e], t[e] = 0;
                }, this.#F = (t, e, i, n)=>{
                    if (this.#L(e)) return 0;
                    if (!I(i)) {
                        if (!n) throw new TypeError("invalid size value (must be positive integer). When maxSize or maxEntrySize is used, sizeCalculation or size must be set.");
                        if ("function" != typeof n) throw new TypeError("sizeCalculation must be a function");
                        if (i = n(e, t), !I(i)) throw new TypeError("sizeCalculation return invalid (expect positive integer)");
                    }
                    return i;
                }, this.#P = (e, i, n)=>{
                    if (t[e] = i, this.#r) {
                        const i = this.#r - t[e];
                        for(; this.#c > i;)this.#M(!0);
                    }
                    this.#c += t[e], n && (n.entrySize = i, n.totalCalculatedSize = this.#c);
                };
            }
            #B = (t)=>{};
            #P = (t, e, i)=>{};
            #F = (t, e, i, n)=>{
                if (i || n) throw new TypeError("cannot set size without setting maxSize or maxEntrySize on cache");
                return 0;
            };
            *#I({ allowStale: t = this.allowStale } = {}) {
                if (this.#a) for(let e = this.#E; this.#j(e) && (!t && this.#k(e) || (yield e), e !== this.#y);)e = this.#p[e];
            }
            *#x({ allowStale: t = this.allowStale } = {}) {
                if (this.#a) for(let e = this.#y; this.#j(e) && (!t && this.#k(e) || (yield e), e !== this.#E);)e = this.#d[e];
            }
            #j(t) {
                return void 0 !== t && this.#u.get(this.#l[t]) === t;
            }
            *entries() {
                for (const t of this.#I())void 0 === this.#f[t] || void 0 === this.#l[t] || this.#L(this.#f[t]) || (yield [
                    this.#l[t],
                    this.#f[t]
                ]);
            }
            *rentries() {
                for (const t of this.#x())void 0 === this.#f[t] || void 0 === this.#l[t] || this.#L(this.#f[t]) || (yield [
                    this.#l[t],
                    this.#f[t]
                ]);
            }
            *keys() {
                for (const t of this.#I()){
                    const e = this.#l[t];
                    void 0 === e || this.#L(this.#f[t]) || (yield e);
                }
            }
            *rkeys() {
                for (const t of this.#x()){
                    const e = this.#l[t];
                    void 0 === e || this.#L(this.#f[t]) || (yield e);
                }
            }
            *values() {
                for (const t of this.#I())void 0 === this.#f[t] || this.#L(this.#f[t]) || (yield this.#f[t]);
            }
            *rvalues() {
                for (const t of this.#x())void 0 === this.#f[t] || this.#L(this.#f[t]) || (yield this.#f[t]);
            }
            [Symbol.iterator]() {
                return this.entries();
            }
            find(t, e = {}) {
                for (const i of this.#I()){
                    const n = this.#f[i], r = this.#L(n) ? n.__staleWhileFetching : n;
                    if (void 0 !== r && t(r, this.#l[i], this)) return this.get(this.#l[i], e);
                }
            }
            forEach(t, e = this) {
                for (const i of this.#I()){
                    const n = this.#f[i], r = this.#L(n) ? n.__staleWhileFetching : n;
                    void 0 !== r && t.call(e, r, this.#l[i], this);
                }
            }
            rforEach(t, e = this) {
                for (const i of this.#x()){
                    const n = this.#f[i], r = this.#L(n) ? n.__staleWhileFetching : n;
                    void 0 !== r && t.call(e, r, this.#l[i], this);
                }
            }
            purgeStale() {
                let t = !1;
                for (const e of this.#x({
                    allowStale: !0
                }))this.#k(e) && (this.delete(this.#l[e]), t = !0);
                return t;
            }
            dump() {
                const t = [];
                for (const e of this.#I({
                    allowStale: !0
                })){
                    const i = this.#l[e], n = this.#f[e], r = this.#L(n) ? n.__staleWhileFetching : n;
                    if (void 0 === r || void 0 === i) continue;
                    const s = {
                        value: r
                    };
                    if (this.#T && this.#b) {
                        s.ttl = this.#T[e];
                        const t = w.now() - this.#b[e];
                        s.start = Math.floor(Date.now() - t);
                    }
                    this.#v && (s.size = this.#v[e]), t.unshift([
                        i,
                        s
                    ]);
                }
                return t;
            }
            load(t) {
                this.clear();
                for (const [e, i] of t){
                    if (i.start) {
                        const t = Date.now() - i.start;
                        i.start = w.now() - t;
                    }
                    this.set(e, i.value, i);
                }
            }
            set(t, e, i = {}) {
                if (void 0 === e) return this.delete(t), this;
                const { ttl: n = this.ttl, start: r, noDisposeOnSet: s = this.noDisposeOnSet, sizeCalculation: o = this.sizeCalculation, status: h } = i;
                let { noUpdateTTL: a = this.noUpdateTTL } = i;
                const c = this.#F(t, e, i.size || 0, o);
                if (this.maxEntrySize && c > this.maxEntrySize) return h && (h.set = "miss", h.maxEntrySizeExceeded = !0), this.delete(t), this;
                let u = 0 === this.#a ? void 0 : this.#u.get(t);
                if (void 0 === u) u = 0 === this.#a ? this.#E : 0 !== this.#g.length ? this.#g.pop() : this.#a === this.#n ? this.#M(!1) : this.#a, this.#l[u] = t, this.#f[u] = e, this.#u.set(t, u), this.#d[this.#E] = u, this.#p[u] = this.#E, this.#E = u, this.#a++, this.#P(u, c, h), h && (h.set = "add"), a = !1;
                else {
                    this.#O(u);
                    const i = this.#f[u];
                    if (e !== i) {
                        if (this.#_ && this.#L(i) ? i.__abortController.abort(new Error("replaced")) : s || (this.#w && this.#s?.(i, t, "set"), this.#S && this.#m?.push([
                            i,
                            t,
                            "set"
                        ])), this.#B(u), this.#P(u, c, h), this.#f[u] = e, h) {
                            h.set = "replace";
                            const t = i && this.#L(i) ? i.__staleWhileFetching : i;
                            void 0 !== t && (h.oldValue = t);
                        }
                    } else h && (h.set = "update");
                }
                if (0 === n || this.#T || this.#D(), this.#T && (a || this.#R(u, n, r), h && this.#U(h, u)), !s && this.#S && this.#m) {
                    const t = this.#m;
                    let e;
                    for(; e = t?.shift();)this.#o?.(...e);
                }
                return this;
            }
            pop() {
                try {
                    for(; this.#a;){
                        const t = this.#f[this.#y];
                        if (this.#M(!0), this.#L(t)) {
                            if (t.__staleWhileFetching) return t.__staleWhileFetching;
                        } else if (void 0 !== t) return t;
                    }
                } finally{
                    if (this.#S && this.#m) {
                        const t = this.#m;
                        let e;
                        for(; e = t?.shift();)this.#o?.(...e);
                    }
                }
            }
            #M(t) {
                const e = this.#y, i = this.#l[e], n = this.#f[e];
                return this.#_ && this.#L(n) ? n.__abortController.abort(new Error("evicted")) : (this.#w || this.#S) && (this.#w && this.#s?.(n, i, "evict"), this.#S && this.#m?.push([
                    n,
                    i,
                    "evict"
                ])), this.#B(e), t && (this.#l[e] = void 0, this.#f[e] = void 0, this.#g.push(e)), 1 === this.#a ? (this.#y = this.#E = 0, this.#g.length = 0) : this.#y = this.#d[e], this.#u.delete(i), this.#a--, e;
            }
            has(t, e = {}) {
                const { updateAgeOnHas: i = this.updateAgeOnHas, status: n } = e, r = this.#u.get(t);
                if (void 0 !== r) {
                    const t = this.#f[r];
                    if (this.#L(t) && void 0 === t.__staleWhileFetching) return !1;
                    if (!this.#k(r)) return i && this.#N(r), n && (n.has = "hit", this.#U(n, r)), !0;
                    n && (n.has = "stale", this.#U(n, r));
                } else n && (n.has = "miss");
                return !1;
            }
            peek(t, e = {}) {
                const { allowStale: i = this.allowStale } = e, n = this.#u.get(t);
                if (void 0 !== n && (i || !this.#k(n))) {
                    const t = this.#f[n];
                    return this.#L(t) ? t.__staleWhileFetching : t;
                }
            }
            #A(t, e, i, n) {
                const r = void 0 === e ? void 0 : this.#f[e];
                if (this.#L(r)) return r;
                const s = new A, { signal: o } = i;
                o?.addEventListener("abort", ()=>s.abort(o.reason), {
                    signal: s.signal
                });
                const h = {
                    signal: s.signal,
                    options: i,
                    context: n
                }, a = (n, r = !1)=>{
                    const { aborted: o } = s.signal, a = i.ignoreFetchAbort && void 0 !== n;
                    if (i.status && (o && !r ? (i.status.fetchAborted = !0, i.status.fetchError = s.signal.reason, a && (i.status.fetchAbortIgnored = !0)) : i.status.fetchResolved = !0), o && !a && !r) return c(s.signal.reason);
                    const l = u;
                    return this.#f[e] === u && (void 0 === n ? l.__staleWhileFetching ? this.#f[e] = l.__staleWhileFetching : this.delete(t) : (i.status && (i.status.fetchUpdated = !0), this.set(t, n, h.options))), n;
                }, c = (n)=>{
                    const { aborted: r } = s.signal, o = r && i.allowStaleOnFetchAbort, h = o || i.allowStaleOnFetchRejection, a = h || i.noDeleteOnFetchRejection, c = u;
                    if (this.#f[e] === u && (a && void 0 !== c.__staleWhileFetching ? o || (this.#f[e] = c.__staleWhileFetching) : this.delete(t)), h) return i.status && void 0 !== c.__staleWhileFetching && (i.status.returnedStale = !0), c.__staleWhileFetching;
                    if (c.__returned === c) throw n;
                };
                i.status && (i.status.fetchDispatched = !0);
                const u = new Promise((e, n)=>{
                    const o = this.#h?.(t, r, h);
                    o && o instanceof Promise && o.then((t)=>e(void 0 === t ? void 0 : t), n), s.signal.addEventListener("abort", ()=>{
                        i.ignoreFetchAbort && !i.allowStaleOnFetchAbort || (e(void 0), i.allowStaleOnFetchAbort && (e = (t)=>a(t, !0)));
                    });
                }).then(a, (t)=>(i.status && (i.status.fetchRejected = !0, i.status.fetchError = t), c(t))), l = Object.assign(u, {
                    __abortController: s,
                    __staleWhileFetching: r,
                    __returned: void 0
                });
                return void 0 === e ? (this.set(t, l, {
                    ...h.options,
                    status: void 0
                }), e = this.#u.get(t)) : this.#f[e] = l, l;
            }
            #L(t) {
                if (!this.#_) return !1;
                const e = t;
                return !!e && e instanceof Promise && e.hasOwnProperty("__staleWhileFetching") && e.__abortController instanceof A;
            }
            async fetch(t, e = {}) {
                const { allowStale: i = this.allowStale, updateAgeOnGet: n = this.updateAgeOnGet, noDeleteOnStaleGet: r = this.noDeleteOnStaleGet, ttl: s = this.ttl, noDisposeOnSet: o = this.noDisposeOnSet, size: h = 0, sizeCalculation: a = this.sizeCalculation, noUpdateTTL: c = this.noUpdateTTL, noDeleteOnFetchRejection: u = this.noDeleteOnFetchRejection, allowStaleOnFetchRejection: l = this.allowStaleOnFetchRejection, ignoreFetchAbort: f = this.ignoreFetchAbort, allowStaleOnFetchAbort: d = this.allowStaleOnFetchAbort, context: p, forceRefresh: y = !1, status: E, signal: g } = e;
                if (!this.#_) return E && (E.fetch = "get"), this.get(t, {
                    allowStale: i,
                    updateAgeOnGet: n,
                    noDeleteOnStaleGet: r,
                    status: E
                });
                const m = {
                    allowStale: i,
                    updateAgeOnGet: n,
                    noDeleteOnStaleGet: r,
                    ttl: s,
                    noDisposeOnSet: o,
                    size: h,
                    sizeCalculation: a,
                    noUpdateTTL: c,
                    noDeleteOnFetchRejection: u,
                    allowStaleOnFetchRejection: l,
                    allowStaleOnFetchAbort: d,
                    ignoreFetchAbort: f,
                    status: E,
                    signal: g
                };
                let v = this.#u.get(t);
                if (void 0 === v) {
                    E && (E.fetch = "miss");
                    const e = this.#A(t, v, m, p);
                    return e.__returned = e;
                }
                {
                    const e = this.#f[v];
                    if (this.#L(e)) {
                        const t = i && void 0 !== e.__staleWhileFetching;
                        return E && (E.fetch = "inflight", t && (E.returnedStale = !0)), t ? e.__staleWhileFetching : e.__returned = e;
                    }
                    const r = this.#k(v);
                    if (!y && !r) return E && (E.fetch = "hit"), this.#O(v), n && this.#N(v), E && this.#U(E, v), e;
                    const s = this.#A(t, v, m, p), o = void 0 !== s.__staleWhileFetching && i;
                    return E && (E.fetch = r ? "stale" : "refresh", o && r && (E.returnedStale = !0)), o ? s.__staleWhileFetching : s.__returned = s;
                }
            }
            get(t, e = {}) {
                const { allowStale: i = this.allowStale, updateAgeOnGet: n = this.updateAgeOnGet, noDeleteOnStaleGet: r = this.noDeleteOnStaleGet, status: s } = e, o = this.#u.get(t);
                if (void 0 !== o) {
                    const e = this.#f[o], h = this.#L(e);
                    return s && this.#U(s, o), this.#k(o) ? (s && (s.get = "stale"), h ? (s && i && void 0 !== e.__staleWhileFetching && (s.returnedStale = !0), i ? e.__staleWhileFetching : void 0) : (r || this.delete(t), s && i && (s.returnedStale = !0), i ? e : void 0)) : (s && (s.get = "hit"), h ? e.__staleWhileFetching : (this.#O(o), n && this.#N(o), e));
                }
                s && (s.get = "miss");
            }
            #V(t, e) {
                this.#p[e] = t, this.#d[t] = e;
            }
            #O(t) {
                t !== this.#E && (t === this.#y ? this.#y = this.#d[t] : this.#V(this.#p[t], this.#d[t]), this.#V(this.#E, t), this.#E = t);
            }
            delete(t) {
                let e = !1;
                if (0 !== this.#a) {
                    const i = this.#u.get(t);
                    if (void 0 !== i) {
                        if (e = !0, 1 === this.#a) this.clear();
                        else {
                            this.#B(i);
                            const e = this.#f[i];
                            this.#L(e) ? e.__abortController.abort(new Error("deleted")) : (this.#w || this.#S) && (this.#w && this.#s?.(e, t, "delete"), this.#S && this.#m?.push([
                                e,
                                t,
                                "delete"
                            ])), this.#u.delete(t), this.#l[i] = void 0, this.#f[i] = void 0, i === this.#E ? this.#E = this.#p[i] : i === this.#y ? this.#y = this.#d[i] : (this.#d[this.#p[i]] = this.#d[i], this.#p[this.#d[i]] = this.#p[i]), this.#a--, this.#g.push(i);
                        }
                    }
                }
                if (this.#S && this.#m?.length) {
                    const t = this.#m;
                    let e;
                    for(; e = t?.shift();)this.#o?.(...e);
                }
                return e;
            }
            clear() {
                for (const t of this.#x({
                    allowStale: !0
                })){
                    const e = this.#f[t];
                    if (this.#L(e)) e.__abortController.abort(new Error("deleted"));
                    else {
                        const i = this.#l[t];
                        this.#w && this.#s?.(e, i, "delete"), this.#S && this.#m?.push([
                            e,
                            i,
                            "delete"
                        ]);
                    }
                }
                if (this.#u.clear(), this.#f.fill(void 0), this.#l.fill(void 0), this.#T && this.#b && (this.#T.fill(0), this.#b.fill(0)), this.#v && this.#v.fill(0), this.#y = 0, this.#E = 0, this.#g.length = 0, this.#c = 0, this.#a = 0, this.#S && this.#m) {
                    const t = this.#m;
                    let e;
                    for(; e = t?.shift();)this.#o?.(...e);
                }
            }
        }
        class R {
            #z;
            #G;
            #K;
            constructor(t, e, i){
                this.#G = t.docExistsSpace, this.#K = i, this.#z = e;
            }
            #W = (t, e = {})=>{
                const i = (t)=>"string" == typeof t || "number" == typeof t ? this.#K.GET(t, e.PIPELINE) : t.FIELD || t.VALUE ? this.#K.GET(t) : t.AND ? this.#K.AND(t.AND.map(i), e.PIPELINE) : t.GET ? this.#K.GET(t.GET, e.PIPELINE) : t.NOT ? this.#K.NOT(i(t.NOT.INCLUDE), i(t.NOT.EXCLUDE)) : t.OR ? this.#K.OR(t.OR.map(i), e.PIPELINE) : t.ALL_DOCUMENTS ? this.ALL_DOCUMENTS(t.ALL_DOCUMENTS) : void 0;
                return i(t).then((t)=>t.RESULT ? Object.assign(t, {
                        RESULT_LENGTH: t.RESULT.length
                    }) : {
                        RESULT: t,
                        RESULT_LENGTH: t.length
                    }).then((t)=>e.BUCKETS ? this.#K.BUCKETS(...e.BUCKETS).then((e)=>Object.assign(t, {
                            BUCKETS: this.#K.AGGREGATION_FILTER(e, t.RESULT)
                        })) : t).then((i)=>e.FACETS ? i.RESULT.length ? t.ALL_DOCUMENTS ? this.FACETS(...e.FACETS).then((t)=>Object.assign(i, {
                            FACETS: t
                        })) : this.FACETS(...e.FACETS).then((t)=>Object.assign(i, {
                            FACETS: this.#K.AGGREGATION_FILTER(t, i.RESULT)
                        })) : Object.assign(i, {
                        FACETS: []
                    }) : i).then((t)=>e.WEIGHT ? Object.assign({
                        RESULT: this.WEIGHT(t.RESULT, e.WEIGHT)
                    }, t) : t).then((t)=>e.SCORE ? this.SCORE(t.RESULT, e.SCORE).then((e)=>Object.assign(t, {
                            RESULT: e
                        })) : t).then((t)=>Object.assign(t, e.SORT ? {
                        RESULT: this.SORT(t.RESULT, e.SORT)
                    } : {})).then((t)=>Object.assign(t, e.PAGE ? {
                        RESULT: this.PAGE(t.RESULT, e.PAGE)
                    } : {})).then((t)=>e.DOCUMENTS ? this.DOCUMENTS(...t.RESULT.map((t)=>t._id)).then((e)=>Object.assign(t, {
                            RESULT: t.RESULT.map((t, i)=>Object.assign(t, {
                                    _doc: e[i]
                                }))
                        })) : t);
            };
            #$ = (t, e)=>new Promise((i)=>(e = JSON.stringify(e), this.#z.has(e) ? i(this.#z.get(e)) : t.then((t)=>this.#z.set(e, t)).then(()=>i(this.#z.get(e)))));
            #Y = (t)=>this.DISTINCT(t).then((t)=>Array.from(t.reduce((t, e)=>t.add(e.VALUE), new Set)).sort((t, e)=>(t + "").localeCompare(e + "", void 0, {
                            numeric: !0,
                            sensitivity: "base"
                        })));
            #X = (...t)=>t.length ? Promise.all(t.map((t)=>this.#K.STORE.get([
                        this.#G,
                        t
                    ]).catch((t)=>null))) : this.ALL_DOCUMENTS();
            #H = (t, e)=>this.#W({
                    AND: [
                        ...t
                    ]
                }, Object.assign({
                    SCORE: {
                        TYPE: "TFIDF"
                    },
                    SORT: !0
                }, e));
            ALL_DOCUMENTS = (t)=>this.#K.STORE.iterator({
                    gte: [
                        this.#G,
                        null
                    ],
                    lte: [
                        this.#G,
                        void 0
                    ],
                    limit: t
                }).all().then((t)=>t.map(([t, e])=>({
                            _id: e._id,
                            _doc: e
                        })));
            DICTIONARY = (t)=>this.#$(this.#Y(t), {
                    DICTIONARY: t || null
                });
            DISTINCT = (...t)=>this.#K.DISTINCT(...t).then((t)=>[
                        ...t.reduce((t, e)=>t.add(JSON.stringify(Object.assign(e, {
                                VALUE: e.VALUE
                            }))), new Set)
                    ].map(JSON.parse));
            DOCUMENTS = (...t)=>this.#$(this.#X(...t), {
                    DOCUMENTS: t
                });
            DOCUMENT_COUNT = ()=>this.#K.STORE.get([
                    "DOCUMENT_COUNT"
                ]);
            DOCUMENT_VECTORS = (...t)=>Promise.all(t.map((t)=>this.#K.STORE.get([
                        "DOC",
                        t
                    ]).catch((t)=>null)));
            FACETS = (...t)=>this.#K.FACETS(...t).then((t)=>[
                        ...t.reduce((t, e)=>t.add(JSON.stringify(Object.assign(e, {
                                VALUE: e.VALUE
                            }))), new Set)
                    ].map(JSON.parse));
            PAGE = (t, e = {})=>{
                const i = (e = Object.assign({
                    NUMBER: 0,
                    SIZE: 20
                }, e)).NUMBER * e.SIZE, n = i + e.SIZE || void 0;
                return t.slice(i, n);
            };
            QUERY = (t, e)=>this.#$(this.#W(t, e), {
                    QUERY: [
                        t,
                        e
                    ]
                });
            SCORE = (t, e = {})=>{
                e = Object.assign({
                    TYPE: "TFIDF"
                }, e);
                const i = (t)=>!e.FIELDS || e.FIELDS.includes(t.FIELD), n = (t)=>(t || []).filter(i);
                return new Promise((r)=>r("TFIDF" === e.TYPE ? this.DOCUMENT_COUNT().then((e)=>t.map((t, n, r)=>{
                            const s = Math.log((e + 1) / r.length);
                            return t._score = +(t._match || []).filter(i).reduce((t, e)=>t + s * +e.SCORE, 0).toFixed(2), t;
                        })) : "PRODUCT" === e.TYPE ? t.map((t)=>({
                            ...t,
                            _score: +n(t._match).reduce((t, e)=>t * +e.SCORE, 1).toFixed(2)
                        })) : "CONCAT" === e.TYPE ? t.map((t)=>({
                            ...t,
                            _score: n(t._match).reduce((t, e)=>t + e.SCORE, "")
                        })) : "SUM" === e.TYPE ? t.map((t)=>({
                            ...t,
                            _score: +n(t._match).reduce((t, e)=>t + +e.SCORE, 0).toFixed(2)
                        })) : "VALUE" === e.TYPE ? t.map((t)=>({
                            ...t,
                            _score: n(t._match).reduce((t, e)=>t + e.VALUE, "")
                        })) : null));
            };
            SEARCH = (t, e)=>this.#$(this.#H(t, e), {
                    SEARCH: [
                        t,
                        e
                    ]
                });
            SORT = (t, e)=>(e = Object.assign({
                    DIRECTION: "DESCENDING",
                    TYPE: "NUMERIC"
                }, e || {}), t.sort((t, e)=>t._id < e._id ? -1 : t._id > e._id ? 1 : 0).sort({
                    NUMERIC: {
                        DESCENDING: (t, e)=>+e._score - +t._score,
                        ASCENDING: (t, e)=>+t._score - +e._score
                    },
                    ALPHABETIC: {
                        DESCENDING: (t, e)=>t._score < e._score ? 1 : t._score > e._score ? -1 : 0,
                        ASCENDING: (t, e)=>t._score < e._score ? -1 : t._score > e._score ? 1 : 0
                    }
                }[e.TYPE][e.DIRECTION]));
            WEIGHT = (t, e)=>t.map((t)=>(t._match = t._match.map((t)=>(e.forEach((e)=>{
                            let i = !1;
                            e.FIELD && e.VALUE ? e.FIELD === t.FIELD && e.VALUE === t.VALUE && (i = !0) : e.FIELD ? e.FIELD === t.FIELD && (i = !0) : e.VALUE && e.VALUE === t.VALUE && (i = !0), i && (t.SCORE = (e.WEIGHT * +t.SCORE).toFixed(2));
                        }), t)), t));
        }
        var N = i(729);
        class U extends Error {
            constructor(t){
                super(t), this.name = "TimeoutError";
            }
        }
        class B extends Error {
            constructor(t){
                super(), this.name = "AbortError", this.message = t;
            }
        }
        const F = (t)=>void 0 === globalThis.DOMException ? new B(t) : new DOMException(t), P = (t)=>{
            const e = void 0 === t.reason ? F("This operation was aborted.") : t.reason;
            return e instanceof Error ? e : F(e);
        };
        var M, j = function(t, e, i, n) {
            if ("a" === i && !n) throw new TypeError("Private accessor was defined without a getter");
            if ("function" == typeof e ? t !== e || !n : !e.has(t)) throw new TypeError("Cannot read private member from an object whose class did not declare it");
            return "m" === i ? n : "a" === i ? n.call(t) : n ? n.value : e.get(t);
        };
        class V {
            constructor(){
                M.set(this, []);
            }
            enqueue(t, e) {
                const i = {
                    priority: (e = {
                        priority: 0,
                        ...e
                    }).priority,
                    run: t
                };
                if (this.size && j(this, M, "f")[this.size - 1].priority >= e.priority) return void j(this, M, "f").push(i);
                const n = function(t, e, i) {
                    let n = 0, r = t.length;
                    for(; r > 0;){
                        const i = Math.trunc(r / 2);
                        let o = n + i;
                        s = t[o], e.priority - s.priority <= 0 ? (n = ++o, r -= i + 1) : r = i;
                    }
                    var s;
                    return n;
                }(j(this, M, "f"), i);
                j(this, M, "f").splice(n, 0, i);
            }
            dequeue() {
                const t = j(this, M, "f").shift();
                return null == t ? void 0 : t.run;
            }
            filter(t) {
                return j(this, M, "f").filter((e)=>e.priority === t.priority).map((t)=>t.run);
            }
            get size() {
                return j(this, M, "f").length;
            }
        }
        M = new WeakMap;
        var z, G, K, W, $, Y, X, H, J, q, Q, Z, tt, et, it, nt, rt, st, ot, ht, at, ct, ut, lt, ft, dt, pt = function(t, e, i, n, r) {
            if ("m" === n) throw new TypeError("Private method is not writable");
            if ("a" === n && !r) throw new TypeError("Private accessor was defined without a setter");
            if ("function" == typeof e ? t !== e || !r : !e.has(t)) throw new TypeError("Cannot write private member to an object whose class did not declare it");
            return "a" === n ? r.call(t, i) : r ? r.value = i : e.set(t, i), i;
        }, yt = function(t, e, i, n) {
            if ("a" === i && !n) throw new TypeError("Private accessor was defined without a getter");
            if ("function" == typeof e ? t !== e || !n : !e.has(t)) throw new TypeError("Cannot read private member from an object whose class did not declare it");
            return "m" === i ? n : "a" === i ? n.call(t) : n ? n.value : e.get(t);
        };
        class Et extends Error {
        }
        class gt extends N {
            constructor(t){
                var e, i, n, r;
                if (super(), z.add(this), G.set(this, void 0), K.set(this, void 0), W.set(this, 0), $.set(this, void 0), Y.set(this, void 0), X.set(this, 0), H.set(this, void 0), J.set(this, void 0), q.set(this, void 0), Q.set(this, void 0), Z.set(this, 0), tt.set(this, void 0), et.set(this, void 0), it.set(this, void 0), Object.defineProperty(this, "timeout", {
                    enumerable: !0,
                    configurable: !0,
                    writable: !0,
                    value: void 0
                }), !("number" == typeof (t = {
                    carryoverConcurrencyCount: !1,
                    intervalCap: Number.POSITIVE_INFINITY,
                    interval: 0,
                    concurrency: Number.POSITIVE_INFINITY,
                    autoStart: !0,
                    queueClass: V,
                    ...t
                }).intervalCap && t.intervalCap >= 1)) throw new TypeError(`Expected \`intervalCap\` to be a number from 1 and up, got \`${null !== (i = null === (e = t.intervalCap) || void 0 === e ? void 0 : e.toString()) && void 0 !== i ? i : ""}\` (${typeof t.intervalCap})`);
                if (void 0 === t.interval || !(Number.isFinite(t.interval) && t.interval >= 0)) throw new TypeError(`Expected \`interval\` to be a finite number >= 0, got \`${null !== (r = null === (n = t.interval) || void 0 === n ? void 0 : n.toString()) && void 0 !== r ? r : ""}\` (${typeof t.interval})`);
                pt(this, G, t.carryoverConcurrencyCount, "f"), pt(this, K, t.intervalCap === Number.POSITIVE_INFINITY || 0 === t.interval, "f"), pt(this, $, t.intervalCap, "f"), pt(this, Y, t.interval, "f"), pt(this, q, new t.queueClass, "f"), pt(this, Q, t.queueClass, "f"), this.concurrency = t.concurrency, this.timeout = t.timeout, pt(this, it, !0 === t.throwOnTimeout, "f"), pt(this, et, !1 === t.autoStart, "f");
            }
            get concurrency() {
                return yt(this, tt, "f");
            }
            set concurrency(t) {
                if (!("number" == typeof t && t >= 1)) throw new TypeError(`Expected \`concurrency\` to be a number from 1 and up, got \`${t}\` (${typeof t})`);
                pt(this, tt, t, "f"), yt(this, z, "m", lt).call(this);
            }
            async add(t, e = {}) {
                return e = {
                    timeout: this.timeout,
                    throwOnTimeout: yt(this, it, "f"),
                    ...e
                }, new Promise((i, n)=>{
                    yt(this, q, "f").enqueue(async ()=>{
                        var r, s, o;
                        pt(this, Z, (s = yt(this, Z, "f"), ++s), "f"), pt(this, W, (o = yt(this, W, "f"), ++o), "f");
                        try {
                            if (null === (r = e.signal) || void 0 === r ? void 0 : r.aborted) throw new Et("The task was aborted.");
                            let n = t({
                                signal: e.signal
                            });
                            e.timeout && (n = function(t, e, i, n) {
                                let r;
                                const s = new Promise((s, o)=>{
                                    if ("number" != typeof e || 1 !== Math.sign(e)) throw new TypeError(`Expected \`milliseconds\` to be a positive number, got \`${e}\``);
                                    if (e !== Number.POSITIVE_INFINITY) {
                                        if ((n = {
                                            customTimers: {
                                                setTimeout,
                                                clearTimeout
                                            },
                                            ...n
                                        }).signal) {
                                            const { signal: t } = n;
                                            t.aborted && o(P(t)), t.addEventListener("abort", ()=>{
                                                o(P(t));
                                            });
                                        }
                                        r = n.customTimers.setTimeout.call(void 0, ()=>{
                                            const n = i instanceof Error ? i : new U(`Promise timed out after ${e} milliseconds`);
                                            "function" == typeof t.cancel && t.cancel(), o(n);
                                        }, e), (async ()=>{
                                            try {
                                                s(await t);
                                            } catch (t) {
                                                o(t);
                                            } finally{
                                                n.customTimers.clearTimeout.call(void 0, r);
                                            }
                                        })();
                                    } else s(t);
                                });
                                return s.clear = ()=>{
                                    clearTimeout(r), r = void 0;
                                }, s;
                            }(Promise.resolve(n), e.timeout)), e.signal && (n = Promise.race([
                                n,
                                yt(this, z, "m", ft).call(this, e.signal)
                            ]));
                            const s = await n;
                            i(s), this.emit("completed", s);
                        } catch (t) {
                            if (t instanceof U && !e.throwOnTimeout) return void i();
                            n(t), this.emit("error", t);
                        } finally{
                            yt(this, z, "m", st).call(this);
                        }
                    }, e), this.emit("add"), yt(this, z, "m", at).call(this);
                });
            }
            async addAll(t, e) {
                return Promise.all(t.map(async (t)=>this.add(t, e)));
            }
            start() {
                return yt(this, et, "f") ? (pt(this, et, !1, "f"), yt(this, z, "m", lt).call(this), this) : this;
            }
            pause() {
                pt(this, et, !0, "f");
            }
            clear() {
                pt(this, q, new (yt(this, Q, "f")), "f");
            }
            async onEmpty() {
                0 !== yt(this, q, "f").size && await yt(this, z, "m", dt).call(this, "empty");
            }
            async onSizeLessThan(t) {
                yt(this, q, "f").size < t || await yt(this, z, "m", dt).call(this, "next", ()=>yt(this, q, "f").size < t);
            }
            async onIdle() {
                0 === yt(this, Z, "f") && 0 === yt(this, q, "f").size || await yt(this, z, "m", dt).call(this, "idle");
            }
            get size() {
                return yt(this, q, "f").size;
            }
            sizeBy(t) {
                return yt(this, q, "f").filter(t).length;
            }
            get pending() {
                return yt(this, Z, "f");
            }
            get isPaused() {
                return yt(this, et, "f");
            }
        }
        G = new WeakMap, K = new WeakMap, W = new WeakMap, $ = new WeakMap, Y = new WeakMap, X = new WeakMap, H = new WeakMap, J = new WeakMap, q = new WeakMap, Q = new WeakMap, Z = new WeakMap, tt = new WeakMap, et = new WeakMap, it = new WeakMap, z = new WeakSet, nt = function() {
            return yt(this, K, "f") || yt(this, W, "f") < yt(this, $, "f");
        }, rt = function() {
            return yt(this, Z, "f") < yt(this, tt, "f");
        }, st = function() {
            var t;
            pt(this, Z, (t = yt(this, Z, "f"), --t), "f"), yt(this, z, "m", at).call(this), this.emit("next");
        }, ot = function() {
            yt(this, z, "m", ut).call(this), yt(this, z, "m", ct).call(this), pt(this, J, void 0, "f");
        }, ht = function() {
            const t = Date.now();
            if (void 0 === yt(this, H, "f")) {
                const e = yt(this, X, "f") - t;
                if (!(e < 0)) return void 0 === yt(this, J, "f") && pt(this, J, setTimeout(()=>{
                    yt(this, z, "m", ot).call(this);
                }, e), "f"), !0;
                pt(this, W, yt(this, G, "f") ? yt(this, Z, "f") : 0, "f");
            }
            return !1;
        }, at = function() {
            if (0 === yt(this, q, "f").size) return yt(this, H, "f") && clearInterval(yt(this, H, "f")), pt(this, H, void 0, "f"), this.emit("empty"), 0 === yt(this, Z, "f") && this.emit("idle"), !1;
            if (!yt(this, et, "f")) {
                const t = !yt(this, z, "a", ht);
                if (yt(this, z, "a", nt) && yt(this, z, "a", rt)) {
                    const e = yt(this, q, "f").dequeue();
                    return !!e && (this.emit("active"), e(), t && yt(this, z, "m", ct).call(this), !0);
                }
            }
            return !1;
        }, ct = function() {
            yt(this, K, "f") || void 0 !== yt(this, H, "f") || (pt(this, H, setInterval(()=>{
                yt(this, z, "m", ut).call(this);
            }, yt(this, Y, "f")), "f"), pt(this, X, Date.now() + yt(this, Y, "f"), "f"));
        }, ut = function() {
            0 === yt(this, W, "f") && 0 === yt(this, Z, "f") && yt(this, H, "f") && (clearInterval(yt(this, H, "f")), pt(this, H, void 0, "f")), pt(this, W, yt(this, G, "f") ? yt(this, Z, "f") : 0, "f"), yt(this, z, "m", lt).call(this);
        }, lt = function() {
            for(; yt(this, z, "m", at).call(this););
        }, ft = async function(t) {
            return new Promise((e, i)=>{
                t.addEventListener("abort", ()=>{
                    i(new Et("The task was aborted."));
                }, {
                    once: !0
                });
            });
        }, dt = async function(t, e) {
            return new Promise((i)=>{
                const n = ()=>{
                    e && !e() || (this.off(t, n), i());
                };
                this.on(t, n);
            });
        };
        class mt {
            #z;
            #K;
            #J;
            #q;
            constructor(t, e, i){
                this.#z = e, this.#K = i, this.#J = t, this.#q = new gt({
                    concurrency: 1
                }), this.#Q(0);
            }
            #Q = (t)=>this.#K.STORE.get([
                    "DOCUMENT_COUNT"
                ]).then((e)=>this.#K.STORE.put([
                        "DOCUMENT_COUNT"
                    ], +e + (+t || 0))).catch((t)=>this.#K.STORE.put([
                        "DOCUMENT_COUNT"
                    ], 0));
            #Z = (t, e)=>{
                this.#z.clear();
                const i = {
                    ...this.#J,
                    ...e
                };
                return ((t, e)=>{
                    const i = (t)=>"string" == typeof t || t instanceof String, n = (t, r)=>new Promise(async (s)=>{
                            var o, h, a;
                            return null === t ? s([
                                null,
                                "1.00"
                            ]) : (o = t) && 0 === Object.keys(o).length && Object.getPrototypeOf(o) === Object.prototype ? s([
                                [],
                                "1.00"
                            ]) : Number.isInteger(t) ? s([
                                t,
                                t
                            ]) : i(t) ? s(e.tokenizer(t, r, e)) : ((t)=>"object" == typeof t && null !== t && !Array.isArray(t))(t) ? s((h = t, new Promise(async (t)=>{
                                const i = {};
                                for(const t in h)i[t] = await n(h[t], t, e);
                                return t(i);
                            }))) : ((t)=>Array.isArray(t))(t) ? s((a = t, Promise.all(a.map(n)))) : s(t);
                        });
                    return Promise.all(t.map(async (t)=>new Promise(async (r)=>{
                            i(t) && (t = {
                                body: t
                            }), Object.prototype.hasOwnProperty.call(t, "_id") || (t._id = e.idGenerator.next().value);
                            const s = {};
                            for(const e in t)s[e] = "_id" !== e ? await n(t[e], e) : t[e];
                            return r(s);
                        })));
                })(t, i).then((t)=>this.#K.PUT(t, e)).then((e)=>this.PUT_RAW(t, e.map((t)=>t._id), !i.storeRawDocs).then(()=>this.#Q(e.filter((t)=>"CREATED" === t.status).length)).then(()=>e));
            };
            #tt = (t)=>this.#K.DELETE(t).then((e)=>this.DELETE_RAW(...t).then(()=>this.#Q(-e.filter((t)=>"DELETED" === t.status).length)).then(()=>this.#z.clear()).then(()=>e));
            DELETE = (...t)=>this.#tt(t);
            DELETE_RAW = (...t)=>Promise.all(t.map((t)=>this.#K.STORE.del([
                        this.#J.docExistsSpace,
                        t
                    ])));
            FLUSH = ()=>this.#K.STORE.clear().then(()=>{
                    this.#z.clear();
                    const t = Date.now();
                    return this.#K.STORE.batch([
                        {
                            type: "put",
                            key: [
                                "~CREATED"
                            ],
                            value: t
                        },
                        {
                            type: "put",
                            key: [
                                "~LAST_UPDATED"
                            ],
                            value: t
                        },
                        {
                            type: "put",
                            key: [
                                "DOCUMENT_COUNT"
                            ],
                            value: 0
                        }
                    ]);
                }).then(()=>!0);
            IMPORT = (t)=>(this.#z.clear(), Promise.resolve(this.#K.IMPORT(t)));
            PUT = (t, e)=>this.#q.add(()=>this.#Z(t, e));
            PUT_RAW = (t, e, i)=>Promise.all(t.map((t, n)=>this.#K.STORE.put([
                        this.#J.docExistsSpace,
                        e[n]
                    ], i ? {} : t))).then((i)=>(this.#z.clear(), t.map((t, i)=>({
                            _id: e[i],
                            status: "OK",
                            operation: "_PUT_RAW"
                        }))));
        }
        class vt {
            constructor(t = {}){
                var e;
                t = {
                    cacheLength: 1e3,
                    caseSensitive: !1,
                    docExistsSpace: "DOC_RAW",
                    idGenerator: function*() {
                        let t = 0;
                        for(;;)yield Date.now() + "-" + t++;
                    }(),
                    skipFields: [],
                    ngrams: {},
                    replace: {},
                    storeRawDocs: !0,
                    stopwords: [],
                    storeVectors: !0,
                    tokenSplitRegex: /[\p{L}\d]+/gu,
                    tokenizer: l,
                    ...t
                }, this.INDEX = new b({
                    ...t,
                    isLeaf: (t)=>Array.isArray(t) && 2 === t.length && t.every((t)=>"string" == typeof t || "number" == typeof t || null === t)
                }), this._CACHE = new D({
                    max: t.cacheLength
                }), this.r = new R(t, this._CACHE, this.INDEX), this.w = new mt(t, this._CACHE, this.INDEX), e = this.INDEX, new Promise((t, i)=>{
                    const n = [
                        "CREATED_WITH"
                    ], r = "search-index@5.0.0-rc1";
                    return e.STORE.get(n).then((e)=>r === e ? t() : i(new Error("This index was created with " + e + ", you are running " + r))).catch((i)=>e.STORE.put(n, r).then(t));
                });
            }
            ALL_DOCUMENTS = (t)=>this.r.ALL_DOCUMENTS(t);
            BUCKETS = (...t)=>this.INDEX.BUCKETS(...t);
            CREATED = ()=>this.INDEX.CREATED();
            DELETE = (...t)=>this.w.DELETE(...t);
            DELETE_RAW = (...t)=>this.w.DELETE_RAW(...t);
            DICTIONARY = (t)=>this.r.DICTIONARY(t);
            DISTINCT = (...t)=>this.r.DISTINCT(...t);
            DOCUMENTS = (...t)=>this.r.DOCUMENTS(...t);
            DOCUMENT_COUNT = ()=>this.r.DOCUMENT_COUNT();
            DOCUMENT_VECTORS = (...t)=>this.r.DOCUMENT_VECTORS(...t);
            EXPORT = ()=>this.INDEX.EXPORT();
            FACETS = (...t)=>this.r.FACETS(...t);
            FIELDS = ()=>this.INDEX.FIELDS();
            FLUSH = ()=>this.w.FLUSH();
            IMPORT = (t)=>this.INDEX.IMPORT(t);
            LAST_UPDATED = ()=>this.INDEX.LAST_UPDATED();
            MAX = (t)=>this.INDEX.MAX(t);
            MIN = (t)=>this.INDEX.MIN(t);
            PUT = (t, e)=>this.w.PUT(t, e);
            PUT_RAW = (t, e, i)=>this.w.PUT_RAW(t, e, i);
            QUERY = (t, e)=>this.r.QUERY(t, e);
            SEARCH = (t, e)=>this.r.SEARCH(t, e);
            TOKENIZATION_PIPELINE_STAGES = {
                SPLIT: r,
                SKIP: s,
                LOWCASE: o,
                REPLACE: h,
                NGRAMS: a,
                STOPWORDS: c,
                SCORE_TERM_FREQUENCY: u
            };
            _AND = (t, e)=>this.INDEX.AND(t, e);
            _BUCKET = (t)=>this.INDEX.BUCKET(t);
            _GET = (t, e)=>this.INDEX.GET(t, e);
            _NOT = (t, e)=>this.INDEX.NOT(t, e);
            _OR = (t, e)=>this.INDEX.OR(t, e);
            _PAGE = (t, e)=>this.r.PAGE(t, e);
            _SORT = (t, e)=>this.r.SORT(t, e);
        }
        class bt {
            constructor(t = {}){
                return new vt({
                    Level: f.v,
                    ...t
                });
            }
        }
    })(), SearchIndex = n;
})();

//# sourceMappingURL=index.0fa55048.js.map
