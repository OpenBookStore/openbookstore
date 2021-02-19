Vue.use(Buefy.Autocomplete);
Vue.use(Buefy.Input);
Vue.use(Buefy.Numberinput);
Vue.use(Buefy.Field);
Vue.use(Buefy.Button);
Vue.use(Buefy.Datepicker);
Vue.use(Buefy.Select);

const qsearchData = {
    data() {
        return {
            data: [],
            name: '',
            selected: null,
            isFetching: false
        };
    },
    methods: {
        getAsyncData: _.debounce(function (name) {
            if (!name.length) {
                this.data = [];
                return;
            }
            // Is the user inputting a number?
            if (/^\d+$/.test(name) && name.length < 13) {
                this.data = [];
                return;
            }
            this.isFetching = true;
            this.$http.get(`/api/quick-search?q=${name}`)
                .then(({ data }) => {
                    // here we go elsewhere if appropriate.
                    if (data && data.hasOwnProperty("go")) {
                        window.location = data.go;
                    }
                    if (data) {
                        this.data = [];
                        data.results.forEach((item) => this.data.push(item));
                    }
                })
                .catch((error) => {
                    this.data = [];
                    throw error;
                })
                .finally(() => {
                    this.isFetching = false;
                });
        }, 500),
        itemSelected: function (item) {
            console.log(item);
            window.location = item.url;
        }
    }
}

var vm = new Vue(qsearchData);
vm.$mount('#quick-search');

////////////////////////////////////////////////////////////////////
/// Receive menu
////////////////////////////////////////////////////////////////////

// Currently, we only look for ISBNs, we don't handle a keyword search.
const receivePage = {
    data() {
        return {
            input: "",
            cards: [],
            counter: 0,  // for the cards entered so far.
        };
    },

    methods: {
        addCard: function () {
            const url = "/api/receive";
            this.counter += 1;
            const current_counter = this.counter;
            this.cards.push({
                counter: this.counter,
                entry: this.input,
            });

            // API call, async.
            // /api/receive?isbn=978&counter=i
            // TODO: handle keyword search.
            //
            // On success:
            // - response contains counter + card data
            // - get card holder with given counter, update card data.
            // TODO: On error, show an alert, show card holder of counter in red.

            async function postData(url, input) {
                let body = "counter=" + current_counter + "&isbn=" + input;
                const response = await fetch(url, {
                    method: 'POST',
                    headers: {
                        'Content-Type':'application/x-www-form-urlencoded'
                    },
                    body: body,
                });
                let text = await response.json();
                return text;
            };

            let res = postData(url, this.input);
            res.then((text) => {
                let res = _.find(this.cards, ['counter', current_counter]);
                res.entry = text;
            });

            this.input = "";
        },

        reversedCards: function () {
            // reverse() mutates in-place. That gives an infinite loop to Vue. Damn JS.
            return this.cards.slice().reverse();
        },
    }
}

// Mount on the right page.
if (document.getElementById('vue-receive')) {
    var vm = new Vue(receivePage);
    vm.$mount('#vue-receive');
}



////////////////////////////////////////////////////////////////////
/// Sell menu
////////////////////////////////////////////////////////////////////

Vue.component('bookstore-card', {
    props: ['card'],
    template:
    " \
    <div class='media'> \
      <div class='media-left'> \
        <p class='image is-64x64'> \
          <img v-bind:src='card.coverUrl'> \
        </p> \
      </div> \
      <div class='media-content'> \
        <div class='content'> \
          <span class='title is-6'> {{ card.title }} </span> \
          <div> {{ card.authors }} </div> \
          <span class='has-text-grey-light'> {{ card.isbn }} </span> \
          <span> éd. {{ _.upperFirst(card.publisher) }} </span> \
          <p> \
          </p> \
        </div> \
      </div> \
          </div> \
"
});

const sellPage = {
    data () {
        return {
            books: [{error: null, card: null, input: "", quantity: 1, show: false}],
            suggestions: [],
            unsaved: false,
            paymentMethods: ["Cash", "Mastercard"],
            paymentMethod: "",
            client: "",
            search: "",
            selected: null,
            isFetching: false,
            sellDate: new Date()
        };
    },
    methods: {
        reset: function () {
            self.books = [];
            self.suggestions = [];
            self.paymentMethod = "";
            self.client = "";
            self.search = "";
            self.sellDate = new Date();
        },
        completeSale: function () {
            var books = [];
            this.books.forEach((book) => {
                if (book.show && !book.error && book.card && book.quantity != 0) {
                    bk = {
                        id: book.card.id,
                        price: book.card.price,
                        quantity: book.quantity
                    };
                    books.push(bk);
                }
            });
            var data = {
                books: books,
                paymentMethod: this.paymentMethod,
                client: this.client,
                sellDate: this.sellDate.toISOString(),
            };
            this.$http.post('/api/sell-complete/', data, {emulateJSON:true} )
                .then(({ data }) => {
                    if (data.success) {
                        this.unsaved = false;
                        this.reset();
                    }
                })
                .catch((error) => {
                    throw error;
                });
        },

        getAsyncData: _.debounce (function (input) {
            if (!input.length) {
                this.suggestions = [];
                return;
            }
            // Is the user inputting a number?
            if (/^\d+$/.test(input)) {
                if (input.length < 13) {
                    this.suggestions = [];
                } else {
                    //Asynchronous search for ISBN happens here...
                    var cardindex = this.newBook({input, error: null, card: null});
                    this.$http.get(`/api/sell-search?q=${input}`)
                        .then(({ data }) => {
                            var book = {input: this.books[cardindex].input,
                                        show: true,
                                        quantity: 1,
                                        card: null,
                                        error: null};
                            if (data && data.hasOwnProperty("card")) {
                                book.card = data.card;
                            } else if (data && data.hasOwnProperty("error")) {
                                book.error = data.error;
                            }
                            Vue.set(this.books, cardindex, book);
                            this.$forceUpdate();
                        })
                        .catch((error) => {
                            this.suggestions = [];
                            throw error;
                        });
                }
                return;
            }
            this.isFetching = true;
            this.$http.get(`/api/sell-search?q=${input}`)
                .then(({ data }) => {
                    if (data && data.hasOwnProperty("card")) {
                        this.newBook({ card: data.card, input });
                    } else if (data && data.hasOwnProperty("options")) {
                        this.suggestions = [];
                        data.options.forEach((item) => this.suggestions.push(item));
                    } else if (data && data.hasOwnProperty("error")) {
                        this.newBook({ error: data.error, input });
                    }
                })
                .catch((error) => {
                    this.suggestions = [];
                    throw error;
                })
                .finally(() => {
                    this.isFetching = false;
                });
        }, 500),
        itemSelected: function (item) {
            if (item) {
                this.newBook({ card: item.card, input: this.search });
            }
        },
        removeBook: function (index) {
            //book isn't removed, just hidden
            this.books[index].show = false;
        },
        newBook: function (params) {
            params.show = true;
            params.quantity = 1;
            this.books.push(params);
            setTimeout(function() {this.search = ""; this.unsaved = true;}.bind(this));
            return this.books.length - 1;
        }
    },
    computed: {
        quantity: function () {
            const reducer = function (accum, val) {
                if (val.show && val.card) {
                    return accum + val.quantity;
                } else {
                    return accum;
                }
            };
            return this.books.reduce(reducer, 0);
        },
        total: function () {
            const reducer = function (accum, val) {
                if (val.show && val.card) {
                    return accum + val.quantity * val.card.price;
                } else {
                    return accum;
                }
            };
            return this.books.reduce(reducer, 0.0);
        }
    }
}


if (document.getElementById('vue-sell')) {
    var vsell = new Vue(sellPage);
    vsell.$mount('#vue-sell');

    window.addEventListener("beforeunload", function (e) {
        if (vsell.$data.unsaved) {
            var msg = 'Page appears to have unsaved changes';
            (e || window.event).returnValue = msg;
            return msg;
        } else {
            return undefined;
        }
    });
}


