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
                    console.log(data);
                    if (data && data.hasOwnProperty("go")) {
                        window.location = data.go;
                    } else if (data.hasOwnProperty("GO")) {
                        window.location = data.GO;
                    }
                    if (data) {
                        this.data = [];
                        if (data.results) {
                            data.results.forEach((item) => this.data.push(item));
                        } else if (data.RESULTS) {
                            data.RESULTS.forEach((item) => this.data.push(item));
                        } else {
                            console.log("No results (data.results)");
                        }
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
            // As soon as an ISBN is entered, we want to display it and give back control
            // to the search input.
            // We first display the ISBN, as is, and we are requesting the book data in the background.
            // We associate the ISBN with an internal counter. We give this counter to the back-end too,
            // so than when the back-end returns our bibliographic data, we can update the
            // right entry (the one of this counter).

            if (this.input.length < 3) {
                return;
            }

            const url = "/api/receive";
            this.counter += 1;
            const current_counter = this.counter;
            this.cards.push({
                counter: this.counter,
                entry: this.input,
            });

            // API call, async.
            // /api/receive?q=978&counter=i
            // TODO: handle keyword search.
            //
            // On success:
            // - response contains counter + card data
            // - get card holder with given counter, update card data.
            // TODO: On error, show an alert, show card holder of counter in red.

            async function postData(url, input) {
                let body = "counter=" + current_counter + "&q=" + input;
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
                let card = _.find(this.cards, ['counter', current_counter]);
                card.entry = text;
            });

            this.input = "";
        },

        reversedCards: function () {
            // reverse() mutates in-place. That gives an infinite loop to Vue. Damn JS.
            return this.cards.slice().reverse();
        },
    }
};

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

    /*
    XXX update 2021-03: unused template. We only need to show the title.
    Use it like this:

    <bookstore-card v-if="book.card"
      v-bind:key="index"
      v-bind:card="book.card">
    </bookstore-card>
    */
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

const paymentMethods = [
    {id: 1, name: "Cash"},
    {id: 2, name:"CARD"}
];

function paymentMethodName (id) {
    return _.find(paymentMethods, {'id': id}).name;
};

function initialSellPage (alt) {
    return {
        books: [{error: null, card: null, input: "", quantity: 1, show: false}],
        suggestions: [],
        unsaved: false,
        paymentMethods: paymentMethods,
        paymentMethod: "",
        client: "",
        search: "",
        message: "",
        selected: null,
        isFetching: false,
        sellDate: new Date(),
        ...alt
    };
}

const sellPage = {
    // Ask the API for a list of items on user input.
    // If there is only one result, add it straight away.
    // If we scan an ISBN, we add it straight away, and *we fetch its data in the background*.
    // Scanning books must be super fast for the user: they scan a book after the other without
    // looking at the UI, they don't want to wait: the scanned book will be found anyways.
    //
    // Props:
    // - this.books: a list of objects containing:
    //   - card: other object, containing all the book's data
    //   - quantity: the quantity to sell
    //   - show: bool
    data () {
        return initialSellPage();
    },
    methods: {
        completeSale: function (payment_method_id) {
            // TODO: finish register payment_method
            var books = [];
            if (this.books && this.books.length == 1 && !this.books[0].card) {
                return;
            }
            this.books.forEach((book) => {
                if (book.show && !book.error && book.card && book.quantity != 0) {
                    bk = {
                        id: book.card.id,
                        price: book.card.price,
                        quantity: book.quantity,
                        url: book.url
                    };
                    books.push(bk);
                }
            });
            var data = {
                books: books,
                paymentMethodId: payment_method_id,
                paymentMethodName: paymentMethodName(payment_method_id),
                client: this.client,
                sellDate: this.sellDate.toISOString(),
            };
            this.$http.post('/api/sell-complete/', data, {emulateJSON:true} )
                .then(({ data }) => {
                    if (data.success) {
                        this.unsaved = false;
                        Object.assign(this.$data, initialSellPage({message: "Success!"}));
                    }
                })
                .catch((error) => {
                    throw error;
                });
        },

        getAsyncData: _.debounce (function (input) {
            // The search input autocomplete of sell.html.
            if (!input.length) {
                this.suggestions = [];
                return;
            }
            //
            // Is the user inputting a number, is it an ISBN?
            //
            if (/^\d+$/.test(input)) {
                if (input.length < 13) {
                    this.suggestions = [];
                } else {
                    //Asynchronous search for ISBN happens here:
                    //
                    // Add a result in the UI straight away, only showing the input text.
                    var cardindex = this.newBook({input, error: null, card: null});
                    // Search for the bibliographic data.
                    this.$http.get(`/api/sell-search?q=${input}`)
                        .then(({ data }) => {
                            // console.log("async book result: ", data);
                            var book = {input: this.books[cardindex].input,
                                        show: true,
                                        quantity: 1,
                                        card: null,
                                        url: "#",
                                        error: null};
                            if (data && data.hasOwnProperty("card")) {
                                book.card = data.card;
                            } else if (data && data.hasOwnProperty("error")) {
                                book.error = data.error;
                            }
                            // Save.
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
            //
            // Normal search (not ISBN).
            //
            this.isFetching = true;
            this.$http.get(`/api/sell-search?q=${input}`)
                .then(({ data }) => {
                    console.log("sell search results: ", data);
                    // If we get one unique result: add it.
                    if (data && data.hasOwnProperty("card")) {
                        this.newBook({ card: data.card, input });
                    } else if (data && data.hasOwnProperty("options")) {
                        // We can get a list of results.
                        this.suggestions = [];
                        data.options.forEach((item) => this.suggestions.push(item));
                    } else if (data && data.hasOwnProperty("error")) {
                        // Deal with errors.
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
            // The item to sell is selected amongst the result list.
            if (item) {
                // console.log("--- itemSelected: ", item, item.title, item.url, "card: ", item.card);
                this.newBook({ card: item.card, input: this.search, url: item.url });
            }
        },

        removeBook: function (index) {
            //book isn't removed, just hidden
            this.books[index].show = false;
            this.focusInput();
        },

        newBook: function (params) {
            // Push a new object on this.books.
            // - show: true
            // - quantity: 1
            // - inherit the card data.
            //
            // Return: this new book index.
            params.show = true;
            params.quantity = 1;
            this.books.push(params);
            setTimeout(function() {this.search = ""; this.unsaved = true;}.bind(this));
            return this.books.length - 1;
        },

        focusInput: function (params) {
            document.getElementById('default-input').focus();
        },
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
    },

    mounted: function () {
        document.getElementById('default-input').focus();
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
