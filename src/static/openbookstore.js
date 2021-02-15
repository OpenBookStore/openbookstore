Vue.use(Buefy.Autocomplete);
Vue.use(Buefy.Input);
Vue.use(Buefy.Numberinput);
Vue.use(Buefy.Field);
Vue.use(Buefy.Button);

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
      <div class='media-right'> \
          <div class='level'> \
              <span class='level-item' name='price'> {{ card.price }} €</span> \
          </div> \
      </div> \
    </div> \
"
})

const sellPage = {
    data () {
        return {
            books: [{error: null, card: null, input: "", quantity: 1, show: false}],
            suggestions: [],
            search: "",
            selected: null,
            isFetching: false
        };
    },
    methods: {
        getAsyncData: _.debounce (function (input) {
            if (!input.length) {
                this.suggestions = [];
                return;
            }
            // Is the user inputting a number?
            if (/^\d+$/.test(input)) {
                if (input.length < 13) {
                    this.suggestions = [];
                    return;
                } else {
                    this.newCard({input});
                }
            }
            this.isFetching = true;
            this.$http.get(`/api/sell-search?q=${input}`)
                .then(({ data }) => {
                    if (data && data.hasOwnProperty("card")) {
                        this.newCard({ card: data.card, input });
                    } else if (data && data.hasOwnProperty("options")) {
                        this.suggestions = [];
                        data.options.forEach((item) => this.suggestions.push(item));
                    } else if (data && data.hasOwnProperty("error")) {
                        this.newCard({ error: data.error, input });
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
                this.newCard({ card: item.card });
            }
        },
        removeBook: function (index) {
            this.books[index].show = false;
        },
        newCard: function (params) {
            params.show = true;
            params.quantity = 1;
            this.books.push(params);
            setTimeout(function() {this.search = "";}.bind(this));
        }
    }
}


if (document.getElementById('vue-sell')) {
    var vsell = new Vue(sellPage);
    vsell.$mount('#vue-sell');
}


