
console.log("Hello openbookstore from .js");

Vue.use(Buefy.Autocomplete);

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
            // Is the user input a number?
            if (/^\d+$/.test(name) && name.length < 13) {
                this.data = [];
                return;
            }
            this.isFetching = true;
            this.$http.get(`/quick-search?q=${name}`)
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


