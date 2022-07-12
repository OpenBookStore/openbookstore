
console.log("--- card page: watching Shelf change.");

(function() {
    let shelves = [];
    let shelf_select = document.getElementById('shelf-select');

    function url_id (url) {
        // extract an id
        let re = /\/(\d+)/;
        let res = url.match(re);
        if (res && res.length == 2) {
            return res[1];
        }
        return null;
    };

    if (shelf_select) {
        console.log("-- adding an event listener on the Shelf select.");
        shelf_select.addEventListener('change', (event) => {
            let options = shelf_select.options;
            let pk = parseInt(event.target.value);
            // if (pk !== -1 && shelves[pk] !== undefined) {
                // let shelf = shelves[pk];
                // pk = shelf.pk;
            // }
            update_shelf(pk);
            console.log("new_shelf: ", event.target.value, pk);
        });
    }

    function update_shelf(shelf_id) {
        console.log("--- save shelf…");
        let card_id = url_id(window.location.pathname);
        let url = "/api/card/update";
        let json_body = '{"card_id": ' + card_id + ', ' +
            '"shelf_id": ' + shelf_id +
            '}';
        fetch(url, {
            method: 'POST',
            headers: new Headers({
                // *not* json, so it works out of the box with my Hunchentoot handlers.
                'Content-Type': 'application/x-www-form-urlencoded', // <-- Specifying the Content-Type
            }),
            body: "cardId=" + card_id + "&shelfId=" + shelf_id,
        })
            .then((response) => {
                return response.json();
            })
            .then((myJson) => {
                if (myJson.status == 200) {
                    Notiflix.Notify.Success('OK');
                }
                else {
                    Notiflix.Notify.Warning('Something happened');
                    console.log("status is not success: ", myJson.status);
                }
            })
            .catch((error) => {
                console.error('There has been a problem with your fetch operation:', error);
            });
    };

}
)();
