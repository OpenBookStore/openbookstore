
console.log("--- card page: watching Shelf change.");

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
        let card_id = url_id(window.location.pathname);
        let url = "/api/card/update?cardId=" + card_id + "&shelfId" + shelf_id;
        fetch(url, {
            method: 'POST',
            headers: new Headers({
                // *not* json, so it works out of the box with my Hunchentoot handlers.
                'Content-Type': 'application/x-www-form-urlencoded',
            }),
            // body is reserved for the review text.
            // body: ""
        })
            .then((response) => {
                return response.json();
            })
            .then((myJson) => {
                if (myJson.status == 200) {
                    Notiflix.Notify.Success('OK');
                }

                // Update the link to filter the stock.
                let link_elt = document.getElementById('link-stock-shelf');
                if (link_elt) {
                    link_elt.href = "/stock?shelf=" + shelf_id;
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

function save_review(textid) {
        // Get the editor element equal to the given textid.
        //
        // We might use more than one editor fields.
        // And this is a copy-paste ;)
        let card_id = url_id(window.location.pathname);
        let url = "/api/card/update?cardId=" + card_id;

        let editor = document.getElementById(textid);
        let review = editor.innerHTML;

        if (textid) {
            url += "&textid=" + textid;
        }

        // TODO: handle CSRF token.
        // Use hunchentools library?
        // let token = document.getElementById('api-token').innerText;
        // console.log("token: ", token);

        fetch(url, {
            method: 'POST',
            headers: new Headers({
                // 'api-token': token,  // unused, handle CSRF.
            }),
            body: review
        })
            .then((response) => {
                console.log("response is ", response);
                return response.json();
            })
            .then((myJson) => {
                if (myJson.status == 200 || myJson.status == "success") {
                    console.log("-- success.");;
                    Notiflix.Notify.Success('OK');
                }
                else {
                    console.log("status is not success: ", myJson.status);
                    Notiflix.Notify.Warning(myJson.message);
                }
            })
            .catch((error) => {
                console.error('There has been a problem with your fetch operation:', error);
                Notiflix.Notify.Warning("An error occured. We have been notified.");
            });

    };
