
(in-package :openbookstore/web)

#|

Status: les websockets marchent, mais je n'arrive pas à afficher un livre PUIS à remplacer le div associé.

L'élément de DOM ajouté est seulement le div class=media, et pas le div class=card et id=card-x

càd la 2nd target de remplacement n'y est pas.


Pour tester:

https://www.piesocket.com/websocket-tester

|#

(defvar *clients-ws* nil
  "List of connected WS")

(defparameter *html-card*
  "
        <div id=\"~a\" hx-swap-oob=\"~a\"
         <div class=\"card\" id=\"~a\"
          <div class=\"card-content\">
            <div class=\"media\">
              <div class=\"media-left\">
                <figure class=\"image is-48x48\">
                  <img src=\"https://bulma.io/images/placeholders/96x96.png\" alt=\"Placeholder image\">
                </figure>
              </div>
              <div class=\"media-content\">
                <p class=\"title is-5\"> ~a
                </p>
                <p class=\"subtitle is-6\"> counter: ~a  </p>
                <p class=\"subtitle is-6\">  </p>

                <p class=\"subtitle is-6\">
                   9.90 €
                </p>
              </div>
            </div>
            <div class=\"content\">
            </div>
          </div>
         </div>
        </div>

")

(defun make-htmx-message (msg &key replace counter)
  "Return a HTML snippet with somehow the id and title."
  (let* ((cnt (or counter
                 (incf *counter*)))
         (card-id (format nil "card-~a" cnt)))
    (format nil *html-card*
            (if replace card-id "id-livres")
            (if replace "true" "afterend")
            card-id
            msg
            cnt)))

(defun broadcast (msg)
  ;; not by default in Portal, why not?
  ;;
  ;; broadcast n'est pas nécessaire pour cette page,
  ;; mais c'est bien pratique pour tester avec une autre fenêtre piesocket tester et voir
  ;; si les messages sont reçus.
  (loop for client in *clients-ws* do
       (pws:send client msg)))

(defvar *counter* 0
  "Counter for the DIV to update.")

(pws:define-resource "/echo"
    :open (lambda (websocket)
            ;; On receive, display a simple message for a ACK (testing).
            (push websocket *clients-ws*)
            (setf *counter* 0)
            (pws:send websocket
                      "<div id=\"body\"> test </div>
                    <div id=\"notifications\" hx-swap-oob=\"true\"> hello from ws app ! </div>"))
    :message (lambda (websocket message)
               (handler-case
                   (let* ((counter (incf *counter*))
                          (json (shasht:read-json message))
                          (msg (access json "chat_message")))
                     ;; Envoyer un 1er message avec un id égale à "card-<counter>",
                     ;; qui est retourné de suite.
                     (broadcast (make-htmx-message msg :counter counter))
                     ;; Puis lancer une thread qui est censée mettre à jour ce premier snippet.
                     (bt:make-thread
                      (lambda ()
                        (sleep 1)
                        ;; (broadcast (format nil "<div id=\"card-~a\" hx-swap-oob=\"true\"> Message is: ~a and counter is ~a </div>" counter msg counter))

                        ;; Ici on essaie de remplacer le 1er snippet avec un nouveau titre.
                        (broadcast (print (make-htmx-message
                                           (format nil "nouveau titre ~a" counter)
                                           :counter counter
                                           :replace t))))
                      :name "ws-child-thread")

                     (pws:send websocket (format nil "<div id=\"notifications\" hx-swap-oob=\"afterbegin\"> Exited right away. Message was: ~a </div>" msg))
                     )

                 (sb-int:closed-stream-error (c)
                   ;; Quand on met le PC en pause, les connexions sont fermées,
                   ;; mais il faut gérer ça à la mano.
                   (declare (ignore c))
                   ;; see also pws:*debug-on-error* ?
                   (format t "websocket is closed, removing from clients: ~S" websocket)
                   ;; Il y a un restart dans Portal qui permet de clore la socket,
                   ;; à étudier pour l'appeler par défaut…
                   (pws::call-resource-function :close websocket)
                   (setf *clients-ws* (remove websocket *clients-ws*))
                   ))

               ;; Premiers tests simples.
               ;; (pws:send websocket (format nil "message 1: ~a" message))
               ;; (pws:send websocket
               ;;           "<div id=\"notifications\" hx-swap-oob=\"true\"> message 1 </div>")
               ;; (broadcast
               ;;  "<div id=\"notifications\" hx-swap-oob=\"afterbegin\"> message 1 </div>")
               ;; (sleep 1)

               )

    :close (lambda (websocket)
             (declare (ignorable websocket))
             (format t "~&--- ws list: ~S" *clients-ws*)
             (format t "~&--- client leaving: ~S" websocket)
             ;; Gérer la liste de clients… pas besoin pour la page Réception.
             (setf *clients-ws* (remove websocket *clients-ws*))
             (format t "~&--- ws list after removal: ~S" *clients-ws*)))

(defparameter ws-server nil)

(defun start-ws ()
  (setf ws-server (pws:server 4432 t)))
