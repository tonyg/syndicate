#lang syndicate

(require "../../widgets.rkt")
(require (only-in racket/string string-prefix?))
(require (only-in racket/function curry))
(require (only-in racket/list first rest))

;; a create-read-update-deleted MVC implementation 

;; ---------------------------------------------------------------------------------------------------
(define frame   (spawn-frame #:label "CRUD"))
(define hpane1  (spawn-horizontal-pane #:parent frame #:border 10 #:alignment '(left bottom)))
(define vpane1  (spawn-vertical-pane #:parent hpane1))
(define filter-tf (spawn-text-field #:parent vpane1 #:label "Filter prefix: " #:init-value ""))
(define lbox    (spawn-list-box #:parent vpane1 #:label #f #:choices '() #:min-width 100 #:min-height 100))
(define vpane2  (spawn-vertical-pane #:parent hpane1 #:alignment '(right center)))
(define name    (spawn-text-field #:parent vpane2 #:label "Name:      " #:init-value "" #:min-width 200))
(define surname (spawn-text-field #:parent vpane2 #:label "Surname: " #:init-value "" #:min-width 200))
(define hpane2  (spawn-horizontal-pane #:parent frame))
(define create-but (spawn-button #:label "Create" #:parent hpane2))
(define update-but (spawn-button #:label "Update" #:parent hpane2))
(define delete-but (spawn-button #:label "Delete" #:parent hpane2))

(spawn
 (field [*data '("Emil, Hans" "Mustermann, Max" "Tisch, Roman")]
        [*selector ""]
        [*selected (*data)]) ;; selected = (filter select data)
 ;; ---------------------------------------------------------------------------------------------------
(define (selector! nu) (*selector nu) (data->selected!))
(define (select s) (string-prefix? s (*selector)))
(define (data->selected!)  (*selected (if (string=? "" (*selector)) (*data) (filter select (*data)))))

(define-syntax-rule (def-! (name x ...) exp) (define (name x ...) (*data exp) (data->selected!)))
(def-! (create-entry new-entry) (append (*data) (list new-entry)))
(def-! (update-entry new-entry i) (operate-on i (curry cons new-entry) (*data) select (*selected)))
(def-! (delete-from i) (operate-on i  values))

#; {N [[Listof X] -> [Listof X]] [Listof X] [X -> Boolean] [Listof X] -> [Listof X]}
;; traverse list to the i-th position of selected in data, then apply operator to rest (efficiency)
;; ASSUME selected = (filter selector data)
;; ASSUME i <= (length selected)
(define (operate-on i operator [data (*data)] [select select] [selected (*selected)])
  (let sync ((i i) (data data) (selected selected))
    (if (select (first data))
        (if (zero? i)
            (operator (rest data))
            (cons (first data) (sync (sub1 i) (rest data) (rest selected))))
        (cons (first data) (sync i (rest data) selected)))))

;; ---------------------------------------------------------------------------------------------------
(define-syntax-rule (def-cb (name x) exp ...) (define (name x) exp ... (send! (set-list-box-choices lbox (*selected)))))
(def-cb (prefix-cb prefix) (selector! prefix))
(def-cb (Create-cb _b) (create-entry (retrieve-name)))
(def-cb (Update-cb _b) (common-cb (curry update-entry (retrieve-name))))
(def-cb (Delete-cb _b) (common-cb delete-from))

(on (message (text-field-update filter-tf $prefix)) (prefix-cb prefix))
(on (message (button-press create-but)) (Create-cb create-but))
(on (message (button-press update-but)) (Update-cb update-but))
(on (message (button-press delete-but)) (Delete-cb delete-but))

(define/query-value current-selection #f (list-box@ lbox $selection) selection)
(define/query-value *surname "" (text-field@ surname $val) val)
(define/query-value *name "" (text-field@ name $val) val)

(local-require 7GUI/should-be-racket)
(define (common-cb f) (when* (current-selection) => f))
(define (retrieve-name) (string-append (*surname) ", " (*name)))

(on-start (prefix-cb "")
          (send! (show frame #t))))
