(defpackage #:bottomfeeder-feeds
  (:use #:common-lisp)
  (:export #:feed-items))


(in-package #:bottomfeeder-feeds)


(require :trivial-http)
(require :s-xml)


(defclass feed-parser-seed ()
  ((ready-items :initform nil
                :accessor ready-items
                :documentation "List of items that are already ready.")
   (in-item :initform nil
            :accessor in-item
            :documentation "Whether we are currently in an item.")
   (in-link :initform nil
            :accessor in-link
            :documentation "Whether we are currently in an link.")
   (in-title :initform nil
            :accessor in-title
            :documentation "Whether we are currently in an title.")
   (current-title :accessor current-title)
   (current-link :accessor current-link)))


(defun begin-item (seed)
  (if (or (in-item seed)
          (in-title seed)
          (in-link seed)
          (slot-boundp seed 'current-title)
          (slot-boundp seed 'current-link))
      (error "Faulty seed state ~s" seed)
      (setf (in-item seed) t)))


(defun end-item (seed)
  (push (cons (current-title seed) (current-link seed)) (ready-items seed))
  (slot-makunbound seed 'current-title)
  (slot-makunbound seed 'current-link)
  (setf (in-item seed) nil))


(defun name-equal (name1 name2)
  (flet ((tostr (name)
           (etypecase name
             (string name)
             (symbol (symbol-name name)))))
    (string-equal (tostr name1) (tostr name2))))


(defun new-element-hook (name attributes seed)
  (declare (ignore attributes))
  (prog1 seed
    (cond
      ((or (name-equal "item" name)
           (name-equal "entry" name))
       (begin-item seed))
      ((name-equal "link" name)
       (setf (in-link seed) t))
      ((name-equal "title" name)
       (setf (in-title seed) t)))))


(defun finish-element-hook (name attributes parent-seed seed)
  (declare (ignore attributes parent-seed))
  (prog1 seed
    (cond
      ((or (name-equal "item" name)
           (name-equal "entry" name))
       (end-item seed))
      ((name-equal "link" name)
       (setf (in-link seed) nil))
      ((name-equal "title" name)
       (setf (in-title seed) nil)))))


(defun text-hook (string seed)
  (prog1 seed
    (cond
      ((and (in-item seed)
            (in-title seed))
       (setf (current-title seed) string))
      ((and (in-item seed)
            (in-link seed))
       (setf (current-link seed) string)))))


(defun feed-items (url)
  "Return a list of items of the form (([title] . [url]) ...) from ATOM or RSS feed."

  (with-open-stream (stream (nth 2 (trivial-http:http-resolve url)))
    (ready-items (s-xml:start-parse-xml stream (make-instance 's-xml:xml-parser-state
                                                              :seed (make-instance 'feed-parser-seed)
                                                              :new-element-hook 'new-element-hook
                                                              :finish-element-hook 'finish-element-hook
                                                              :text-hook 'text-hook)))))




;;; test stuff

;; (defparameter *feed-urls*
;;   (list
;;    "http://www.hs.fi/uutiset/rss/"
;;    "http://feeds2.feedburner.com/cyclingnews/news"))

;(mapcar 'feed-items *feed-urls*)
