;; @module Request
;; @author Jeff Ober <jeffober@gmail.com>, Kanen Flowers <kanendosei@gmail.com>
;; @version 1.1
;; @location http://static.artfulcode.net/newlisp/request.lsp
;; @package http://static.artfulcode.net/newlisp/request.qwerty
;; @description Request module to replace input functions in the standard CGI module (updated for newlisp 10)
;; 'Request' encapsulates the processing of CGI environmental variables in
;; a different manner than the standard CGI library.  POST and GET variables
;; are separated, and the url is parsed into a list of segments in order to
;; make use with .htaccess files and mod_rewrite simpler.
;; 
;; POST data can only be read once, after which it becomes unavailable
;; to future parts of the program.  Should the default cgi.lsp module precede
;; this module, POST data will be unavailable through the Request class.  If
;; cgi.lsp is loaded after this module, POST data is unavailable to it.
;; 
;; This module does not include output functions, including setting cookies.
;; Output is a function of the Response class.
;; <h4>Version history</h4>
;; <b>1.1</b>
;; &bull; fixed incompatibilites with newlisp 10
;; 
;; <b>1.0</b>
;; &bull; initial release

(constant '*max-post-length* 4096)
(global '*max-post-length*)

(context 'Request)

(setq _cgi-keys '("REDIRECT_STATUS" "HTTP_HOST" "HTTP_USER_AGENT" "HTTP_ACCEPT"
  "HTTP_ACCEPT_LANGUAGE" "HTTP_ACCEPT_ENCODING" "HTTP_ACCEPT_CHARSET"
  "HTTP_KEEP_ALIVE" "HTTP_CONNECTION" "HTTP_COOKIE" "HTTP_CACHE_CONTROL" "PATH"
  "SERVER_SIGNATURE" "SERVER_SOFTWARE" "SERVER_NAME" "SERVER_ADDR" "SERVER_PORT"
  "REMOTE_ADDR" "DOCUMENT_ROOT" "SERVER_ADMIN" "SCRIPT_FILENAME" "REMOTE_PORT"
  "REDIRECT_URL" "GATEWAY_INTERFACE" "SERVER_PROTOCOL" "REQUEST_METHOD"
  "QUERY_STRING" "REQUEST_URI" "SCRIPT_NAME" "PATH_INFO" "PATH_TRANSLATED"))

;;; set cleaned CGI environment parameters
(setq _cgi-env (map (fn (key) (list key (trim (string (env key))))) _cgi-keys))

;;; set default method
(setq _method "get")

(define (parse-query query-string)
  (let ((params '()))
    (dolist (element (parse query-string "&"))
      (let ((pair (parse element "=")))
        (if (= 1 (length pair)) (push nil pair -1))
        (push pair params -1)))
    params))

;;; deal with GET params from QUERY_STRING
(setq _get (parse-query (lookup "QUERY_STRING" _cgi-env)))

;;; deal with POST params from stdin data
(read-buffer (device) post-data *max-post-length*)
(setq _post '())
(when post-data
  (setq _method "post")
  (setq _post (parse-query post-data)))

;;; deal with HTTP_COOKIE data
(setq _cookies '())
(dolist (element (parse (lookup "HTTP_COOKIE" _cgi-env) ";"))
  (let ((pair (parse element "=")))
    (push pair _cookies -1)))

;;; store segment information
(setq _segments (parse (trim (lookup "REQUEST_URI" _cgi-env) "/") "/"))

;; @syntax (Request:method)
;; @return string
;; <p>Returns the request method.</p>
(define (method)
  _method)

;; @syntax (Request:get?)
;; @return boolean
;; <p>Predicates that the request method is GET.</p>
(define (get?)
  (= _method "get"))

;; @syntax (Request:post?)
;; @return boolean
;; <p>Predicates that the request method is POST.</p>
(define (post?)
  (= _method "post"))

;; @syntax (Request:get <str-key>)
;; @param <str-key> a GET variable
;; <p>Fetches a GET variable from the query string.</p>
(define (get key)
  (if key
    (lookup key _get)
    _get))

;; @syntax (Request:post <str-key>)
;; @param <str-key> a POST variable
;; <p>Fetches a POST variable from the request content.</p>
(define (post key)
  (if key
    (lookup key _post)
    _post))

;; @syntax (Request:cookies <str-key>)
;; @param <str-key> a COOKIE variable
;; <p>Fetches the value of a cookie.</p>
(define (cookies key)
  (if key
    (lookup key _cookies)
    _cookies))

;; @syntax (Request:cookie? <str-key>)
;; @param <str-key> a COOKIE variable
;; <p>Predicates that a COOKIE variable is set.</p>
(define (cookie? key)
  (lookup key _cookies))

(setq _current-segment -1)
(define (segment)
  (unless (>= _current-segment (- (length _segments) 1))
    (nth (if (args) (args 0) (inc '_current-segment)) _segments)))

;; @syntax (Request:segments)
;; @return list
;; <p>Returns a list of each part of the query path, e.g.
;; /foo/bar/baz becomes '("foo" "bar" "baz")'.</p>
(define (segments)
  _segments)

(setq path (lookup "REQUEST_URI" _cgi-env))
(setq domain (lookup "HTTP_HOST" _cgi-env))

(context MAIN)

