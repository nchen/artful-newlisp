;; @module Response
;; @author Jeff Ober <jeffober@gmail.com>
;; @version 1.2.1
;; @location http://static.artfulcode.net/newlisp/response.lsp
;; @package http://static.artfulcode.net/newlisp/response.qwerty
;; @description Response module to replace output functions in the standard CGI module (updated for newlisp 10)
;; This module is independent of the Http module, and duplicates some of the effort there
;; by providing the ability to write headers to the HTTP output (such as the content-type and
;; the ability to redirect).  It is meant to be used with an existing CGI serving application,
;; such as Apache.
;;
;; Output is controlled using 'Response:write' or default functor (`Response:Response`),
;; which formats the output as a CGI response.  The result of 'Response:Response' is a
;; formatted string.  It is the application's responsibility to print this string or pass
;; it to the appropriate middle-man.
;; 
;; <h4>Version history</h4>
;; <b>1.2.1</b>
;; &bull; replaced deprecated set-assoc with setf in Response:header
;; 
;; <b>1.2</b>
;; &bull; fixed incompatibilities with newlisp 10
;; 
;; <b>1.1</b>
;; &bull; fixed bug in 'headers'
;; &bull; fixed bug forcing text/html on all responses
;; 
;; <b>1.0</b>
;; &bull; initial release

(context 'Response)

(set '_response-codes
	 '((200 "OK")
	   (302 "Found")
	   (404 "Not Found")
	   (500 "Internal Error")))

(set '_response-code 200)
(set '_content-type "text/html; charset=utf-8")
(set '_headers '(("Content-type" "text/html; charset=utf-8")))
(set '_cookies '())
(set '_content "")

;;; functions for setting headers, adding content, and predicates.

;; @syntax (Response:write <str-text> ...)
;; @param <str-text> text to write to the output buffer
;; <p>'Response:write' allows programmatic output of CGI content.</p>
;; @example
;; (Response:write "<html>\n")
;; (Response:write "<head><title>Test page</title></head>\n")
;; (Response:write "<body><h3>Hello world</h3></body>\n")
;; (Response:write "</html>")
(define (Response:write)
  (doargs (a) (write-buffer _content (string a))))

;; @syntax (Response:header <str-key> <str-val>)
;; @param <str-key> header name
;; @param <str-val> header content
;; <p>Adds a new header to the output.</p>
(define (header key val)
  (set 'key (title-case key))
  (if (assoc key _headers)
	    (setf (assoc key _headers) (list key val))
	    (push (list key val) _headers)))

;; @syntax (Response:header? <str-key>)
;; @param <str-key> header name
;; <p>Predicates that a header has already been added for output.</p>
(define (header? key)
  (lookup key _headers))

;; @syntax (Response:headers)
;; <p>Returns an association list of headers that have been set.</p>
(define (headers)
  _headers)

(define (format-cookie key value domain path expires)
  ;; expires must be timestamp
  (set 'value (if value (string value) ""))
  (let ((cookie ""))
    (write-buffer cookie (format "%s=%s;" key value))
    (if domain
		    (write-buffer cookie (format "; domain=.%s" domain)))
    (if path
		    (write-buffer cookie (format "; path=%s" path)))
    (if expires
		    (write-buffer cookie (format "; expires=%s" (date (int expires) 0 "%a, %d %b %Y %H:%M:%S %Z"))))
    cookie))

;; @syntax (Response:set-cookie <str-key> <str-value> [<str-domain> [<str-path> [<int-expires>]]])
;; @param <str-key> the cookie's name
;; @param <str-value> the cookie's value
;; @param <str-domain> optional; the cookie's domain
;; @param <str-path> optional; the cookie's path
;; @param <int-expires> optional; integer timestamp suitable as an input to 'date'
;; <p>Adds a cookie to the output buffer.</p>
;; @example
;; (let ((sid (new-session-id)))
;;   (Response:set-cookie "session-id" sid nil nil (+ (date-value) (* 60 60))))
;; 
;; => sets cookie "session-id" to the result of the function 'new-session-id' and
;;    an expiration date one hour in the future.
(define (set-cookie key value domain path expires)
  (if (cookie-set? key '? domain path)
	  (delete-cookie key domain path))
  (push (list key value domain path expires) _cookies -1))

;; @syntax (Response:delete-cookie <str-key> [<str-domain> [<str-path>]])
;; @param <str-key> the cookie's name
;; @param <str-domain> optional; the cookie's domain
;; @param <str-path> optional; the cookie's path
;; <p>Deletes a cookie with matching key/domain/path that has been previously set.</p>
;;; TODO: needs to check for set cookies in _cookies and remove
(define (delete-cookie key domain path)
  (if (cookie-set? key '? domain path)
	  (pop _cookies (find (list key '? domain path '?) _cookies match))
	  (set-cookie key nil domain path (date-value))))

;; @syntax (Response:cookie-set? <str-key> [<str-domain> [<str-path>]])
;; @param <str-key> the cookie's name
;; @param <str-domain> optional; the cookie's domain
;; @param <str-path> optional; the cookie's path
;; <p>Predicates whether a cookie with a matching pattern has been previously set.</p>
(define (cookie-set? key domain path)
  (true? (find (list key '? domain path '?) _cookies match)))

;;; utility functions for rendering of headers

(define (_write-cookies)
  (if-not (null? _cookies)
    (let ((buffer ""))
      (dolist (cookie _cookies)
        (set 'cookie (apply format-cookie cookie))
        (write-buffer buffer (string "Set-Cookie: " cookie "\n")))
      buffer)
    ""))

(define (_write-content str)
  (if str (set '_content str))
  (if _content
	    (let ((buffer ""))
		    (write-buffer buffer (string "Content-Length: " (length _content) "\n\n"))
		    (write-buffer buffer (string _content))
		    buffer)
	      ""))

(set '_response
  (lambda (str)
    (let ((buffer ""))
      (let ((response-text (lookup _response-code _response-codes)))
        (write-buffer buffer (string "Status: " _response-code " " (lookup _response-code _response-codes) "\n"))
        (dolist (hdrs _headers)
          (write-buffer buffer (string (join hdrs ": ") "\n")))
        (write-buffer buffer (_write-cookies))
        (write-buffer buffer (_write-content str))
        buffer))))

;;; functions that render complete headers.  functions that need to override the
;;; default rendering of complete headers simply need to replace the lambda
;;; _response which returns a string containing the complete header.

;; @syntax (Response:Response [<str-text>])
;; @param <str-text> optional; string text to output
;; <p>The default functor will return a formatted response, suitable for CGI output.
;; If <str-text> is provided, this will be the output.  Otherwise, any content added
;; with 'Response:write' will be used.</p>
;; <p>Output will include any necessary headers, including cookies, that were set
;; for this session.</p>
;; <p>This module is not re-entrant, so in a stateful environment, it should be
;; prototyped using 'new' for each new response.</p>
;; <p>The manner in which the response is rendered may be hacked by setting the
;; variable 'Response:_response' to a suitable lambda expression.  See the code for
;; more implementation details.</p>
;; @example
;; (Response "Hello world")
;; => "Status: 200 OK\nContent-type: text/html; charset=utf-8\nContent-Length: 11\n\nHello world"
;; 
;; (Response:write "Hello world")
;; (Response)
;; => "Status: 200 OK\nContent-type: text/html; charset=utf-8\nContent-Length: 11\n\nHello world"
(define (Response:Response str)
  (_response str))

;; @syntax (Response:redirect <str-path>)
;; @param <str-path> the url or path to redirect to
;; <p>Redirects the client to <str-path>.</p>
;; @example
;; (Response:redirect "/some/other/path")
;; => "Status: 302 Found\nLocation: /some/other/path\n\n"
(define (redirect path)
  (set '_response-code 302)
  (header "Location" path)
  (set '_response
	   (expand 
		'(lambda ()
		   (string "Status: 302 Found\n" (_write-cookies) "Location: " path 
				   "\n\n"))
		'path))
  (_response))

;;; 404 and 500 errors accept a string to output as the 404 page or 500 page.
;;; These are useful as primitives for more sophisticated error-catching
;;; routines that output formatted error and not-found pages.

;; @syntax (Response:not-found <str-message>)
;; @param <str-message> 404 NOT FOUND error message to display
;; <p>Outputs a 404 NOT FOUND error message.</p>
;; @example
;; (Response:not-found "<p>These are not the droids you are looking for.</p>")
;; => "Status: 404 Not Found\nContent-type: text/html; charset=utf-8\nContent-Length: 52\n\n<p>These are not the droids you are looking for.</p>"
(define (not-found str)
  (set '_response-code 404)
  (set '_content str)
  (_response))

;; @syntax (Response:error <str-message>)
;; @param <str-message> 500 ERROR message to display
;; <p>Outputs a 500 ERROR message.</p>
;; @example
;; (Response:error "<p>An error occurred while processing this request.</p>")
;; => "Status: 500 Internal Error\nContent-type: text/html; charset=utf-8\nContent-Length: 55\n\n<p>An error occurred while processing this request.</p>"
(define (error str)
  (set '_response-code 500)
  (set '_content str)
  (_response))

(context MAIN)
