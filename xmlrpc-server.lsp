;; @module Xmlrpc-server
;; @author Jeff Ober <jeffober@gmail.com>
;; @version 1.1
;; @location http://static.artfulcode.net/newlisp/xmlrpc-server.lsp
;; @package http://static.artfulcode.net/newlisp/xmlrpc-server.qwerty
;; @description A simple XML-RPC server (updated for newlisp 10).
;; Xmlrpc-server implements a basic XML-RPC server. It requires the element,
;; http, sockets, and util modules, and newlisp 10.
;; 
;; Xmlrpc-server is able to convert data between xmlrpc and newLISP. Any uncaught
;; errors that occur during the loading of a response are handled by returning a fault
;; response. Fault codes are gleaned from
;; @link http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php Dan&nbsp;Libby's&nbsp;specifications.
;;
;; The server is used by registering <handler> functions that handle requests to
;; a specific path. Only one introspection function, <system.listMethods>, is included.
;; 
;; <h4>Version history</h4>
;; <b>1.1</b>
;; &bull; updated for newlisp 10
;; &bull; updated to use element module for faster xml serialization
;; 
;; <b>1.0</b>
;; &bull; initial release
;; 
;; @example
;; (load "xmlrpc-server.lsp")
;; 
;; (define (get-time)
;;   (date (date-value)))
;; 
;; (Xmlrpc-server:add-to-registry "myapp.getTime" get-time)
;; (Xmlrpc-server:run-server 8080 "/RPC2")

(context 'Xmlrpc-server)

;;; Error codes
(constant
  'NOT-WELL-FORMED -32700
  'INVALID-XML-RPC -32600
  'METHOD-NOT-FOUND -3260
  'INVALID-PARAMETERS -32602
  'APPLICATION-ERROR -32500
  'SERVER-ERROR -32603
  'OTHER-ERROR -32099)

(setf error-codes
  '((NOT-WELL-FORMED (-32700 "Parse error: XML not well formed"))
    (INVALID-XML-RPC (-32600 "Parse error: Invalid XML-RPC"))
    (METHOD-NOT-FOUND (-32601 "Server error: Method not found"))
    (INVALID-PARAMETERS (-32602 "Server error: Invalid method parameters"))
    (APPLICATION-ERROR (-32500 "Application error"))
    (SERVER-ERROR (-32400 "Server error"))
    (OTHER-ERROR (-32099 "An error has occurred"))))

;;; Parsing XML-RPC into Lisp expressions
(define (parse-request xml , old-type-tags parsed call)
  (setf old-type-tags (xml-type-tags))
  (xml-type-tags nil nil nil nil)
  (setf parsed (xml-parse xml (+ 1 2 4 8)))
  (apply xml-type-tags old-tags)
  
  (when (or (xml-error) (not (assoc 'methodCall parsed)))
    (throw-error (if (xml-error) NOT-WELL-FORMED INVALID-XML-RPC)))
  
  (setf call (rest (assoc 'methodCall parsed)))
  (list (lookup 'methodName call)
        (map xmlrpc->lisp (rest (assoc 'params call)))))

(define (xmlrpc-value->lisp value (type-param string))
  (case type-param
    (string (XML:decode (string value)))
    (i4 (int value 0 10))
    (int (int value 0 10))
    (double (float value))
    (boolean (= "1" value))
    (base64 (base64-dec value))
    (true (XML:decode (string value)))))

(define (xmlrpc->lisp expr , m)
  (cond
    ((setf m (match '(param (value (? ?))) expr))
     (xmlrpc-value->lisp (m 1) (m 0)))
    ((setf m (match '(param (value ?)) expr))
     (xmlrpc-value->lisp (m 0)))
    ((setf m (match '(param (array (data *))) expr))
     (map 'xmlrpc->lisp (map list (dup 'param (length (m 0))) (m 0))))
    ((setf m (match '(param (struct *)) expr))
     (map (fn (x)
            (list (lookup 'name x)
                  (xmlrpc->lisp (list 'param (assoc (x 'value))))))
          (m 0)))
    (true (string "???: " expr))))

;;; Generating XML-RPC from Lisp expressions
(define (array->xmlrpc e)
  (list->xmlrpc (array-list e)))

(define (list->xmlrpc e)
  (Element "array" nil
    (Element "data" nil
      (join (map lisp->value e)))))

(define (context->xmlrpc e)
  (Element "struct" nil
    (map (fn (key)
           (Element "member" nil
             (Element "name" nil key)
             (lisp->value (e key))))
         (keys e))))

(define (lisp->value expr)
  (case (type-of expr)
    ("integer" (Element "value" nil (Element "int" nil expr)))
    ("float" (Element "value" nil (Element "double" nil expr)))
    ("boolean" (Element "value" nil (Element "boolean" nil (if (true? expr) 1 0))))
    ("array" (Element "value" nil (array->xmlrpc expr)))
    ("list" (Element "value" nil (list->xmlrpc expr)))
    ("context" (Element "value" nil (context->xmlrpc expr)))
    ("symbol" (Element "value" nil (Element "string" nil (Element:encode (name expr)))))
    (true (Element "value" nil (Element "string" nil (Element:encode expr))))))

(define (lisp->xmlrpc-params expr)
  (Element "params" nil
    (if (atom? expr)
        (Element "param" nil (lisp->value expr))
        (apply 'string (map (fn (v) (Element "param" nil v))
                            (map lisp->value expr))))))

(define (response str)
  (Element:doc nil (Element "methodResponse" nil str)))

;;; Faults
(define (fault code msg)
  (if-not msg (setf msg (or (lookup code error-codes) "Unknown error")))
  (response
    (Element "fault" nil
      (Element "value" nil
        (Element "struct" nil
          (Element "member" nil
            (Element "name" nil "faultCode")
            (Element "value" nil (Element "int" nil code)))
          (Element "member" nil
            (Element "name" nil "faultString")
            (Element "value" nil (Element "string" nil (Element:encode msg)))))))))

;;; Registration of functions

;; @syntax (Xmlrpc-server:add-to-registry <str-path> <lambda-func>)
;; @param <str-path> the path which will respond to the passed function
;; @param <lambda-func> the function which will be applied to requests on <str-path>
;; <p>Registers a function to respond when requests are sent to <str-path>. The function
;; will be passed a lisp representation of the XML-RPC request.</p>
(setf registry '())

(define (add-to-registry key func)
  (push (list (string key) func) registry -1))

(define (registered key)
  (lookup key registry))

;;; Xmlrpc response handler
(define (response-handler request-xml , req fun res)
  (if-not (catch (parse-request request-xml) 'req)
    ;; parse error
    (fault req)
    ;; valid xml-rpc
    (if-not (setf fun (registered (first req)))
      ;; method not found
      (fault METHOD-NOT-FOUND (string "Method not found: " (first req)))
      ;; valid method
      (if-not (catch (apply fun (req 1)) 'res)
        ;; error in function
        (fault APPLICATION-ERROR (string "Application error: " (first (parse res "\n"))))
        ;; valid response
        (if-not (catch (lisp->xmlrpc-params res) 'res)
          ;; server error when translating to xmlrpc params
          (fault SERVER-ERROR res)
          ;; valid response
          (response res))))))

(define (handler request , http res)
  (if (catch
        (begin
          (setf http (Http:parse-request request))
          (when (and (= "POST" (upper-case (lookup "method" http)))
                     (= server-path (trim (lookup "path" http) "" "/")))
              (let ((resp (response-handler (lookup "content" http))))
                (Http:format-response resp 200 "text/xml"))))
        'res)
      res
      (Http:format-response
        (fault SERVER-ERROR (string "Server error: " res))
        200 "text/xml")))

;; @syntax (Xmlrpc-server:run-server [<int-port> [<str-path>]])
;; @param <int-port> port on which to listen; defaults to 8080
;; @param <str-path> path on which to response; defaults to "/RPC2"
;; <p>Initializes and starts server. Server will block if-not started in a
;; child process.</p>
(setf server-path "/RPC2")

(define (run-server (port 8080) (path "/RPC2"))
  (setf server-path path)
  (setf SocketServer:handler handler)
  (println "Starting server")
  (SocketServer:serve port))

;;; Introspection methods (partially implemented)
(define (system-listMethods)
  (list (map 'string (map 'first registry))))

(add-to-registry "system.listMethods" system-listMethods)

(context MAIN)
