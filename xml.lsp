;; @module XML
;; @author Jeff Ober <jeffober@gmail.com>, Kanen Flowers <kanendosei@gmail.com>
;; @version 2.1
;; @location http://static.artfulcode.net/newlisp/xml.lsp
;; @package http://static.artfulcode.net/newlisp/xml.qwerty
;; @description Parsing and serializing of XML data (updated for newlisp 10).
;; Functions to parse XML text (non-validating) and serialize lisp structures to XML.
;; Requires matching and newlisp 10.
;; <h4>Version history</h4>
;; <b>2.1</b>
;; &bull; code clean-up
;; &bull; updated for newlisp 10
;; &bull; some arguments have changed in lisp->xml and xml->lisp
;; &bull; default encoding is now determined by newlisp UTF-8 compile options
;; &bull; added functions to trim whitespace and decode entities
;; 
;; <b>2.0</b>
;; &bull; complete rewrite
;; &bull; added XML-compliant entities
;; &bull; automatic serialization of data
;; 
;; <b>1.0</b>
;; &bull; initial release

;;;============================================================================
;;; XML context
;;;============================================================================
  
(context 'XML)

(constant 'trim-ws-re (regex-comp {(^\s*)|(\s*$)} ))
(constant 'xml-entity-decode-re (regex-comp "&#(\\d{1,4});"))
(constant 'xml-entity-encode-re (regex-comp (string "(" (join (map (fn (i) (format {\x%x} i)) '(34 38 39 60 62)) "|") ")")))
(constant 'default-parse-options (+ 1 16))

;; @syntax (XML:trim-whitespace <str>)
;; @param <str> a string
;; <p>Trims all whitespace off both ends of <str>.</p>
(define (trim-whitespace text)
  (replace trim-ws-re text "" 0x10000)
  text)

;; @syntax (XML:decode <str>)
;; @param <str> a string
;; <p>Decodes XML entities and converts them to characters.</p>
(define (decode str)
  (replace xml-entity-decode-re str (char (int $1)) 0x10000))

;; @syntax (XML:encode <str>)
;; @param <str> a string
;; <p>Encodes characters in a string to be valid for XML.</p>
(define (encode str)
  (replace xml-entity-encode-re str (string "&#" (char $1) ";") 0x10000))

(define (parse-string text (options default-parse-options) , old-tags parsed)
  (setf old-tags (xml-type-tags))
  (xml-type-tags nil nil nil nil)
  (setf parsed (xml-parse text options))
  (apply xml-type-tags old-tags)
  parsed)

(define (serialize-attributes attr-list)
  (match-let ((attrs) (@ *) attr-list)
    (join (map (fn (pair) (format " %s=\"%s\"" (map string pair))) attrs) "")))

(define (opening-tag node)
  (match-let ((tag attr _) (? ? *) node)
    (string "<" tag (serialize-attributes attr) ">")))

(define (closing-tag node)
  (string "</" (first node) ">"))

(define (empty-tag node)
  (match-let ((tag attr _) (? ? *) node)
    (string "<" tag (serialize-attributes attr) " />")))

(define (serialize-text-node node)
  (match-let ((tag attr text) (? ? ?) node)
    (string (opening-tag node) (encode (decode text)) (closing-tag node))))

(define (serialize xml indent? (encoding (if utf8 "UTF-8" "ASCII")) (depth 0), buf)
  (setf buf "")
  
  (when (zero? depth)
    (write-buffer buf (string {<?xml version="1.0" encoding="} encoding {" ?>} "\n"))
    (setf xml (first xml)))
  
  (when indent? (write-buffer buf (string "\n" (dup "  " depth))))
  
  (write-buffer buf
    (cond
      ((match '(? ?) xml) (empty-tag xml))
      ((match '(? ? ?) xml) (serialize-text-node xml))
      ((match '(? ? *) xml)
        (string
          (opening-tag xml)
          (join (map (fn (child) (serialize child indent? nil (+ 1 depth))) (rest (rest xml))))
          (if indent? (string "\n" (dup "  " depth)) "")
          (closing-tag xml)))))
  
  buf)

;; @syntax (XML:lisp->xml <sxml-list> [<indent?> [<str-encoding>]])
;; @param <sxml-list> an SXML list
;; @param <indent?> optional; whether or not to format the resulting XML (default nil)
;; @param <str-encoding> optional; sets the encoding in the declaration
;; <p>Serializes an SXML list (equivalent to parsing an XML document with
;; (xml-type-tags nil nil nil nil) and options 1 and 16). The encoding in the
;; declaration defaults to UTF-8 if newlisp was compiled with UTF-8 support,
;; ASCII otherwise.</p>
(setf lisp->xml serialize)

;; @syntax (XML:xml->lisp <str-xml>)
;; @param <str-xml> an XML string
;; <p>Parses <str-xml> and returns an SXML list. Uses newlisp's built-in parser.</p>
;; <p>Equivalent to:</p>
;; <pre>(begin (xml-type-tags nil nil nil) (xml-parse <str-xml> (+ 1 16)))</pre>
(setf xml->lisp parse-string)

(context 'MAIN)
