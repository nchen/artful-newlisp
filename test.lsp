(load "matching.lsp")

(context 'json)

(define (read-number text , matched)
  (setf text (trim text))
  (when (setf matched (regex {^([.,\d]+(e[.,\d]+)?)} text))
    (list (float (pop text 0 (matched 5))) text)))

(define (read-string text , start end)
  (setf text (trim text))
  (setf start (pop text))
  (setf end (find (string {(?<!\\)} start) text 0))
  (unless end (throw-error (string "unbalanced string delimiters")))
  (setf str (pop text 0 end))
  (list (replace (string {\} start) str start) (rest text)))

(define (read-identifier text , matched)
  (setf text (trim text))
  (setf matched (regex {([$_a-zA-Z][$_a-zA-Z0-9]*)(.*)} text 4))
  (list (nth 3 matched) (nth 6 matched)))

(define (tokenize text (acc '()) , tok tail)
  (setf text (trim text))
  (if (empty? text)
    acc
    (begin
      (setf buffer text)
      (cond
        ((regex {^\s+} text)
         (tokenize (replace {^\s+} text "" 0) acc))
        ((regex {^\d} text)
         (map set '(tok tail) (read-number text))
         (push tok acc -1)
         (tokenize tail acc))
        ((regex {^['"]} text)
         (map set '(tok tail) (read-string text))
         (push tok acc -1)
         (tokenize tail acc))
        ((regex [text]^[{}\[\]:,][/text] text)
         (setf tok (pop text))
         (case tok
           ("{" (push 'OPEN_BRACE acc -1))
           ("}" (push 'CLOSE_BRACE acc -1))
           ("[" (push 'OPEN_BRACKET acc -1))
           ("]" (push 'CLOSE_BRACKET acc -1))
           (":" (push 'COLON acc -1))
           ("," (push 'COMMA acc -1)))
         (tokenize text acc))
        (true
         (map set '(tok tail) (read-identifier text))
         (push tok acc -1)
         (tokenize tail acc))))))

(define (lex tokens, (tree '()) (loc '(-1)))
  (dolist (tok tokens)
    (case tok
      (OPEN_BRACKET
        (push (list) tree loc)
        (push -1 loc))
      (OPEN_BRACE
        (push (list) tree loc)
        (push -1 loc))
      (CLOSE_BRACKET (pop loc))
      (CLOSE_BRACE (pop loc))
      (COMMA)
      (COLON
        (push (list (pop tree loc)) tree loc)
        (push -1 loc))
      (true
        (push tok tree loc))))
  (first tree))



(setf text [text]
[
  'life',
  "the universe",
  'everything',
  42,
  {
    weather: {
      "foo": ["bar", 42, baz, 84, "bat"],
      "serverUTCTime": "April 02, 2009 22:28:35",
      "sunset": "April 02, 2009 19:33:00",
      sunrise: "April 02, 2009 06:55:00",
      "location": "Fairfax, VA",
      "formattedRise": "6:55 am",
      "formattedSet": "7:33 pm",
      "formattedCivilStart": "6:28 am",
      "formattedCivilEnd": "8:00 pm",
      "timezone": "-4",
      "longitude": "W 77.3",
      "latitude": "N 38.8",
      "zip": "22030",
      "CTE": "April 02, 2009 20:00:00",
      "CTS": "April 02, 2009 06:28:00"
    }
  }
]
[/text])

(setf toks (tokenize text))
(setf lexed (lex toks))
(println lexed)

(context 'MAIN)



