;; @module matching
;; @author Jeff Ober <jeffober@gmail.com>
;; @version 1.0
;; @location http://static.artfulcode.net/newlisp/matching.lsp
;; @package http://static.artfulcode.net/newlisp/matching.qwerty
;; @description Complex conditionals using match and unify (updated for newlisp 10)
;; <p>Matching conditionals make possible a very terse style of programming common to the
;; ML family of languages.</p>
;; <h4>Version history</h4>
;; <b>1.0</b>
;; &bull; updated for newlisp 10
;; &bull; renamed module to matching
;; &bull; removed dependency on util.lsp
;; &bull; made match-bind a global symbol
;; &bull; fixed error in documentation for match-cond
;; &bull; fixed error in match-cond that bound arguments incorrectly
;; &bull; removed match-with and if-match because they were generally confusing and unnecessary
;; &bull; match-bind no longer binds exact matches (e.g. 'foo and 'foo), only wildcards
;; &bull; fixed bug in match-case where target was bound incorrectly in some cases
;; 
;; <b>0.5</b>
;; &bull; fixed bug in 'with-match' causing $0 to be misinterpreted in certain circumstances
;; 
;; <b>0.4</b>
;; &bull; added 'with-match', a simpler operator that is more idiomatic of newLISP
;; 
;; <b>0.3</b>
;; &bull; added 'if-match', 'match-with'
;; 
;; <b>0.2</b>
;; &bull; altered argument order in 'match-cond'
;; &bull; added 'match-case'
;; 
;; <b>0.1</b>
;; &bull; initial release
;; &bull; added 'match-bind', 'match-let'

;; @syntax (match-bind <vars> <pattern> <target>)
;; @param <vars> symbols to bind
;; @param <pattern> match pattern
;; @param <target> match target
;; <p>If '(match <pattern> <target>)' is valid, binds <vars> to
;; the result of its evaluation.</p>
;; @example
;; (match-bind '(a b) '(? ?) '(1 2))
;; a => 1
;; b => 2
(define (match-bind var-list pattern target)
  (let ((m (match pattern target)))
    (map set var-list m)))

(global 'match-bind)

;; @syntax (match-let (<vars> <pattern> <target>) <body> ...)
;; @param <vars> symbols to bind
;; @param <pattern> match pattern
;; @param <target> match target
;; @param <body> series of forms to be evaluated
;; <p>'match-let' will evaluate body in an environment where
;; variables <vars> are bound to the destructured values from
;; <target> according to match pattern <pattern>.  Thus, if
;; the result of '(match <pattern> <target>)' is '(1 2 (3 4))',
;; <vars> '(a b c)' will be bound as '((a 1) (b 2) (c '(3 4)))'.</p>
;; <p>Should <pattern> not match <target>, an error is signaled.
;; Note that <target> is evaluated before <body> is executed.
;; <target> is evaluated even if the match fails, as it is the
;; evaluated form against which <pattern> is matched.</p>
;; @example
;; (let ((lst '(1 2 3 4)))
;;   (match-let ((a b c) (? ? *) lst)
;;     (+ a b (apply * c))))
;; 
;; => 15
(define-macro (match-let)
  (letex ((var-list (args 0 0))
          (pattern (args 0 1))
          (target (args 0 2))
          (body (cons 'begin (rest (args)))))
  (if (match 'pattern target)
      (local var-list
        (match-bind 'var-list 'pattern target)
        body)
      (throw-error "no match possible"))))

(global 'match-let)

;; @syntax (match-case <target> (<case-pattern> <case-vars> <case-expr>) ...)
;; @param <target> the expression to match against
;; @param <case-pattern> the pattern to match with <target>
;; @param <case-vars> the symbols to bind to the result of the match
;; @param <case-expr> the form to be evaluated should <case-pattern> match successfully
;; <p>'match-case' tries a series of match cases in sequence and returns the result of 
;; evaluating the first successful match's <case-expr> in a local scope in which symbols
;; <case-vars> are bound to the result of matching <case-pattern> against <target>.</p>
;; @example
;; (let ((x '(1 2 3 4 5)))
;;   (match-case x
;;     ((? ? ?) (a b c) (println "this form is not evaluated since '(? ? ?) does not match x"))
;;     ((? ? *) (a b c) (println "c is bound to " c " in this form"))
;;     ((*) (a) (println "catch-all")))) ; (*) matches all lists, so it is catch-all for x
;; 
;; => "c is bound to (3 4 5) in this form"
(define-macro (match-case)
  (let ((target (args 0)))
    (catch
      (dolist (form (rest (args)))
        (letex ((tgt (eval target)) (pattern (form 0)) (vars (form 1)) (expr (form 2)))
          (if (match 'pattern 'tgt)
            (match-let (vars pattern 'tgt)
              (throw expr))))))))

(global 'match-case)

;; @syntax (match-cond ((<pattern> <vars> <target>) <body-forms>) ...)
;; @param <pattern> match pattern
;; @param <vars> symbols to bind
;; @param <target> match target
;; @param <body> series of forms to be evaluated
;; <p>'match-cond' evaluates a series of match/bind combinations until one
;; of them evaluates non-nil.  The result of the successful match will be bound
;; to the symbols in <vars>, and the associated <body-forms> will be evaluated
;; with those symbols locally bound.  The result of the evaluation is nil if
;; no forms match or the result of the final <body-form> evaluated.</p>
;; <p>'match-cond' is more versatile than 'match-case' in that 'match-cond' may
;; test against multiple targets and evaluates its <body-forms> in an implicit
;; 'begin' block.</p>
;; @example
;; (let ((x '(1 2 3 4 5)))
;;   (match-cond
;;     (((? ? ?) (a b c) x) (println "evaluation never gets here"))
;;     (((? ? *) (a b c) x) (println "c gets bound to " c))
;;     (((*) (a) x) (println "catch-all")))) ; (*) matches all lists, so is catch-all for x
;; 
;; => "c gets bound to (3 4 5)"
(define-macro (match-cond)
  (catch
    (doargs (form)
      (letex ((pattern (form 0 0))
              (vars (form 0 1))
              (target (form 0 2))
              (body (cons 'begin (rest form))))
        (if (match 'pattern target)
            (match-let (vars pattern target)
              (throw body)))))))

(global 'match-cond)

;; @syntax (with-match <target> (<match-form-n> <body-n>) ...)
;; @param <target> target of the match
;; @param <match-expr-n> match pattern to be tested against <target>
;; @param <body-n> block to be evaluated if <match-expr-n> matches successfully
;; <p>Tests each <match-expr-n> in turn against <target>.  On the first successful match,
;; the system variable '$0' is bound to the result of the match and the paired <body-n> is
;; evaluated.  No further match forms are tested after a successful match and the result of
;; the evaluation of <body-n> is returned.  If no match is successful, 'nil' is returned.</p>
;; @example
;; (with-match '(1 2 3 (4 5))
;;   ((? ? ? (? ?)) (apply + $0))
;;   ((? *) (println "Never gets here")))
;; => 15
(define-macro (with-match)
  (letex ((target (args 0)) (forms (rest (args))))
    (catch
      (dolist (form 'forms)
        (letex ((match-form (first form)) (body (cons 'begin (rest form))))
          (let (($0 (match 'match-form target)))
            (if $0 (throw body))))))))

(global 'with-match)
