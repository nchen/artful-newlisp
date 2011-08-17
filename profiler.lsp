;; @module Profiler
;; @author Jeff Ober <jeffober@gmail.com>, Kanen Flowers <kanendosei@gmail.com>
;; @version 1.0
;; @location http://static.artfulcode.net/newlisp/profiler.lsp
;; @package http://static.artfulcode.net/newlisp/profiler.qwerty
;; @description Profiles applications to help identify bottlenecks (updated for newlisp 10)
;; <h4>Version history</h4>
;; <b>1.3</b>
;; &bull; fixed incompatibilities with newlisp 10
;; 
;; <b>1.2</b>
;; &bull; added percentage of total time to report
;; &bull; added ability to sort report by column
;; 
;; <b>1.1</b>
;; &bull; updated report to dynamically calculate column lengths
;; &bull; updated profile-context to accept multiple contexts
;; 
;; <b>1.0</b>
;; &bull; initial release
;; 
;; @example
;; (define (fib:fib n)
;;   (if (< n 2) 1
;;       (+ (fib (- n 1)) (fib (- n 2)))))
;; 
;; (define (fib-memo:fib-memo n)
;;   (or (context 'fib-memo (string n))
;;       (if (< n 2) 1
;;           (context 'fib-memo (string n)
;;             (+ (fib-memo (- n 1)) (fib-memo (- n 2)))))))
;; 
;; (Profiler:profile-context fib fib-memo)
;; (dotimes (i 25)
;;   (println "Fib  " i ": " (fib i))
;;   (fib-memo i))
;; (println)
;; (Profiler:report 'calls)
;; 
;; =>
;; Fib  0: 1
;; Fib  1: 1
;; Fib  2: 2
;; Fib  3: 3
;; Fib  4: 5
;; Fib  5: 8
;; Fib  6: 13
;; Fib  7: 21
;; Fib  8: 34
;; Fib  9: 55
;; Fib  10: 89
;; Fib  11: 144
;; Fib  12: 233
;; Fib  13: 377
;; Fib  14: 610
;; Fib  15: 987
;; Fib  16: 1597
;; Fib  17: 2584
;; Fib  18: 4181
;; Fib  19: 6765
;; Fib  20: 10946
;; Fib  21: 17711
;; Fib  22: 28657
;; Fib  23: 46368
;; Fib  24: 75025
;; 
;; function           |      calls |   total ms |       ms/call |     % time
;; -------------------+------------+------------+---------------+-----------
;; fib:fib            |      24169 |       4942 |      0.204477 |     100.00
;; fib-memo:fib-memo  |         71 |          0 |      0.000000 |       0.00

(define calls:calls)
(define times:times)

(context 'Profiler)

(setq profiled '())
(set 'originals '())

;; @syntax (Profiler:reset-functions)
;; <p>Resets functions to their values prior to being adapted for profiling.
;; This function is automatically called when a profiler report is printed.</p>
(define (reset-functions)
  (dolist (f originals)
    (set (first f) (last f))))

;; @syntax (Profiler:reset-statistics)
;; <p>Resets profiler statistics. This function is automatically called when a
;; profiler report is printed.</p>
(define (reset-statistics)
  (dolist (p profiled)
    (calls p nil)
    (times p nil))
  (set 'profiled '())
  (set 'originals '()))

;; @syntax (Profiler:report <sort-by>)
;; @param column to sort by (descending order); default is 'calls.
;; <p>Prints statistics for functions profiled.  <sort-by> may be 'times, 'calls, 'per-call,
;; or 'percentage.  Sorts by number of calls by default.</p>
(define (row-width func)
  (+ 1 (apply 'max (cons 9 (map 'length (map 'string (map func profiled)))))))

(define (time/call f , c t)
  (set 'c (calls f))
  (set 't (times f))
  (if (or (zero? c) (zero? t)) 0 (div t c)))

(define (runtime-% f , t t-all)
  (set 't (times f))
  (set 't-all (apply '+ (cons 0 (map times profiled))))
  (mul 100 (div t t-all)))

(define (report (sort-by 'calls) , c t p) ; sort-by can be 'times, 'calls, 'per-call, 'percentage
  (set 'sort-by (name sort-by))
  (let ((col-1 (row-width (fn (p) p)))
        (col-2 (row-width (fn (p) (calls p))))
        (col-3 (row-width (fn (p) (times p))))
        (col-4 (row-width time/call))
        (col-5 (row-width runtime-%)))
    (println (format (string "%-" col-1 "s | %" col-2 "s | %" col-3 "s | %" col-4 "s | %" col-5 "s")
                     "function" "calls" "total ms" "ms/call" "% time"))
    (println (dup "-" (+ 1 col-1)) "+"
             (dup "-" (+ 2 col-2)) "+"
             (dup "-" (+ 2 col-3)) "+"
             (dup "-" (+ 2 col-4)) "+"
             (dup "-" (+ 1 col-5)))
    (dolist (f (sort profiled
                     (fn (a b) (case sort-by
                                 ("calls" (> (calls a) (calls b)))
                                 ("times" (> (times a) (times b)))
                                 ("per-call" (> (time/call a) (time/call b)))
                                 ("percentage" (> (runtime-% a) (runtime-% b)))))))
      (println (format (string "%-" col-1 "s | %" col-2 "d | %" col-3 "d | %" col-4 "f | %" col-5 ".2f")
                       f (calls f) (times f) (time/call f) (runtime-% f)))))
  (reset-functions)
  (reset-statistics))

;; @syntax (Profiler:profiled-function <func>)
;; @param <func> function to profile
;; <p>Returns a modified function to use with the profiler.</p>
(define-macro (profiled-function)
  (letex ((fn-name (string (args 0)))
          (fn-calls (sym (string "_" (args 0)) calls))
          (fn-times (sym (string "_" (args 0)) times))
          (func (args 0)))
    (when (or (protected? func) (not (lambda? func)))
          (throw-error (format "%s is protected or not a function" fn-name)))
    (push fn-name profiled)
    (calls fn-name 0)
    (times fn-name 0)
    (letex ((params (first func)) (body (cons 'begin (rest func))))
      (lambda params
        (local (res)
          (inc fn-calls)
          (inc fn-times (time (set 'res body)))
          res)))))

;; @syntax (Profiler:profile-functions <list-functions>)
;; @param <list-functions> list of functions to profile
;; <p>Sets functions listed in <list-functions> to have statistics collected while
;; they run.  Protected and built-in functions cannot be profiled.</p>
;; @example
;; (define (foo x y) (+ x y))
;; (define (bar x y) (foo x y))
;; (Profiler:profile-functions '(foo bar))
;; => '(foo bar)
(define (profile-functions fns)
  (reset-statistics)
  (dolist (f fns)
    (letex ((f f))
      (push (list 'f f) originals)
      (set 'f (profiled-function f))))
  fns)

;; @syntax (Profiler:profile-context <ctx> [<ctx-2> ...])
;; @param <ctx> context to profile
;; <p>Updates all functions in context <ctx> to be profiled.</p>
;; @example
;; (define (foo:foo x y) (+ x y))
;; (define (foo:bar x y) (foo x y))
;; (Profiler:profile-context foo)
;; => '(foo:bar foo:foo)
(define (profile-context)
  (profile-functions (clean (lambda (s)
                              (or (protected? s)
                                  (not (lambda? (eval s)))))
                            (apply 'append (map 'symbols (args))))))

(context 'MAIN)
