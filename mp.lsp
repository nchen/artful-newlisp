;; @module MP
;; @author Jeff Ober <jeffober@gmail.com>, Kanen Flowers <kanendosei@gmail.com>
;; @version 1.1
;; @location http://static.artfulcode.net/newlisp/mp.lsp
;; @package http://static.artfulcode.net/newlisp/mp.qwerty
;; @description Classes for multi-processing and synchronization (requires newlisp 10)
;; Provides many classes for controlling access to resources as well as
;; utilities for common multi-processing tasks. Requires newlisp 10 and the
;; util module.
;; 
;; <h4>Version history</h4>
;; <b>1.1</b>
;; &bull; MP:iter and MP:map both now check spawn returns for errors and re-throw them
;; &bull; MP:map and MP:iter now block in sleep, rather than sync, which uses *much* less cpu time
;; 
;; <b>1.0</b>
;; &bull; initial release (replaces locks module)

;;;=============================================================================
;;; MP: multi-processing utilities for newlisp.
;;;=============================================================================

(context 'MP)

;; @syntax (MP:get-pid)
;; <p>Returns the pid of the current process.</p>
;; @example
;; (MP:get-pid) => 16024
(define (get-pid) (sys-info 6))

;; @syntax (MP:with-lock-held <lock> <expr> [<expr> ...])
;; @param <lock> an instance of a locking class with an :acquire and :release method
;; @param <expr> one or more expressions to be evaluated
;; <p>Evaluates one or more expressions with <lock> acquired. <lock> may be an
;; instance of any class with an :acquire and :release method. MP:with-lock-held
;; guarantees that the lock will be released, even if an error is thrown during
;; evaluation of the body expressions. Errors thrown will be re-thrown after the
;; lock is released. The value of the expression is the value of the last body
;; form evaluated.</p>
;; @example
;; (setf lock (Lock))
;; (MP:with-lock-held lock
;;   (do stuff))
(define-macro (with-lock-held)
  (letex ((_inst (args 0))
          (_body (cons begin (rest (args))))
          (_err (gensym))
          (_res (gensym)))
    (local (_err _res)
      (:acquire _inst)
      (setf _err (catch _body '_res))
      (:release _inst)
      (if-not _err
        (throw-error _res)
        _res))))

;; @syntax (MP:wait <fn-condition> <int-start> <int-max> [<int-timeout>])
;; @param <fn-condition> a predicate used to test state
;; @param <int-start> the initial sleep time (milliseconds)
;; @param <int-max> the maximum sleep time (milliseconds)
;; @param <int-timeout> the maximum number of milliseconds to wait for <fn-condition> to return true
;; <p>Blocks until <fn-condition> returns true. Wait will poll <fn-condition>
;; every <int-start> ms, growing every ten polling cycles up to <int-max> ms,
;; up to an optional <int-timeout> ms. Returns true when the polling loop
;; returns normally, nil when <int-timeout> (if present) was reached. Note that
;; <int-timeout> will be approximately observed; it is affected by the current
;; interval. If <int-start> and <int-max> are equal, no change in the polling
;; interval will take place.</p>
;; @example
;; ; Blocks until 'some-flag is set to true. Polls initially every 50ms,
;; ; increasing to 500ms. After 5 seconds (5000 ms), returns even if
;; ; 'some-flag is nil.
;; 
;; (MP:wait (fn () (true? some-flag)) 50 500 5000)
(define (wait condition start-interval max-interval timeout , waited result increase current)
  (setf increase (ceil (/ (- max-interval start-interval) 10)))
  (setf current start-interval)
  (setf waited 0)
  (until (or (and timeout (>= waited timeout)) ; if timeout param present, check for timeout
             (setf result (condition))) ; gives us our return value
    (when (zero? (mod $idx 10))
      (inc current increase))
    (inc waited (sleep current)))
  result)

;; @syntax (MP:map <fun> <seq> [<limit>])
;; @param <fun> a function to apply to each element of <seq>
;; @param <seq> a list
;; @param <limit> the max number of processes to start
;; @param <timeout> the (approximate) max time to wait for the process to complete
;; <p>Maps <fun> over <seq>, bounding the number of running processes to
;; <limit>.</p>
;; @example
;; (MP:map pow (sequence 0 4) 4)
;; => (0 1 4 9 16)
(define (MP:map fun seq limit timeout , result mem (MP:wait-n 1) (max-wait 500) (increment 2))
  ; Using an array makes symbol access faster
  (setf mem (make-array (length seq) gensym))
  (dolist (elt seq)
    ; Gradually increasing the sync timeout reduces polling overhead
    ; for long-running calculations.
    (when limit
      (until (< (length (sync)) limit)
        (when (< wait-n max-wait)
          (setf wait-n (* increment wait-n)))
        (sleep wait-n)
        (sync 50)))
    ; Add a new child process
    (spawn (mem $idx) (fun elt)))
  ; Wait for remaining calculations to complete
  (sync -1)
  ; Get results and delete symbols used
  (setf result (MAIN:map eval (array-list mem)))
  ; Check for errors in results
  (dolist (res result)
    (when (and (string? res) (starts-with res "ERR:"))
      (throw-error (replace {(ERR: user error : )+} res "" 0))))
  (array-iter delete mem)
  ; Return result
  result)

;; @syntax (MP:iter <fun> <seq> [<limit> [<timeout>]])
;; @param <fun> a function to apply to each element of <seq>
;; @param <seq> a list
;; @param <limit> the max number of processes to start
;; @param <timeout> the (approximate) max time to wait for the process to complete
;; <p>Iterates over <seq>, applying <fun> to each element. If <limit> is
;; specified, will not start more than <limit> processes. Returns the value of
;; the final iteration.</p>
;; @example
;; (MP:iter println (sequence 0 4) 4)
;; 0
;; 1
;; 3
;; 2
;; 4
(define (iter fun seq limit , mem check result (MP:wait-n 1) (max-wait 500) (increment 2))
  ; Using an array makes symbol access faster
  (setf mem (make-array (length seq) gensym))
  (dolist (elt seq)
    ; Gradually increasing the sync timeout reduces polling overhead
    ; for long-running calculations.
    (when limit
      (until (< (length (sync)) limit)
        (when (< wait-n max-wait)
          (setf wait-n (* increment wait-n)))
        (sleep wait-n)
        (sync 50)))
    ; Add a new child process
    (spawn (mem $idx) (fun elt)))
  ; Wait for remaining calculations to complete
  (sync -1)
  ; Check for errors in results
  (dotimes (i (length seq))
    (setf check (eval (nth i mem)))
    (when (and (string? check) (starts-with check "ERR:"))
      (throw-error (replace {(ERR: user error : )+} check "" 0))))
  ; Get results and delete symbols used
  (setf result (eval (last mem)))
  (array-iter delete mem)
  result)

(context 'MAIN)

;;;=============================================================================
;;; Semaphore: a synchronized counter that blocks when attempting to decrement
;;; below zero. Also known as a counting semaphore. By default, newly created
;;; Semaphores are initialized with a count of 1.
;;;=============================================================================

;; @syntax (Semaphore <int-initial>)
;; @param <int-initial> the initial value of the semaphore
;; <p>Creates a synchronized counter that cannot drop below zero. Any attempt to
;; do so will block until the counter has been incremented (released). A basic
;; semaphore may count as high or low as desired; this is useful for protecting
;; queues or stacks. By default, Semaphores are initialized with a value of 1.</p>
;; @example
;; (setf sem (Semaphore))
;; (setf queue '())
;; (dotimes (i 10)
;;   (push i queue)
;;   (:inc sem 1))
(define (Semaphore:Semaphore (initial-value 1) , sem)
  (setf sem (semaphore))
  (semaphore sem initial-value)
  (list (context) sem))

;; @syntax (:inc <inst> [<int-amount>])
;; @param <inst> an instance of Semaphore
;; @param <int-amount> the amount to increment; default is 1
;; <p>Increments (releases) the Semaphore by <int-amount>.</p>
(define (Semaphore:inc inst (n 1))
  (semaphore (inst 1) n))

;; @syntax (:dec <inst> [<int-amount>])
;; @param <inst> an instance of Semaphore
;; @param <int-amount> the amount to decrement; default is 1
;; <p>Decrements (acquires) the Semaphore by <int-amount>.</p>
(define (Semaphore:dec inst (n 1))
  (semaphore (inst 1) (- n)))

;; @syntax (:count <inst>)
;; @param <inst> an instance of Semaphore
;; <p>Returns the current count of the Semaphore.</p>
(define (Semaphore:count inst)
  (semaphore (inst 1)))

;; @syntax (:acquire <inst> [<blocking> [<int-amount>]])
;; @param <inst> an instance of Semaphore
;; @param <blocking> if true (default is true) blocks if Semaphore is held
;; @param <int-amount> the amount by which the Semaphore is to be decremented
;; <p>Attempts to acquire the Semaphore. If <blocking> is true, :acquire will
;; block until Semaphore becomes available. If <blocking> is nil, :acquire will
;; attempt to acquire the Semaphore and return nil immediately if it is
;; unavailable.</p>
(define (Semaphore:acquire inst (blocking true) (n 1))
  (when (or blocking (>= n (:count inst)))
    (:dec inst n)))

;; @syntax (:release <inst>)
;; @param <inst> an instance of Semaphore
;; <p>Releases the Semaphore.</p>
(setf Semaphore:release Semaphore:inc)

;;;=============================================================================
;;; Share: a shared page in memory. Only integers, floats, or strings may be
;;; stored. To store complex objects, use source and pack to first convert the
;;; object to a string.
;;;=============================================================================

;; @syntax (Share)
;; <p>A Share wraps a single page in memory which may be used to store interger,
;; float, or string values between processes. In order to store compound objects
;; or lists, use source and/or pack to serialize the object first. Access to the
;; Share between different processes must be protected with locking
;; mechanisms.</p>
(define (Share:Share)
  (list (context) (share)))

;; @syntax (:set <inst> <value>)
;; @param <inst> an instance of Share
;; @param <value> the new value for the Share
;; <p>Sets the Share's value to <value>.</p>
(define (Share:set inst value)
  (share (inst 1) value))

;; @syntax (:get <inst>)
;; @param <inst> an instance of Share
;; <p>Gets the value of the Share.</p>
(define (Share:get inst)
  (share (inst 1)))

;;;=============================================================================
;;; Synchronized: a Share that has access protected with a semaphore.
;;;=============================================================================

;; @syntax (Synchronized [<initial-value>])
;; @param <initial-value> the initial value, if any
;; <p>Synchronized wraps a Share and protects access to it with a Semaphore.</p>
(define (Synchronized:Synchronized <initial-value>)
  (setf mem (Share))
  (when initial-value
    (:set mem initial-value))
  (list (context) mem (Semaphore 1)))

;; @syntax (:get <inst>)
;; @param <inst> an instance of Synchronized
;; <p>Gets the current value of the Synchronized instance. Will block if another
;; process is currently getting or setting the value.</p>
(define (Synchronized:get inst)
  (MP:with-lock-held (inst 2)
    (:get (inst 1))))

;; @syntax (:set <inst> <expr>)
;; @param <inst> an instance of Synchronized
;; @param <expr> the new value
;; <p>Sets the value of the Synchronized share. If the new value is an
;; expression, it will be evaluated with the variable $0 set to the old value
;; of the share. This is necessary to prevent a deadlock when dealing with
;; self-referential values.</p>
;; @example
;; (setf mem (Synchronized))
;; (:set mem 10)
;; => 10
;; (:set mem (+ 10 $0))
;; => 20
(define-macro (Synchronized:set)
  (letex ((_sync_inst (args 0)) (_sync_expr (args 1)))
    (MP:with-lock-held (_sync_inst 2)
      (setf $0 (:get (_sync_inst 1)))
      (:set (_sync_inst 1) (eval _sync_expr)))))

;;;=============================================================================
;;; Lock: a binary semaphore. It is an error for a different process than that
;;; which acquires the lock to release the lock.
;;;=============================================================================

;; @syntax (Lock)
;; <p>A Lock is a binary semaphore (or mutual exclusion lock) that may be set to
;; either 1 (released) or 0 (acquired). It is an error for a process to release
;; a Lock it has not acquired.</p>
(define (Lock:Lock)
  (list (context) (Semaphore) (Synchronized)))

;; @syntax (:acquire <inst> [<blocking>])
;; @param <inst> an instance of Lock
;; @param <blocking> whether to block if the Lock is not available (default is true)
;; <p>Attempts to acquire the Lock, blocking until it becomes available if
;; <blocking> is true.</p>
(define (Lock:acquire inst (blocking true))
  (when (:acquire (inst 1) blocking)
    (:set (inst 2) (MP:get-pid))
    true))

;; @syntax (:release <inst>)
;; @param <inst> an instance of Lock
;; <p>Releases the Lock.</p>
(define (Lock:release inst)
  (if (= (MP:get-pid) (:get (inst 2)))
    (:release (inst 1))
    (throw-error "unlocking process does not match owner")))

;;;=============================================================================
;;; RLock: identical to a Lock except that the locking process may acquire the
;;; lock multiple times. The number of acquires must be >= the number of
;;; releases.
;;;=============================================================================

;; @syntax (RLock)
;; <p>An RLock is a Lock that may be acquired multiple times by the same
;; process. This is useful to lock various inter-dependent functions in the same
;; process with a single lock. Observes the invariant # acquires >= # releases.</p>
(define (RLock:RLock)
  (list (context) (Semaphore) (Synchronized) (Synchronized)))

(define (RLock:owner inst)
  (inst 2))

(define (RLock:counter inst)
  (inst 3))

(define (RLock:held? inst)
  (zero? (:count (inst 1))))

(define (RLock:process-is-owner? inst)
  (= (MP:get-pid) (:get (:owner inst))))

(define (RLock:inc inst)
  (:set (:counter inst) (+ $0 1)))

(define (RLock:dec inst)
  (:set (:counter inst) (- $0 1)))

;; @syntax (:acquire <inst> [<blocking>])
;; @param <inst> an instance of RLock
;; @param <blocking> whether to block if the RLock is not available (default is true)
;; <p>Attempts to acquire the RLock, blocking until it becomes available if
;; <blocking> is true.</p>
(define (RLock:acquire inst (blocking true))
  (if (and (:held? inst) (:process-is-owner? inst))
    (:inc inst)
    (when (:acquire (inst 1))
      (:set (:counter inst) 1)
      (:set (:owner inst) (MP:get-pid)))))

;; @syntax (:release <inst>)
;; @param <inst> an instance of RLock
;; <p>Releases the RLock.</p>
(define (RLock:release inst)
  (unless (:held? inst)
    (throw-error "lock is not held"))
  (unless (:process-is-owner? inst)
    (throw-error "owner and releasing process do not match"))
  (:dec inst)
  (when (zero? (:get (:counter inst)))
    (:release (inst 1))))

;;;=============================================================================
;;; Event: a simple mechanism to signal one or more waiting processes that some
;;; condition has been met.
;;;=============================================================================

;; @syntax (Event)
;; <p>An Event is a simple mechanism for synchronization. It allows multiple
;; processes to block until a controlling process issues a signal to
;; unblock.</p>
(define (Event:Event , mem)
  (list (context) (Synchronized 0)))

;; @syntax (:reset inst)
;; @param <inst> an instance of Event
;; <p>Resets this Event. Does <no> checking to see if any processes are
;; waiting on this event. Those processes will remain locked until this
;; Event is signaled again.</p>
(define (Event:reset inst)
  (:set (inst 1) 0))

;; @syntax (:signaled? inst)
;; @param <inst> an instance of Event
;; <p>Returns true if this Event has already been signaled.
(define (Event:signaled? inst)
  (= 1 (:get (inst 1))))

;; @syntax (:signal inst)
;; @param <inst> an instance of Event
;; <p>Signals <inst> and unblocks any processes waiting on this Event.</p>
(define (Event:signal inst)
  (:set (inst 1) 1))

;; @syntax (:wait <inst> [<int-timeout>])
;; @param <inst> an instance of Event
;; @param <int-timeout> the maximum number of milliseconds to wait
;; <p>Blocks until <inst> is signaled or <int-timeout>, if present, expires.
;; Returns true when exiting normally, nil if wait times out.</p>
(define (Event:wait inst timeout)
  (unless (:signaled? inst)
    (MP:wait (fn () (:signaled? inst)) 50 500 timeout)))

;;;=============================================================================
;;; Pipe: a one-way communications pipe.
;;;=============================================================================

;; @syntax (Pipe [<in> <out>])
;; @param <in> an in-channel of an existing pipe
;; @param <out> an out-channel of an existing pipe
;; <p>A Pipe is a one-way communcations channel. If <in> and <out> are supplied,
;; an existing pipe (such as the result of the pipe function) may be used.</p>
(define (Pipe:Pipe in out)
  (unless (and in out)
    (map set '(in out) (pipe)))
  (list (context) in out))

(define (Pipe:in inst)
  (inst 1))

(define (Pipe:out inst)
  (inst 2))

;; @syntax (:send <inst> <msg>)
;; @param <inst> an instance of Pipe
;; @param <msg> the message to send
;; <p>Sends a message along the Pipe. Returns the number of bytes sent
;; (including message encoding).</p>
;; @example
;; (setf p (Pipe))
;; (:send p "Hello world.")
(define (Pipe:send inst msg , expr)
  (setf expr msg)
  (setf msg (source 'expr))
  (write-buffer (:out inst) (string "<msg>" msg "</msg>")))

;; @syntax (:peek <inst>)
;; @param <inst> an instance of Pipe
;; <p>Returns the number of bytes ready for reading on Pipe. This does not
;; correspond directly with the size of the message (extra data is send with
;; the message).
(define (Pipe:peek inst)
  (peek (:in inst)))

;; @syntax (:has-messages? <inst>)
;; @param <inst> an instance of Pipe
;; <p>Returns true if there is a message ready to be read from the Pipe.</p>
(define (Pipe:has-messages? inst)
  (not (zero? (:peek inst))))

;; @syntax (:receive <inst> [<block>])
;; @param <inst> an instance of Pipe
;; @param <block> when true, blocks until a message is available on the Pipe
;; <p>Returns the next message on the Pipe. By default, blocks until the next
;; message is available.</p>
(define (Pipe:receive inst (block true) , msg buf has-messages expr)
  (setf has-messages (:has-messages? inst))
  (when (or has-messages ; there is a message waiting
            (and block (not has-messages))) ; no messages but we will wait
    (setf msg "")
    (until (ends-with msg "</msg>")
      (read-buffer (:in inst) buf 4096 "</msg>")
      (write-buffer msg buf))
    (setf msg (slice msg 5 (- (length msg) 5 6)))
    
    ; eval msg in 'MAIN to work-around source/eval-string bug with FOOP contexts
    (context 'MAIN)
    (eval-string msg)
    (context 'Pipe)
    
    expr))

;; @syntax (:close <inst>)
;; @param <inst> an instance of Pipe
;; <p>Closes the read and write handles for this Pipe.</p>
(define (Pipe:close inst)
  (close (inst 1))
  (close (inst 2)))

;;;=============================================================================
;;; Channel: a two-way communcations channel using Pipes.
;;;=============================================================================

;; @syntax (Channel)
;; <p>Creates a two-way communcations channel using Pipes. Channels have two
;; Pipes, a parent and a child, each of which may be given to separate processes
;; to communicate back and forth using the standard Pipe syntax.</p>
;; @example
;; (setf ch (Channel))
;; (map set '(parent child) (:pipes ch))
;; 
;; ; in parent, send a message
;; (:send parent "Hello child.")
;; 
;; ; fork child, receive the message, and send a response
;; (fork
;;  (begin
;;    (println "Child received: " (:receive child))
;;    (:send child "Hello yourself!")))
;; 
;; ; in the parent process, block until a response becomes available
;; (setf resp (:receive parent))
;; (println "Parent received: " resp)
(define (Channel:Channel , parent child)
  (setf parent (pipe))
  (setf child (pipe))
  (list (context) (Pipe (parent 0) (child 1)) (Pipe (child 0) (parent 1))))

;; @syntax (:pipes <inst>)
;; @param <inst> an instance of Channel
;; <p>Returns a list of the parent and child pipes. Equivalent to
;; (list (:parent inst) (:child inst)).</p>
(define (Channel:pipes inst)
  (list (:parent inst) (:child inst)))

;; @syntax (:parent <inst>)
;; @param <inst> an instance of Channel
;; <p>Returns the parent Pipe.</p>
(define (Channel:parent inst)
  (inst 1))

;; @syntax (:child <inst>)
;; @param <inst> an instance of Channel
;; <p>Returns the child Pipe.</p>
(define (Channel:child inst)
  (inst 2))

;;;=============================================================================
;;; Queue: a synchronized FIFO queue. Objects may be of any size (share is not
;;; used).
;;;=============================================================================

;; @syntax (Queue [<size>])
;; @param <size> the maximum size for the queue (no max if nil)
;; <p>A Queue is a synchronized first in, first out list of items that is safe
;; for use in multiple processes. Object size is not restricted as when using
;; a shared page of memory. A Queue must be closed when no longer needed using
;; the :close method.</p>
(define (Queue:Queue size)
  (list (context) (Pipe) (Semaphore) (when size (Semaphore size))))

(define (Queue:comm inst)
  (inst 1))

(define (Queue:lock inst)
  (inst 2))

(define (Queue:counter inst)
  (inst 3))

;; @syntax (:count <inst>)
;; @param <inst> an instance of Queue
;; <p>Returns the number of items currently in the queue.</p>
(define (Queue:count inst)
  (:count (:counter inst)))

(define (Queue:inc inst)
  "Records an increase in the size of the queue."
  (when (:counter inst)
    (:dec (:counter inst))))

(define (Queue:dec inst)
  "Records a decrease in the size of the queue."
  (when (:counter inst)
    (:inc (:counter inst))))

;; @syntax (:put <inst> <expr> [<block>])
;; @param <inst> an instance of Queue
;; @param <expr> the object to be added
;; @param <block> when true, blocks until space is available in the queue
;; <p>Adds <expr> to the Queue, blocking by default. Returns true when the item
;; was added. If <block> is nil, returns nil when the Queue is full.</p>
;; @example
;; (setf q (Queue 4))
;; (dotimes (i 5)
;;   (if (:put q i nil) (print i)))
;; => 0123
(define (Queue:put inst expr (block true))
  (when (or block (not (zero? (:count inst))))
    (:inc inst)
    (:send (:comm inst) expr)
    true))

;; @syntax (:get <inst> [<block>])
;; @param <inst> an instance of Queue
;; @param <block> when true, blocks until an item is available from the queue
;; <p>Pulls the next item off of the Queue. If <block> is nil, returns nil when
;; no item is available. Otherwise, blocks until one becomes available.</p>
(define (Queue:get inst (block true) , msg)
  (MP:with-lock-held (:lock inst)
    (setf msg (:receive (:comm inst) block)))
  (when msg
    (:dec inst)
    msg))

;; @syntax (:close <inst>)
;; @param <inst> an instance of Queue
;; <p>Closes the Queue and removes its temporary files.</p>
(define (Queue:close inst)
  (:close (:comm inst)))






