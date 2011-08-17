;; @module Sockets
;; @author Jeff Ober <jeffober@gmail.com>, Kanen Flowers <kanendosei@gmail.com>
;; @version 1.0
;; @location http://static.artfulcode.net/newlisp/sockets.lsp
;; @package http://static.artfulcode.net/newlisp/sockets.qwerty
;; @description Classes for socket operations (requires newlisp 10)
;; Sockets are a fast, efficient method for client/server operations. newLisp
;; has a broad range of functions for dealing with socket-based connections,
;; many of them somewhat arcane if not familiar with socket jargon. The Sockets
;; module encapsulates some of this complexity into two simple classes.
;; The Socket class provides methods for reading, writing, and sending lisp
;; expressions between client and server processes. Requires newlisp 10+ and
;; unix sockets.
;; 
;; <h4>Version history</h4>
;; <b>1.0</b>
;; &bull; initial release (replaces SocketServer module)
;; 
;; @example
;; ; Fork a server
;; (setf pid
;;   (fork
;;     (let ((server (SocketServer "/tmp/newlisp.sock")))
;;      (:run-server server
;;        (lambda (client server , request)
;;          (if client
;;            (begin
;;              (setf request (:read-expr client))
;;              (eval request) ; not safe unless you know your client
;;              (:write-expr client '(println "Hello, client!"))
;;              (:close client))
;;            (println "client connection error: " (net-error))))
;;        nil ; no operations between connections
;;        (lambda (err server) (println "An error has occurred: " err))))))
;;  
;; (sleep 500) ; give server time to start
;; 
;; ; Connect to server process as a client
;; (let ((socket (Sockets:make-file-socket "/tmp/newlisp.sock")))
;;   (:write-expr socket '(println "Hello, server!"))
;;   (setf expr (:read-expr socket))
;;   (:close socket))
;; (eval expr) ; not safe unless you know your server
;; 
;; (destroy pid) ; clean up

(context 'Sockets)

;; @syntax (Sockets:make-file-socket <str-file>)
;; @param <str-file> a file to use as a socket file
;; <p>Creates a new Socket instance using <str-file></p>.
;; @example
;; (setf socket (Sockets:create-socket-file "/tmp/my_app.sock"))
(define (make-file-socket file)
  (Socket:Socket (net-connect file)))

;; @syntax (Sockets:make-network-socket <str-host> <int-port>)
;; @param <str-host> the HTTP hostname of the remote server
;; @param <int-port> the remote server's port number to connect to
;; <p>Creates a new Socket instance connected to <str-host>:<int-port>.</p>
;; @example
;; (setf socket (Sockets:create-network-soket "http://www.artfulcode.net" 80))
(define (make-network-socket host port)
  (Socket:Socket (net-connect host port)))

(context 'MAIN)

;; @syntax (Socket <socket>)
;; @param <socket> an open socket file descriptor
;; <p>A Socket is a wrapper around a socket file descriptor and uses the
;; built-in net-* functions to manage socket operations.</p>
(define (Socket:Socket socket)
  (when (net-error) (throw-error (net-error)))
  (list (context) socket))

;; @syntax (:socket <inst>)
;; @param <inst> an instance of Socket
;; <p>Returns the socket file descriptor.</p>
(define (Socket:socket inst)
  (inst 1))

;; @syntax (:close <inst>)
;; @param <inst> an instance of Socket
;; <p>Closes the socket connection.</p>
(define (Socket:close inst)
  (net-close (:socket inst)))

;; @syntax (:peek <inst>)
;; @param <inst> an instance of Socket
;; <p>Returns the number of bytes ready to be read from the socket.</p>
(define (Socket:peek inst)
  (:peek (:socket inst)))

;; @syntax (:ready? <inst>)
;; @param <inst> an instance of Socket
;; <p>Returns true if the socket is ready for reading.</p>
(define (Socket:ready? inst)
  (not (zero? (:peek inst))))

;; @syntax (:write <inst> <str-message> [<int-chunk-size>])
;; @param <inst> an instance of Socket
;; @param <str-message> the message to send
;; @param <int-chunk-size> optional; sends message in chunks of size <int-chunk-size>
;; <p>Sends <str-message> along the Socket. If optional <int-chunk-size> is
;; specified, sends the message in chunks.</p>
(define (Socket:write inst message chunk-size , sent)
  (if-not chunk-size
    (net-send (:socket inst) message)
    (until (= (length message)
              (inc sent (net-send (:socket inst)
                                  (slice message sent)
                                  chunk-size)))
      sent)))

;; @syntax (:read-chunk <inst> <int-bytes> [<block>])
;; @param <inst> an instance of Socket
;; @param <int-bytes> the number of bytes to read
;; @param <block> optional; if explicitly nil, does not block for bytes to be available
;; <p>Blocks until a maximum of <int-bytes> are available to read from the
;; socket and then returns the resulting string. If <blocks> is explicitly
;; passed as nil, returns nil when no bytes are available to read.</p>
(define (Socket:read-chunk inst bytes (block true) , bytes-read buf)
  (when (net-error) (throw-error (net-error)))
  (when (or block (:ready? inst))
    (setf bytes-read (net-receive (:socket inst) buf bytes))
    (unless (zero? bytes-read)
      buf)))

;; @syntax (:read <inst> [<chunk-size> [<block>]])
;; @param <inst> an instance of Socket
;; @param <chunk-size> the number of bytes to read at a time (default is 512)
;; @param <block> whether to block for data (default is true)
;; <p>Reads from the socket until there is nothing left to read. Will block
;; until something is available to read unless block is nil. This can lead
;; to an indefinitely blocking read if neither end closes the connection.</p>
(define (Socket:read inst (chunk-size 512) (block true) , buf str)
  (setf buf "" str "")
  (while (setf buf (:read-chunk inst chunk-size))
    (write-buffer str buf))
  str)

;; @syntax (:read-line <inst>)
;; @param <inst> an instance of Socket
;; <p>Reads one line from the socket. Read-line will continue to read from
;; socket until and end-line character (\n or \r or both) is found. This
;; is not as fast as :read or :read-chunk, as it reads byte-by-byte, and
;; should not be used unless the source is trusted!</p>
(define (Socket:read-line inst , buf str)
  (setf buf "" str "")
  (while (and (!= buf "\n") (setf buf (:read-chunk inst 1)))
    (write-buffer str buf))
  (replace {(\s*$)} str "" 4))

;; @syntax (:write-expr <inst> <expr>)
;; @param <inst> an instance of Socket
;; @param <expr> a quoted lisp expression
;; <p>Sends <expr> along the socket. The other end of the socket may
;; then use :read-expr to read the expression back into lisp. Optionally,
;; <expr> may be a string, in which case it will be converted to the
;; expression '(string <expr>). Combined with :read-expr, this provides a
;; simple way to move complex data across a socket connection.</p>
(define (Socket:write-expr inst expr)
  (:write inst (string (if (string? expr) (list 'string expr) expr))))

;; @syntax (:read-expr <inst>)
;; @param <inst> an instance of Socket
;; <p>Reads an expression from the socket. Returns a valid lisp expression.
;; This function will block until a valid lisp expression is encountered.
;; Combined with :write-expr, this provides a simple way to move complex
;; data across a socket connection.</p>
(define (Socket:read-expr inst , buf src expr)
  (setf buf "" src "" expr "")
  (while (and (not (setf expr (read-expr src 'MAIN nil)))
              (setf buf (:read-chunk inst 1)))
    (write-buffer src buf))
  expr)


(new 'Socket 'SocketServer)

;; @syntax (SocketServer <int-port>)
;; @syntax (SocketServer <str-file>)
;; @param <int-port> the port number to listen on
;; @param <str-file> alternately, the socket file to listen on
;; <p>Creates a new SocketServer from a port number or socket file path.
;; SocketServer inherits directly from Socket, so all I/O methods from 
;; Socket are available to SocketServer instances. Throws an error if it
;; encounters a problem creating the socket.</p>
(define (SocketServer:SocketServer p , socket)
  (setf socket
    (cond
      ((integer? p) (net-listen p))
      ((string? p) (if (number? p) (net-listen (int p)) (net-listen p)))
      (true (throw-error "Invalid port or socket file path."))))
  (if (net-error)
    (throw-error (net-error))
    (list (context) socket)))

;; @syntax (:accept <inst>)
;; @param <inst> an instance of SocketServer
;; <p>Blocks until a connection is available and returns a new Socket
;; instance for the resulting connection.</p>
(define (SocketServer:accept inst)
  (setf socket (net-accept (:socket inst)))
  (unless (net-error) (Socket socket)))

;; @syntax (:until-connection-ready <inst> <func> [<poll-length>])
;; @param <inst> an instance of SocketServer
;; @param <func> a function to call while awaiting a connection
;; @param <poll-length> the number of ms to wait for a connection on each poll
;; <p>Repeatedly calls <func> until a client connection is ready on the socket.
;; Optional parameter <poll-timeout> (milliseconds) controls the length of time
;; to wait for a connection before calling func. The function is passed the
;; server instance.</p>
(define (SocketServer:until-connection-ready inst func (poll-length 100))
  (until (net-select (:socket inst) "read" poll-length)
    (func inst)))

;; @syntax (:on-connection <inst> <func>)
;; @param <inst> an instance of SocketServer
;; @param <func> a function to call when a client connects
;; <p>Blocks for a client connection and then calls <func> with the client
;; socket and the server instance. If an error occurs accepting the connection,
;; nil is passed to <func>. The error is then available via net-error.</p>
(define (SocketServer:on-connection inst func)
  (func (:accept inst) inst))

;; @syntax (:run-server <inst> <func-connect> [<func-wait> [<func-err>] [<poll-length>]])
;; @param <inst> an instance of SocketServer
;; @param <func-connect> called when a client connects
;; @param <func-wait> called repeatedly while awaiting a client
;; @param <func-err> called when a server error occurs
;; @param <poll-length> the number of ms to wait for a connection on each poll
;; <p>Runs the server in a loop. If <func-connect> is provided, it will be
;; called in the event of a server error. It is passed the error string and
;; server instance. If <func-wait> is provided, it will be called repeatedly
;; until a client connection is available. The only required parameters are the
;; SocketServer instance and <func-connect>, which will be called when a client
;; connection is accepted. It will be passed the client socket and the server
;; instance. If <func-err> is not defined, the server will throw an error when
;; an error occurs.</p>
(define (SocketServer:run-server inst on-connect until-connect on-error (poll-length 100))
  (while true
    (cond
      ((and (net-error) on-error) (on-error (net-error) server))
      ((net-error) (throw-error (net-error)))
      (until-connect
        (:until-connection-ready inst until-connect poll-length)
        (:on-connection inst on-connect))
      (true (:on-connection inst on-connect)))))









