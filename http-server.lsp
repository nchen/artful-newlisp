;;; TO DO: documentation
;;; TO DO: pre-forking handlers using sockets or queues

(load "nlmod/http.lsp")
(load "nlmod/sockets.lsp")
(load "nlmod/util.lsp")

(context 'HttpServer)
(constant 'MAX_REQ_SIZE (* 1024 1024 8)) ; 8 megabytes
(context 'MAIN)

(define (HttpServer:HttpServer (port 80) fn-req fn-wait fn-error (log-fd (device)) max-req , server)
  (setf server
    (list (context)
          (SocketServer port)
          log-fd
          (or max-req HttpServer:MAX_REQ_SIZE)
          fn-req
          fn-wait
          fn-error))
  (:log server "Server starting up on port " port)
  server)

(define (HttpServer:socket-server inst) (inst 1))
(define (HttpServer:fd inst) (inst 2))
(define (HttpServer:max-request-size) (inst 3))
(define (HttpServer:request-fn inst) (eval (inst 4)))
(define (HttpServer:wait-fn inst) (eval (inst 5)))
(define (HttpServer:error-fn inst) (eval (inst 6)))
(define (HttpServer:client-socket inst) (inst 7)) ; added in :request-handler

(define (HttpServer:log inst)
  (let ((old-device (device)))
    (device (:fd inst))
    (println (string (join (map string (args)))))
    (device old-device)))

(define (HttpServer:respond inst response mime-type code headers)
  (setf mime-type (or mime-type "text/html"))
  (setf code (or code 200))
  (setf headers (or headers (list)))
  (:write (:client-socket inst) (Http:format-response response code mime-type headers))
  (:close (:client-socket inst)))

(define (HttpServer:request-handler inst socket server , buf req)
  (when socket
    (push socket inst -1)
    (setf buf (:read-chunk socket (:max-request-size inst)))
    (setf req (Http:parse-request buf))
    ((:request-fn inst) inst (lookup "method" req) (lookup "path" req) (lookup "content" req))))

(define (HttpServer:wait-handler inst)
  (when (:wait-fn inst)
    ((:wait-fn inst))))

(define (HttpServer:error-handler inst)
  (if (:error-fn inst)
    ((:error-fn inst))
    (:log inst "Server error: " (args 0))))

(define (HttpServer:run-server inst)
  (:run-server
    (:socket-server inst)
    (partial HttpServer:request-handler inst)
    (partial HttpServer:wait-handler inst)
    (partial HttpServer:error-handler inst)))