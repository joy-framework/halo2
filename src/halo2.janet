(import codec)
(import cipher)

(def- status-messages
  {100 "Continue"
   101 "Switching Protocols"
   200 "OK"
   201 "Created"
   202 "Accepted"
   203 "Non-Authoritative Information"
   204 "No Content"
   205 "Reset Content"
   206 "Partial Content"
   300 "Multiple Choices"
   301 "Moved Permanently"
   302 "Found"
   303 "See Other"
   304 "Not Modified"
   305 "Use Proxy"
   307 "Temporary Redirect"
   400 "Bad Request"
   401 "Unauthorized"
   402 "Payment Required"
   403 "Forbidden"
   404 "Not Found"
   405 "Method Not Allowed"
   406 "Not Acceptable"
   407 "Proxy Authentication Required"
   408 "Request Time-out"
   409 "Conflict"
   410 "Gone"
   411 "Length Required"
   412 "Precondition Failed"
   413 "Request Entity Too Large"
   414 "Request-URI Too Large"
   415 "Unsupported Media Type"
   416 "Requested range not satisfiable"
   417 "Expectation Failed"
   500 "Internal Server Error"
   501 "Not Implemented"
   502 "Bad Gateway"
   503 "Service Unavailable"
   504 "Gateway Time-out"
   505 "HTTP Version not supported"})


(def- mime-types {"txt" "text/plain"
                  "css" "text/css"
                  "js" "application/javascript"
                  "json" "application/json"
                  "xml" "text/xml"
                  "html" "text/html"
                  "svg" "image/svg+xml"
                  "pg" "image/jpeg"
                  "jpeg" "image/jpeg"
                  "gif" "image/gif"
                  "png" "image/png"
                  "wasm" "application/wasm"
                  "gz" "application/gzip"})
(def- request-peg
  (peg/compile ~{:main (sequence :request-line :crlf (group (some :headers)) :crlf (opt :body))
                 :request-line (sequence (capture (to :sp)) :sp (capture (to :sp)) :sp "HTTP/" (capture (to :crlf)))
                 :headers (sequence (capture (to ":")) ": " (capture (to :crlf)) :crlf)
                 :body (capture (some (if-not -1 1)))
                 :sp " "
                 :crlf "\r\n"}))
(def- path-peg (peg/compile '(capture (some (if-not (choice "?" "#") 1)))))
(def- content-length-peg (peg/compile ~(some (choice (sequence "Content-Length: " (cmt (capture (to "\r\n")) ,scan-number)) 1))))


(defn- header-peg [s]
  (peg/compile ~(some (choice (sequence ,s ":" :s (capture (to "\r\n"))) 1))))


(def- content-length-peg (header-peg "Content-Length"))
(def- connection-peg (header-peg "Connection"))
(def- upgrade-peg (header-peg "Upgrade"))
(def- websocket-key-peg (header-peg "Sec-WebSocket-Key"))


(defn- content-length-header [buf]
  (scan-number
    (or (first (peg/match content-length-peg buf))
        "0")))


(defn- connection-header [buf]
  (first (peg/match connection-peg buf)))


(defn- upgrade-header [buf]
  (first (peg/match upgrade-peg buf)))


(defn- websocket-key [buf]
  (first (peg/match websocket-key-peg buf)))


(defn- content-type [s]
  (as-> (string/split "." s) _
        (last _)
        (get mime-types _ "text/plain")))


(defn- decode-headers [parts]
  (var output @{})

  (let [parts (partition 2 parts)]

    (each [k v] parts
      (if (get output k)
        (put output k (string (get output k) "," v))
        (put output k v))))

  output)


(defn- encode-header [header]
  (let [[k v] header]
    (if (indexed? v)
      (string/format "%s: %s" k (string/join v ","))
      (string/format "%s: %s" k v))))


(defn- encode-headers [headers]
  (as-> (pairs headers) ?
        (map encode-header ?)
        (string/join ? "\r\n")))


(defn request [buf]
  (when-let [parts (peg/match request-peg buf)
             [method uri http-version headers body] parts
             headers (decode-headers headers)
             [path] (peg/match path-peg uri)]
    @{:headers headers
      :uri uri
      :method method
      :http-version http-version
      :scheme "http"
      :path path
      :body body}))


(defn- file-exists? [str]
  (= :file (os/stat str :mode)))


(defn- http-response-string [res]
  (let [status (get res :status 200)
        status-message (get status-messages status "Unknown Status Code")
        body (get res :body "")
        headers (get res :headers @{})
        headers (merge {"Content-Length" (string (length body))} headers)
        headers (encode-headers headers)]
    (string "HTTP/1.1 " status " " status-message "\r\n"
            headers "\r\n\r\n"
            body)))


(defn http-response
  "Turns a response dictionary into an http response string"
  [response]
  # check for static files
  (if-let [file (get response :file)]
    (let [content-type (content-type file)
          headers (get response :headers {})
          file-exists? (file-exists? file)
          body (if file-exists? (slurp file) "not found")
          status (if file-exists? 200 404)
          gzip? (= "application/gzip" content-type)]
      (http-response-string @{:status status
                              :headers (merge headers {"Content-Type" content-type
                                                       "Content-Encoding" (when gzip? "gzip")})
                              :body body}))
    # regular http responses
    (http-response-string response)))


(defn- websocket? [connection-header upgrade-header]
  (and (= "Upgrade" connection-header)
       (= "websocket" upgrade-header)))


(defmacro- ignore-socket-hangup! [& args]
  ~(try
     ,;args
     ([err fib]
      (unless (or (= err "Connection reset by peer")
                  (= err "timeout"))
        (propagate err fib)))))


(def- ws-magic-string "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
(defn- websocket-handshake [buf]
  (let [key (websocket-key buf)
        response-key (as-> (string key ws-magic-string) _
                           (codec/sha1 _)
                           (cipher/hex2bin _)
                           (string _)
                           (codec/encode _))]
    (http-response
      @{:status 101
        :headers @{"Upgrade" "websocket"
                   "Connection" "Upgrade"
                   "Sec-WebSocket-Accept" response-key}})))


(defn- unmask-data [mask masked-data]
  (var i 0)
  (let [data @""]
    (each byte masked-data
      (buffer/push data (bxor byte (get mask (% i 4))))
      (++ i))
    data))


(defn- websocket-frame [buf]
  (let [bytes (buffer/slice buf 0 2)
        fin (band (bytes 0) 2r10000000)
        opcode (band (bytes 0) 2r00001111)
        masked? (band (bytes 1) 2r10000000)
        payload-size (band (bytes 1) 2r01111111)
        # read 4 mask bytes
        mask (buffer/slice buf 2 6)
        # read the payload size
        masked-data (buffer/slice buf 6 (+ 6 payload-size))]
        # unmask data
    (unmask-data mask masked-data)))


(defn- websocket-response [data]
  (buffer/push @"" 2r10000001 (length data) data))


(comment
  # read websocket frame metadata
  (let [bytes (:read connection 2)
        fin (band (bytes 0) 2r10000000)
        opcode (band (bytes 0) 2r00001111)
        masked? (band (bytes 1) 2r10000000)
        payload-size (band (bytes 1) 2r01111111)
        # read 4 mask bytes
        mask (:read connection 4)
        # read the payload size
        masked-data (:read connection payload-size)
        data (unmask-data mask masked-data)]
    # unmask data
    (as-> data _
          (buffer/push @"" 2r10000001 (length _) _)
          (:write connection _))))


(defn make-socket [connection]
  @{:connection connection
    :send (fn [self message]
            (:write connection (websocket-response message)))})


(defn- connection-handler
  "A function for turning http handlers into connection handlers"
  [handler max-size]
  (def buf @"")

  (fn [connection]
    (var handshake? true)
    (def Socket (make-socket connection))

    (ignore-socket-hangup!
     (defer (do (buffer/clear buf)
                (:close connection))
       # while keeps the socket open?
       # in case of http/1.1 or websockets
       (while (:read connection 1024 buf 30)
         (let [connection-header (connection-header buf)
               upgrade-header (upgrade-header buf)]

           # ws://
           (if (websocket? connection-header upgrade-header)
             (when-let [req (request buf)]
               (when handshake?
                 (:write connection (websocket-handshake buf))
                 (set handshake? false)
                 (handler (table/setproto (merge req @{:on :connect :scheme "ws"}) Socket)))
               # only handle single frame payloads (less than 127 bytes)
               (let [idx (string/find "\r\n\r\n" buf)
                     b (buffer/slice buf (+ 4 idx))
                     frame (if (empty? b) (:read connection 126) b)
                     data (websocket-frame frame)]
                  (handler (table/setproto (merge req @{:on :text :message data :scheme "ws"}) Socket))))
               # TODO ping pong
               # TODO websocket close

             # http://
             (let [content-length (content-length-header buf)]
               # handle 413
               (when (> content-length max-size)
                 (:write connection (http-response-string @{:status 413}))
                 (break))

               (when-let [req (request buf)
                          _ (= content-length (length (get req :body "")))]
                 (->> req
                      (handler)
                      (http-response)
                      (:write connection)))

               (buffer/clear buf)

               (when (= "close" connection-header)
                 (break))))))))))


(defn server [handler port &opt host max-size]
  (default host "localhost")
  (default max-size 8192)

  (let [port (string port)
        socket (net/server host port)]
    (printf "Starting server on %s:%s" host port)

    (forever
      (when-let [conn (:accept socket)]
        (ev/call (connection-handler handler max-size) conn)))))
