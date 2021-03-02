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
                  "wasm" "application/wasm"})

(var- *max-size* 8_192) # 8k max body size

(def request-peg
  (peg/compile ~{:main (sequence :request-line :crlf (group (some :headers)) :crlf (opt :body))
                 :request-line (sequence (capture (to :sp)) :sp (capture (to :sp)) :sp "HTTP/" (capture (to :crlf)))
                 :headers (sequence (capture (to ":")) ": " (capture (to :crlf)) :crlf)
                 :body (capture (some (if-not -1 1)))
                 :sp " "
                 :crlf "\r\n"}))

(def path-peg
  (peg/compile '(capture (some (if-not (choice "?" "#") 1)))))

(def content-length-peg (peg/compile ~(some (choice (sequence "Content-Length: " (cmt (capture (to "\r\n")) ,scan-number)) 1))))

(defn content-length [buf]
  (or (first (peg/match content-length-peg buf))
      0))


(defn content-type [s]
  (as-> (string/split "." s) _
        (last _)
        (get mime-types _ "text/plain")))


(defn close-connection? [req]
  (let [conn (get-in req [:headers "Connection"])]
    (= "close" conn)))


(defn request-headers [parts]
  (var output @{})

  (let [parts (partition 2 parts)]

    (each [k v] parts
      (if (get output k)
        (put output k (string (get output k) "," v))
        (put output k v))))

  output)


(defn request [buf]
  (when-let [parts (peg/match request-peg buf)
             [method uri http-version headers body] parts
             headers (request-headers headers)
             [path] (peg/match path-peg uri)]
    @{:headers headers
      :uri uri
      :method method
      :http-version http-version
      :path path
      :body body}))


(defn http-response-header [header]
  (let [[k v] header]
    (if (indexed? v)
      (string/format "%s: %s" k (string/join v ","))
      (string/format "%s: %s" k v))))


(defn http-response-headers [headers]
  (as-> (pairs headers) ?
        (map http-response-header ?)
        (string/join ? "\r\n")))


(defn file-exists? [str]
  (= :file (os/stat str :mode)))


(defn http-response-string [res]
  (let [status (get res :status 200)
        status-message (get status-messages status "Unknown Status Code")
        body (get res :body "")
        headers (get res :headers @{})
        headers (merge {"Content-Length" (string (length body))} headers)
        headers (http-response-headers headers)]
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
          status (if file-exists? 200 404)]
      (http-response-string @{:status status
                              :headers (merge {"Content-Type" content-type} headers)
                              :body body}))
    # regular http responses
    (http-response-string response)))


(defn payload-too-large? [req]
  (> (length (get req :body "")) *max-size*))


(defmacro ignore-socket-hangup! [& args]
  ~(try
     ,;args
     ([err fib]
      (unless (= err "Connection reset by peer")
        (propagate err fib)))))


(defn connection-handler
  "A function for turning circlet http handlers into stream handlers"
  [handler]
  (def buf (buffer/new 1024))

  (fn [stream]
    (ignore-socket-hangup!
      (defer (do (buffer/clear buf)
                 (:close stream))

        (while (:read stream 1024 buf 1)
          (when-let [content-length (content-length buf)
                     req (request buf)
                     _ (= content-length (length (get req :body "")))]

            # handle payload too large
            (when (payload-too-large? req)
             (:write stream (http-response-string @{:status 413})
              (break)))

            (->> req
                 handler
                 http-response
                 (:write stream))

            (buffer/clear buf)

            # close connection right away if Connection: close
            (when (close-connection? req)
              (break))))))))


(defn server [handler port &opt host max-size]
  (default host "localhost")
  (default max-size 8192)

  (set *max-size* max-size)

  (let [port (string port)
        socket (net/server host port)]
    (printf "Starting server on %s:%s" host port)

    (forever
      (when-let [conn (:accept socket)]
        (ev/call (connection-handler handler) conn)))))
