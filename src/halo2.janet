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


(defn content-length [headers]
  (when-let [cl (get headers "Content-Length")]
    (scan-number cl)))


(defn request-parsing-complete? [buf {:headers headers :body body}]
  (let [cl (content-length headers)]
    (or (= cl (length body))
        (and (nil? cl)
             (string/has-suffix? "\r\n\r\n" buf)))))


(defn close-connection? [req]
  (let [conn (get-in req [:headers "Connection"])]
    (= "close" conn)))


(def request-peg (peg/compile '{:main (sequence :request-line :crlf (some :headers) :crlf)
                                :request-line (sequence (capture (to :sp)) :sp (capture (to :sp)) :sp "HTTP/" (capture (to :crlf)))
                                :headers (sequence (opt :crlf) (capture (to ":")) ": " (capture (to :crlf)))
                                :sp " "
                                :crlf "\r\n"}))


(defn body [headers buf]
  (if-let [len (content-length headers)]
    # inc required to get the full content-length up to crlf
    (string/slice buf (* -1 (inc len)))
    ""))


(defn request [buf]
  (when-let [parts (peg/match request-peg buf)
             [method uri http-version] parts
             headers (table ;(drop 3 parts))
             body (body headers buf)
             req @{:headers headers
                   :uri uri
                   :method method
                   :http-version http-version
                   :body body}]
    (if (request-parsing-complete? buf req)
      req
      nil)))


(defn http-response-headers [headers]
  (as-> (pairs headers) ?
        (map (fn [[k v]] (string/format "%s: %s" k v)) ?)
        (string/join ? "\r\n")))


(defn http-response
  "Turns a response dictionary into an http response string"
  [response]
  (let [status (get response :status 200)
        status-message (get status-messages status "Unknown Status Code")
        body (get response :body "")
        headers (get response :headers {})
        headers (merge {"Content-Length" (string (length body))} headers)
        headers (http-response-headers headers)]
    (string/format "HTTP/1.1 %d %s\r\n%s\r\n\r\n%s"
                   status
                   status-message
                   headers
                   body)))


(defn connection-handler
  "A function for turning circlet http handlers into stream handlers"
  [handler]
  (fn [stream]
    (def buf @"")

    (defer (:close stream)
      (while (:read stream 1024 buf)
        (when-let [req (request buf)
                   res (handler req)
                   response (http-response res)]

          # write the response to the stream
          (:write stream response)

          # clear buffer for memory?
          (buffer/clear buf)

          # close connection right away if Connection: close
          (when (close-connection? req)
            (break)))))))


(defn server [handler port &opt host]
  (default host "localhost")

  (let [socket (net/server host port)]
    (printf "Starting server on %s:%s" host (string port))

    (forever
      (when-let [conn (:accept socket)]
        (ev/call (connection-handler handler) conn)))))


