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


(def- mime-types {".txt" "text/plain"
                  ".css" "text/css"
                  ".js" "application/javascript"
                  ".json" "application/json"
                  ".xml" "text/xml"
                  ".svg" "image/svg+xml"
                  ".jpg" "image/jpeg"
                  ".jpeg" "image/jpeg"
                  ".gif" "image/gif"
                  ".png" "image/png"})

(def- MAX_SIZE 1024)
(def- CRLF_2 "\r\n\r\n")

(def head-peg (peg/compile '{:main (sequence :request-line :crlf (some :headers) :crlf)
                             :request-line (sequence (capture (to :sp)) :sp (capture (to :sp)) :sp "HTTP/" (capture (to :crlf)))
                             :headers (sequence (capture (to ":")) ": " (capture (to :crlf)) :crlf)
                             :sp " "
                             :crlf "\r\n"}))


(defn content-length [req]
  (when-let [cl (get-in req [:headers "Content-Length"])]
    (scan-number cl)))


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


(defn request [head]
  (when-let [parts (peg/match head-peg head)
             [method uri http-version] parts
             headers (request-headers (drop 3 parts))]
    @{:headers headers
      :uri uri
      :method method
      :http-version http-version}))


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


(defn http-response
  "Turns a response dictionary into an http response string"
  [response]
  (var res response)
  (def {:file file} response)

  # check for static files
  (when file
    (if (file-exists? file)
      (let [ext (->> file
                     (string/find-all ".")
                     (last)
                     (string/slice file))
            content-type (get mime-types ext)
            body (-> file slurp string)]
        (set res @{:status 200
                   :headers @{"Content-Type" content-type}
                   :body body}))
      (set res @{:status 404
                 :headers @{"Content-Type" "text/plain"}
                 :body "Not found"})))

  # regular http responses
  (let [status (get res :status 200)
        status-message (get status-messages status "Unknown Status Code")
        body (get res :body "")
        headers (get res :headers @{})
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
      (while (:read stream MAX_SIZE buf)

        # handle payload too large
        (when (> (length buf) MAX_SIZE)
          (:write stream (http-response @{:status 413}))
          (break))

        # parse request
        (when-let [idx (+ (string/find CRLF_2 buf) 4)
                   head (buffer/slice buf 0 idx)
                   req (request head)
                   _ (if-let [size (content-length req)
                              body (buffer/slice buf idx)]
                       (when (= size (length body))
                         (put req :body body))
                       true)]

          (->> req
               handler
               http-response
               (:write stream))

          (buffer/clear buf)

          # close connection right away if Connection: close
          (when (close-connection? req)
            (break)))))))


(defn server [handler port &opt host]
  (default host "localhost")

  (let [socket (net/server host (string port))]
    (printf "Starting server on %s:%s" host (string port))

    (forever
      (when-let [conn (:accept socket)]
        (ev/call (connection-handler handler) conn)))))
