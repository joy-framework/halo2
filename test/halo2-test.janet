# conditional imports
(defmacro try! [& forms]
  ~(try
    (do ,;forms)
    ([_])))

(import ../src/halo2 :prefix "")

# tester is required
# but didn't want to add as dependency
# $ jpm install tester
# to install
# then
# $ jpm test
# to run
(try!
  (import tester :prefix "" :exit true))

(deftests
  (test "response"
    (is (deep= "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 4\r\n\r\ntest"
               (http-response {:status 200
                               :headers {"Content-Type" "text/plain"}
                               :body "test"}))))

  (test "request peg"
    (is (deep= @{:http-version "1.1"
                 :headers @{"Content-Type" "text/plain"}
                 :uri "/"
                 :method "GET"}
               (request @"GET / HTTP/1.1\r\nContent-Type: text/plain\r\n\r\n")))))
