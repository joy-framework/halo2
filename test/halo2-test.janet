(import tester :prefix "" :exit true)
(import ../src/halo2 :prefix "")

# make sure tester is installed before running these tests

(defn string-contains [needle haystack]
  (def find-result (string/find needle haystack))
  (if (nil? find-result) 
    false 
    (>= find-result 0)))
  

(deftests
  (test "response"
    (is (deep= "HTTP/1.1 200 OK\r\nContent-Length: 4\r\nContent-Type: text/plain\r\n\r\ntest"
               (http-response {:status 200
                               :headers {"Content-Type" "text/plain"}
                               :body "test"}))))

  (test "response with headers of list"
    (do
      (def actual-result 
        (http-response {:status 200
                        :headers {
                          "A" "alpha" 
                          "List-Header" ["one" "two"] # two items in list, two header outputs
                          "Z" ["zed"] # one item in the list, valid
                          "Empty" []} # empty list, the header is not in the output
                        :body "test"}))
      (def expected-headers 
        {"A: alpha" true
         "List-Header: one" true
         "List-Header: two" true
         "Z: zed" true
         "Empty: " false}) # should not find this in http response
      (def look-result (map (fn [h] (string-contains h actual-result)) (keys expected-headers)))
      (is (deep= (values expected-headers) look-result))))

  (test "request peg"
    (is (deep= @{:http-version "1.1"
                 :headers @{"Content-Type" "text/plain"}
                 :uri "/"
                 :path "/"
                 :method "GET"}
               (request @"GET / HTTP/1.1\r\nContent-Type: text/plain\r\n\r\n")))))
