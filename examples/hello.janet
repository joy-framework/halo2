(import ../src/halo2)


(defn hello [request]
  {:status 200
   :body "hello world!"
   :headers {"Content-Type" "text/plain"}})


(defn static [request]
  {:file (request :uri)})


(defn app [request]
  (case (request :uri)
    "/"
    (hello request)

    (static request)))


(halo2/server app 9021 "0.0.0.0")