(import ../src/halo2)

(defn home [request]
  {:status 200
   :body "hello world!"
   :headers {"Content-Type" "text/plain"}})

(defn static [request]
  {:file (request :uri)})

(defn app [request]
  (case (request :uri)
    "/" (home request)

    # anything else is static
    (static request)))

(halo2/server app 9021 "localhost")
