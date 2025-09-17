(import ../src/halo2)

(defn home [request]
  {:status 200
   :body "hello world!"
   :headers {"Content-Type" "text/plain"}})

(defn static [request]
  {:file (request :uri)})

(defn post [request]
  (printf "%q" request)
  {:status 302
   :headers @{"Location" "/"}})


(defn invalid [request]
  "Returns an invalid response: structs/tables can't be returned directly as body.
  They must be converted to string first.
  This will trigger a 500 Internal Server Error."
  {:status 200
   :body {:data "invalid"}})

(defn app [request]
  (case (request :uri)
    "/" (home request)
    "/post" (post request)
    "/invalid" (invalid request)

    # anything else is static
    (static request)))

(halo2/server app 9021 "localhost")
