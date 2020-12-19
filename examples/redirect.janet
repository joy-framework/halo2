(import ../src/halo2)

(defn home [request]
  {:status 200
   :body "hello world!"
   :headers {"Content-Type" "text/plain"}})

(defn submit [request]
  {:status 302
   :headers {"Location" "/"}})

(defn app [request]
  (case (request :uri)
    "/" (home request)

    "/submit" (submit request)

    # anything else is 404
    {:status 404}))

(halo2/server app 9021 "localhost")
