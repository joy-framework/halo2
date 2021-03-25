(import ../src/halo2)

(defn home [request]
  {:status 200
   :body "hello world!"
   :headers {"Content-Type" "text/plain"}})

(defn static [request]
  {:file (string "./public" (request :path))})

(defn ws [request]
  (case (request :on)
    :connect
    (print "connected")

    :text
    (do
      (printf "received a message: %q" (request :message))
      "text response")))


(defn app [request]
  (case (request :path)
    "/" (home request)
    nil (ws request)
    # anything else is static
    (static request)))

(halo2/server app 9001 "localhost")
