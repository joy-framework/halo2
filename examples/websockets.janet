(import ../src/halo2)

(defn home [request]
  {:status 200
   :body "hello world!"
   :headers {"Content-Type" "text/plain"}})

(defn static [request]
  {:file (string "./public" (request :path))})

(defn ws [socket]
  (case (socket :on)
    :connect
    (do
      (print "connected")
      (:send socket "Connected successfully"))

    :text
    (do
      (print (socket :message))
      (:send socket "Yes, I can hear you"))))


(defn app [request]
  (case [(request :scheme) (request :path)]
    ["http" "/"] (home request)
    ["ws" "/"] (ws request)
    # anything else is static
    (static request)))

(halo2/server app 9001 "localhost")
