(import halo2)

(defn handler [request]
  {:status 200 :body "halo2" :headers {"Content-Type" "text/plain"}})

(halo2/server handler 8080 "localhost")
