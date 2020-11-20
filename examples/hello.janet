(import ../src/halo2)

(defn hello [request]
  # (printf "%q" request)

  {:status 200
   :body "hello world!"
   :headers {"Content-Type" "text/plain"}})

(halo2/server hello 9021 "0.0.0.0")