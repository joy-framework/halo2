(import ../src/halo2)

# You can listen to the events in the browser console with:
# ```js
# var a = new EventSource("/");
# a.onmessage = console.log;
# ```

(defn handler [request]
  {:status 200 
   :headers {"Content-Type" "text/event-stream"
             "Cache-Control" "no-cache"
             "Connection" "keep-alive"}
   :body (coro
           (each i (range 1 50)
             (yield (string "data: Message " i "\n\n"))
             (ev/sleep 0.1)))})

(halo2/server handler 9021 "localhost")
