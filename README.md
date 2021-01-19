## halo2

halo2 is an http server for [janet](https://github.com/janet-lang/janet)

## Install

```sh
jpm install https://github.com/joy-framework/halo2
```

## Example

```clojure
(import halo2)

(defn handler [request]
  {:status 200 :body "halo2" :headers {"Content-Type" "text/plain"}})

(halo2/server handler 8080 "localhost")
```

Test that out with curl:

```sh
curl localhost:8080
# => halo2
```
