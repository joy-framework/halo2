(declare-project
  :name "halo2"
  :description "A pure janet streaming http/1.1 server"
  :author "Sean Walker"
  :license "MIT"
  :url "https://github.com/joy-framework/halo2"
  :repo "git+https://github.com/joy-framework/halo2.git"
  :dependencies ["https://github.com/joy-framework/codec"
                 "https://github.com/joy-framework/cipher"])

(declare-source
  :source ["src/halo2.janet"])
