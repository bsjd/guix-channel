(define-module (bsjd packages basedpyright)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system node)
  #:use-module (guix gexp)
  #:use-module (guix licenses))

(define-public node-basedpyright-1.33.0
  (package
    (name "node-basedpyright")
    (version "1.33.0")
    (source
      (origin
        (method url-fetch)
        (uri
          "https://github.com/DetachHead/basedpyright/releases/download/v1.33.0/basedpyright-1.33.0.tar.gz"
        (sha256
          (base32 "0fi43rrrl3819hfizflnxzkxn2n0bc6ljx8wr70di4sv539ddr57"))))
    (build-system node-build-system)
    (arguments
      (list
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (delete 'build)
            (add-after 'patch-dependencies 'delete-dev-dependencies
              (lambda _
                (modify-json (delete-dependencies '("shx" "webpack"
                                                    "ts-loader"
                                                    "typescript"
                                                    "@types/node"
                                                    "webpack-cli"
                                                    "esbuild-loader"
                                                    "copy-webpack-plugin"))))))))
      (home-page "https://github.com/detachhead/basedpyright#readme")
      (synopsis
      "a fork of pyright with various type checking improvements, pylance features and more.")
      (description
      "a fork of pyright with various type checking improvements, pylance features and more.")
      (license expat)))
node-basedpyright-1.33.0
