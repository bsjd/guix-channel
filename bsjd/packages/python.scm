(define-module (bsjd packages python)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system node)
  #:use-module (guix gexp)
  #:use-module (guix licenses))

(define-public basedpyright
  (package
    (name "basedpyright")
    (version "1.33.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/basedpyright/-/basedpyright-1.33.0.tgz")
        (sha256
          (base32 "0b45b9jbcxx8di4xg9q3xn559sm1jk4w0r7q0h3csxrnsl3j0ajs"))))
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
    (synopsis "a fork of pyright with various type checking improvements, pylance features and more.")
    (description "a fork of pyright with various type checking improvements, pylance features and more.")
    (license expat)))
