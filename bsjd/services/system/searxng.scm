(define-module (bsjd services searxng)
  #:use-module (gnu services docker)
  #:use-module (gnu services)
  #:use-module (gnu services web)
  #:use-module (gnu services certbot)
  #:use-module (momo nginx-certbot-config)
  #:export (searxng-service
            searxng-nginx-config
            searxng-certbot-config))

(define searxng-service
  (service oci-container-service-type
    (list
      (oci-container-configuration
        (image "paulgoio/searxng:production")
        (requirement '(searxng-redis))
        (provision "searxng")
        (ports '("8083:8080"))
        (environment
          '(("PUBLIC_INSTANCE" . "true")
            ("LIMITER" . "true")
            ("REDIS_URL" . "redis://searxng-redis:6379/0")
            ("IMAGE_PROXY" . "true")
            ("UWSGI_WORKERS" . "8")
            ("UWSGI_THREADS" . "4")
            ("BASE_URL" . "https://search.lan")
            ("NAME" . "My Personal Search")
            ("SEARCH_DEFAULT_LANG" . "en-US")
            ("SEARCH_ENGINE_ACCESS_DENIED" . "60")))
        (network "backend")
        (extra-arguments
          '("--cap-drop=ALL"
            "--cap-add=NET_ADMIN"
            "--cap-add=SETGID"
            "--cap-add=SETUID"
            "--log-driver=json-file"
            "--log-opt=max-size=1m"
            "--log-opt=max-file=1")))
      (oci-container-configuration
        (image "redis:alpine")
        (provision "searxng-redis")
        (command '("redis-server" "--save" "" "--appendonly" "no"))
        (network "backend")
        (extra-arguments
          '("--tmpfs=/var/lib/redis"
            "--cap-drop=ALL"
            "--cap-add=SETGID"
            "--cap-add=SETUID"
            "--cap-add=DAC_OVERRIDE"))))))

(nginx-certbot-config "search.lan" "http://localhost:8083"
                      searxng-nginx-config searxng-certbot-config)
