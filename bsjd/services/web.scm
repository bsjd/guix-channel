(define-module (bsjd services web)
  #:use-module (guix gexp)
  #:export (%nginx-deploy-hook
            adguard-nginx-config         adguard-certbot-config
            searxng-nginx-config         searxng-certbot-config
            grafana-nginx-config         grafana-certbot-config
            prometheus-nginx-config      prometheus-certbot-config
            portainer-nginx-config       portainer-certbot-config
            node-red-nginx-config        node-red-certbot-config
            home-assistant-nginx-config  home-assistant-certbot-config
            deemix-nginx-config          deemix-certbot-config
            syncthing-nginx-config       syncthing-certbot-config
            fyt-nginx-config             fyt-certbot-config
            fytcoach-nginx-config        fytcoach-certbot-config
            cubetrek-nginx-config        cubetrek-certbot-config
            endurain-nginx-config        endurain-certbot-config
            auuki-nginx-config           auuki-certbot-config
            pgadmin-nginx-config         pgadmin-certbot-config
            org-roam-nginx-config        org-roam-certbot-config))

(define %nginx-deploy-hook
  (program-file "nginx-deploy-hook"
    #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
    (kill pid SIGHUP))))

(define-syntax nginx-certbot-config
  (syntax-rules ()
    ((_ domain host nginx-var certbot-var #:extra-config extra-config)
     (begin
       (define nginx-var
         (nginx-server-configuration
           (server-name `(,domain))
           (listen '("443 ssl"))
           (ssl-certificate (string-append "/etc/certs/" domain "/fullchain.pem"))
           (ssl-certificate-key (string-append "/etc/certs/" domain "/privkey.pem"))
           (locations
             (list
               (nginx-location-configuration
                 (uri "/")
                 (body (append (list (string-append "proxy_pass " host ";")) extra-config)))))
           (raw-content extra-config)))
       (define certbot-var
         (certificate-configuration
           (domains (list domain))
           (deploy-hook %nginx-deploy-hook)))))
    ((_ domain host nginx-var certbot-var)
     (nginx-certbot-config domain host nginx-var certbot-var #:extra-config '()))))

(nginx-certbot-config "adguard.lan"        "http://localhost:3000"
                      adguard-nginx-config adguard-certbot-config)
(nginx-certbot-config "searxng.lan"        "http://localhost:3001"
                      searxng-nginx-config searxng-certbot-config)
(nginx-certbot-config "grafana.lan"        "http://localhost:3002"
                      grafana-nginx-config grafana-certbot-config
                      #:extra-config '("proxy_set_header Host $http_host;"))
(nginx-certbot-config "prometheus.lan"     "http://localhost:3003"
                      prometheus-nginx-config prometheus-certbot-config
                      #:extra-config '("proxy_set_header Host $http_host;"))
(nginx-certbot-config "loki.lan"           "http://localhost:3004"
                      loki-nginx-config loki-certbot-config
                      #:extra-config '("proxy_set_header Host $http_host;"))
(nginx-certbot-config "portainer.lan"      "http://localhost:3005"
                      portainer-nginx-config portainer-certbot-config)
(nginx-certbot-config "node-red.lan"       "http://localhost:3006"
                      node-red-nginx-config node-red-certbot-config)
(nginx-certbot-config "home-assistant.lan" "http://localhost:3007"
                      home-assistant-nginx-config home-assistant-certbot-config)
(nginx-certbot-config "deemix"             "http://localhost:6595"
                      deemix-nginx-config deemix-certbot-config)
(nginx-certbot-config "syncthing.lan"      "http://localhost:8384"
                      syncthing-nginx-config syncthing-certbot-config)
(nginx-certbot-config "fyt.lan"            "http://localhost:9000"
                      fyt-nginx-config fyt-certbot-config)
(nginx-certbot-config "fytcoach.lan"       "http://localhost:9001"
                      fytcoach-nginx-config fytcoach-certbot-config)
(nginx-certbot-config "cubetrek.lan"       "http://localhost:9002"
                      cubetrek-nginx-config cubetrek-certbot-config)
(nginx-certbot-config "endurain.lan"       "http://localhost:9003"
                      endurain-nginx-config endurain-certbot-config)
(nginx-certbot-config "auuki.lan"          "http://localhost:9004"
                      auuki-nginx-config auuki-certbot-config)
(nginx-certbot-config "pgadmin.lan"        "http://localhost:9876"
                      pgadmin-nginx-config pgadmin-certbot-config)
(nginx-certbot-config "org-roam.lan"       "http://localhost:35901"
                      org-roam-nginx-config org-roam-certbot-config)
