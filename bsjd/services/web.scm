(define-module (bsjd services web)
  #:use-module (guix gexp)
  #:use-module (gnu services certbot)
  #:use-module (gnu services web)
  #:export (%nginx-deploy-hook
            adguard-nginx-config
            searxng-nginx-config
            music-nginx-config
            grafana-nginx-config
            prometheus-nginx-config
            loki-nginx-config
            portainer-nginx-config
            node-red-nginx-config
            home-assistant-nginx-config
            deemix-nginx-config
            syncthing-nginx-config
            fyt-nginx-config
            fytcoach-nginx-config
            cubetrek-nginx-config
            endurain-nginx-config
            auuki-nginx-config
            pgadmin-nginx-config
            org-roam-nginx-config))

(define %nginx-deploy-hook
  (program-file "nginx-deploy-hook"
    #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
    (kill pid SIGHUP))))

(define-syntax bsjd/nginx-config
  (syntax-rules ()
    ((_ domain host nginx-var #:extra-config extra-config)
     (begin
       (define nginx-var
         (nginx-server-configuration
           (server-name `(,(string-append domain ".fytcoach.com") ,domain))
           (listen '("443 ssl"))
           (ssl-certificate "/etc/certs/lan.fytcoach.com/fullchain.pem")
           (ssl-certificate-key "/etc/certs/lan.fytcoach.com/privkey.pem")
           (locations
             (list
               (nginx-location-configuration
                 (uri "/")
                 (body (append (list (string-append "proxy_pass " host ";")) extra-config)))))
           (raw-content extra-config)))))
    ((_ domain host nginx-var)
     (bsjd/nginx-config domain host nginx-var #:extra-config '()))))

(bsjd/nginx-config "adguard.lan"        "http://localhost:3053" adguard-nginx-config)
(bsjd/nginx-config "searxng.lan"        "http://localhost:3001" searxng-nginx-config)
(bsjd/nginx-config "music.lan"          "http://localhost:3001" music-nginx-config)
(bsjd/nginx-config "grafana.lan"        "http://localhost:3002" grafana-nginx-config
  #:extra-config '("proxy_set_header Host $http_host;"))
(bsjd/nginx-config "prometheus.lan"     "http://localhost:3003" prometheus-nginx-config
  #:extra-config '("proxy_set_header Host $http_host;"))
(bsjd/nginx-config "loki.lan"           "http://localhost:3004" loki-nginx-config
  #:extra-config '("proxy_set_header Host $http_host;"))
(bsjd/nginx-config "portainer.lan"      "http://localhost:3005" portainer-nginx-config)
(bsjd/nginx-config "node-red.lan"       "http://localhost:3006" node-red-nginx-config)
(bsjd/nginx-config "home-assistant.lan" "http://localhost:3007" home-assistant-nginx-config)
(bsjd/nginx-config "deemix.lan"         "http://localhost:6595" deemix-nginx-config)
(bsjd/nginx-config "syncthing.lan"      "http://localhost:8384" syncthing-nginx-config)
(bsjd/nginx-config "fyt.lan"            "http://localhost:9000" fyt-nginx-config)
(bsjd/nginx-config "fytcoach.lan"       "http://localhost:9001" fytcoach-nginx-config)
(bsjd/nginx-config "cubetrek.lan"       "http://localhost:9002" cubetrek-nginx-config)
(bsjd/nginx-config "endurain.lan"       "http://localhost:9003" endurain-nginx-config)
(bsjd/nginx-config "auuki.lan"          "http://localhost:9004" auuki-nginx-config)
(bsjd/nginx-config "pgadmin.lan"        "http://localhost:9876" pgadmin-nginx-config)
(bsjd/nginx-config "org-roam.lan"       "http://localhost:35901" org-roam-nginx-config)
