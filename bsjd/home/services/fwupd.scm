;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (bsjd services fwupd)
  #:use-module (gnu packages firmware)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (fwupd-configuration
            fwupd-configuration?
            fwupd-configuration-fields
            fwupd-configuration-fwupd
            fwupd-service-type))

(define-configuration/no-serialization fwupd-configuration
  (fwupd
   (package fwupd)
   "The fwupd package that will be installed in the system profile."))

(define (fwupd-shepherd-services config)
  (let ((fwupd
         (fwupd-configuration-fwupd config)))
    (list
     (shepherd-service (provision '(fwupd))
                       (requirement
                        '(user-processes))
                       (documentation
                        "Run the fwupd daemon.")
                       (start
                        #~(make-forkexec-constructor
                           (list
                            #$(file-append fwupd "/libexec/fwupd/fwupd"))))
                       (stop
                        #~(make-kill-destructor))))))

(define fwupd-service-type
  (service-type (name 'fwupd)
                (extensions
                 (list
                  (service-extension shepherd-root-service-type
                                     fwupd-shepherd-services)
                  (service-extension profile-service-type
                                     (compose list fwupd-configuration-fwupd))
                  (service-extension dbus-root-service-type
                                     (compose list fwupd-configuration-fwupd))
                  (service-extension polkit-service-type
                                     (compose list fwupd-configuration-fwupd))))
                (default-value (fwupd-configuration))
                (description
                 "This service configures fwupd on the Guix System.")))

