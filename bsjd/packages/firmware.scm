(define-module (bsjd packages firmware)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix memoization)
  #:use-module (guix deprecation)
  #:use-module (guix packages)
  #:use-module (guix packages firmware)
  #:use-module (guix platform)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avr)
  #:use-module (gnu packages avr-xyz)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages efi)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages flashing-tools)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mingw)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:autoload (ice-9 pretty-print) (pretty-print)
  #:use-module (ice-9 regex)
  #:autoload (ice-9 textual-ports) (get-string-all)
  #:use-module (srfi srfi-26)

(define-public ovmf-secboot-x86-64
  (let ((base (make-ovmf-firmware "x86_64")))
    (package
      (inherit base)
      (name "ovmf-secboot-x86-64")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (replace 'install
                (lambda _
                  (let ((fmw (string-append #$output "/share/firmware")))
                    (mkdir-p fmw)

                    (copy-file "Build/OvmfX64/RELEASE_GCC/FV/OVMF_CODE.fd" (string-append fmw "/ovmf_code_x64.bin"))
                    (copy-file "Build/OvmfX64/RELEASE_GCC/FV/OVMF_VARS.fd" (string-append fmw "/ovmf_vars_x64.bin"))

                    (copy-file "Build/OvmfX64/RELEASE_GCC/FV/OVMF_CODE.secboot.fd" (string-append fmw "/ovmf_code_x64.secboot.bin"))
                    (copy-file "Build/OvmfX64/RELEASE_GCC/FV/OVMF_VARS.fd" (string-append fmw "/ovmf_vars_x64.secboot.bin"))

                    (copy-file "Build/OvmfX64/RELEASE_GCC/FV/OVMF_CODE_4M.fd" (string-append fmw "/ovmf_code_4m_x64.bin"))
                    (copy-file "Build/OvmfX64/RELEASE_GCC/FV/OVMF_VARS_4M.fd" (string-append fmw "/ovmf_vars_4m_x64.bin"))

                    (copy-file "Build/OvmfX64/RELEASE_GCC/FV/OVMF_CODE_4M.secboot.fd" (string-append fmw "/ovmf_code_4m_x64.secboot.bin"))
                    (copy-file "Build/OvmfX64/RELEASE_GCC/FV/OVMF_VARS_4M.fd" (string-append fmw "/ovmf_code_4m_x64.secboot.bin"))

                    (for-each
                     (lambda (file)
                       (copy-file
                        (string-append "Build/OvmfX64/RELEASE_GCC"
                                       "/FV/" file ".fd")
                        (string-append fmw "/" (string-downcase file) "_x64.secboot.bin")))
                     (list "OVMF"
                           "OVMF_CODE"
                           "OVMF_VARS"
                           )))))
              (add-after 'install 'install-qemu-firmware-metadata
                (lambda _
                  ;; The QEMU firmware metadata files are taken from the
                  ;; Fedora project (see:
                  ;; https://src.fedoraproject.org/rpms/edk2/tree/rawhide).
                  (let ((31-edk2-ovmf-2m-raw-x64-sb-enrolled.json-source #$(ovmf-aux-file "31-edk2-ovmf-2m-raw-x64-sb-enrolled.json"))
                        (31-edk2-ovmf-2m-raw-x64-sb-enrolled.json-dest (string-append #$output "/share/qemu/firmware/31-edk2-ovmf-2m-raw-x64-sb-enrolled.json"))
                        (41-edk2-ovmf-2m-raw-x64-sb.json-source #$(ovmf-aux-file "41-edk2-ovmf-2m-raw-x64-sb.json"))
                        (41-edk2-ovmf-2m-raw-x64-sb.json-dest (string-append #$output "/share/qemu/firmware/41-edk2-ovmf-2m-raw-x64-sb.json")))
                    (mkdir-p (dirname 31-edk2-ovmf-2m-raw-x64-sb-enrolled.json-dest))
                    (mkdir-p (dirname 41-edk2-ovmf-2m-raw-x64-sb.json-dest))
                    (copy-file 31-edk2-ovmf-2m-raw-x64-sb-enrolled.json-source
                               31-edk2-ovmf-2m-raw-x64-sb-enrolled.json-dest)
                    (copy-file 41-edk2-ovmf-2m-raw-x64-sb.json-source
                               41-edk2-ovmf-2m-raw-x64-sb.json-dest)
                    (substitute* 31-edk2-ovmf-2m-raw-x64-sb-enrolled.json-dest
                      (("/usr/share/edk2/ovmf/OVMF_(CODE|VARS).secboot.fd" _ kind)
                       (string-append
                        #$output "/share/firmware/ovmf_"
                        (string-downcase kind) "_x64.secboot.bin")))
                    (substitute* 41-edk2-ovmf-2m-raw-x64-sb.json-dest
                      (("/usr/share/edk2/ovmf/OVMF_CODE.secboot.fd" _ kind)
                       (string-append #$output "/share/firmware/ovmf_code_x64.secboot.bin")))
                    (substitute* 41-edk2-ovmf-2m-raw-x64-sb.json-dest
                      (("/usr/share/edk2/ovmf/OVMF_VARS.fd" _ kind)
                       (string-append #$output "/share/firmware/ovmf_vars_x64.bin")))
                    ))))))))))
