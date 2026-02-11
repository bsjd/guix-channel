(define-module (bsjd packages firmware)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix memoization)
  #:use-module (guix deprecation)
  #:use-module (guix packages)
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
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
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
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-26))

(define-public edk2-tools
  (package
    (name "edk2-tools")
    (version "202402")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tianocore/edk2")
                    (commit (string-append "edk2-stable" version))
                    ;; EDK2 makes extensive use of submodules.
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y7jfpijgi099znhzjklnsczn0k0vm1d1qznq9x2a2sa0glydsin"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "BUILD_CC=" #$(cc-for-target))
                   (string-append "CC=" #$(cc-for-target)))
           #:test-target "Tests"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'change-directory
                 (lambda _
                   (chdir "BaseTools")))
               (replace 'build
                 (lambda* (#:key (make-flags #~'()) #:allow-other-keys)
                   ;; The default build target also runs tests.
                   (apply invoke "make" "-C" "Source/C" make-flags)))
               (delete 'configure)
               (replace 'install
                 (lambda _
                   (mkdir #$output)
                   (copy-recursively "Source/C/bin"
                                     (string-append #$output "/bin")))))))
    (native-inputs
     (list python-wrapper))
    (inputs
     (list `(,util-linux "lib")))       ;for libuuid
    (home-page
     "https://github.com/tianocore/tianocore.github.io/wiki/EDK-II-Tools-List")
    (synopsis "EFI development tools")
    (description
     "This package contains tools for processing UEFI firmware content.
Executables included are:

@itemize
@item @code{EfiRom}: Build Option ROM images.
@item @code{GenFfs}: Generate FFS files.
@item @code{GenFv}: Generate a PI firmware volume image.
@item @code{GenFw}: Get image data from PE32 files.
@item @code{GenSec}: Generate EFI_SECTION type files.
@item @code{VfrCompile}: Parse preprocessed UEFI and Framework VFR files.
@item @code{VolInfo}: Display the contents of a firmware volume.
@end itemize")
    ;; See BaseTools/Source/C/GNUmakefile
    (supported-systems '("x86_64-linux" "i686-linux" "armhf-linux"
                         "aarch64-linux" "riscv64-linux"))
    (license license:bsd-2)))

(define* (make-ovmf-firmware arch)
  (let ((toolchain "GCC")
        (arch-string (match arch
                           ("x86_64" "X64")
                           ("i686" "IA32")
                           ("aarch64" "AARCH64")
                           ("armhf" "ARM")
                           ("riscv64" "RISCV64")
                           ("loongarch64" "LOONGARCH64")
                           (_ "NONE"))))
    (package
      (inherit edk2-tools)
      (name (string-append "ovmf-" arch))
      (arguments
       (list
        #:tests? #f                     ; No check target.
        #:target #f                     ; Package produces firmware.
        #:modules '((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 match))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-source
              (lambda _
                (substitute* "edksetup.sh"
                  (("^return \\$\\?")
                   "exit $?"))))
            (add-before 'configure 'set-env
              (lambda _
                (unless (string-prefix? #$arch #$(%current-system))
                  (setenv (string-append #$toolchain "_X64_PREFIX")
                          "x86_64-linux-gnu-")
                  (setenv (string-append #$toolchain "_IA32_PREFIX")
                          "i686-linux-gnu-")
                  (setenv (string-append #$toolchain "_AARCH64_PREFIX")
                          "aarch64-linux-gnu-")
                  (setenv (string-append #$toolchain "_ARM_PREFIX")
                          "arm-linux-gnueabihf-")
                  (setenv (string-append #$toolchain "_RISCV64_PREFIX")
                          "riscv64-linux-gnu-")
                  (setenv (string-append #$toolchain "_LOONGARCH64_PREFIX")
                          "loongarch64-linux-gnu-"))))
            (replace 'configure
              (lambda _
                (let* ((cwd (getcwd))
                       (tools (string-append cwd "/BaseTools"))
                       (bin (string-append tools "/BinWrappers/PosixLike")))
                  (setenv "WORKSPACE" cwd)
                  (setenv "EDK_TOOLS_PATH" tools)
                  (setenv "PYTHON3_ENABLE" "TRUE")
                  (setenv "PYTHON_COMMAND" "python3")
                  (setenv "PATH" (string-append (getenv "PATH") ":" bin))
                  (invoke "bash" "edksetup.sh")
                  (substitute* "Conf/target.txt"
                    (("^TARGET[ ]*=.*$") "TARGET = RELEASE\n")
                    (("^TOOL_CHAIN_TAG[ ]*=.*$")
                     (string-append "TOOL_CHAIN_TAG = " #$toolchain "\n"))
                    (("^TARGET_ARCH[ ]*=.*$")
                     (string-append "TARGET_ARCH = " #$arch-string
                                    "\n"))
                    (("^MAX_CONCURRENT_THREAD_NUMBER[ ]*=.*$")
                     (format #f "MAX_CONCURRENT_THREAD_NUMBER = ~a~%"
                             (number->string (parallel-job-count)))))
                  ;; Build build support.
                  (setenv "CC" "gcc")
                  (invoke "make" "-C" tools))))
            (replace 'build
              (lambda _
                (invoke "build" "-a" #$arch-string "-t" #$toolchain "-p"
                        (match #$arch
                               ("x86_64"
                                "OvmfPkg/OvmfPkgX64.dsc")
                               ("i686"
                                "OvmfPkg/OvmfPkgIa32.dsc")
                               ((or "aarch64" "armhf")
                                "ArmVirtPkg/ArmVirtQemu.dsc")
                               ("riscv64"
                                "OvmfPkg/RiscVVirt/RiscVVirtQemu.dsc")
                               (_ #t)))))
            (add-before 'install 'install-efi-shell
              (lambda _
                (let ((fmw (string-append #$output "/share/firmware")))
                  (mkdir-p fmw)
                  (for-each
                    (lambda (file)
                      (copy-file file
                                 (string-append fmw "/Shell_"
                                                (string-downcase #$arch-string)
                                                ".efi")))
                    (find-files "Build" "Shell\\.efi"))))))))
      (native-inputs
       (append
         (list acpica
               nasm
               perl
               python-3
               (list util-linux "lib"))
         (if (not (string-prefix? arch (%current-system)))
             (if (string=? arch "armhf")
                 (list (cross-gcc "arm-linux-gnueabihf")
                       (cross-binutils "arm-linux-gnueabihf"))
                 (list (cross-gcc (string-append arch "-linux-gnu"))
                       (cross-binutils (string-append arch "-linux-gnu"))))
             '())))
      (synopsis "UEFI firmware for QEMU")
      (description "OVMF is an EDK II based project to enable UEFI support for
Virtual Machines.  OVMF contains a sample UEFI firmware for QEMU and KVM.")
      (license (list license:expat
                     license:bsd-2 license:bsd-3 license:bsd-4)))))

(define %ovmf-files-path
  (make-parameter
   (map (cut string-append <> "/bsjd/packages/ovmf/")
        %load-path)))

(define (ovmf-local-file name)
  "Return as a gexp the auxiliary OVMF file corresponding to NAME."
  (local-file (search-path (%ovmf-files-path) name)))

(define-public edk2-ovmf-x86-64
  (let ((base (make-ovmf-firmware "x86_64")))
    (package
      (inherit base)
      (name "edk2-ovmf-x86-64")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (replace 'install
                (lambda _
                  (let ((fmw (string-append #$output "/share/firmware")))
                    (mkdir-p fmw)

                    (copy-file "Build/OvmfX64/RELEASE_GCC/FV/OVMF_CODE.fd"      (string-append fmw "/ovmf_code_x64.fd"))
                    (copy-file "Build/OvmfX64/RELEASE_GCC/FV/OVMF_VARS.fd"      (string-append fmw "/ovmf_vars_x64.fd"))

                    (copy-file #$(ovmf-local-file "edk2-x86_64-code.fd")        (string-append fmw "/edk2_ovmf_code_x64.fd"))
                    (copy-file #$(ovmf-local-file "edk2-i386-vars.fd")          (string-append fmw "/edk2_ovmf_vars_x64.fd"))

                    (copy-file #$(ovmf-local-file "edk2-x86_64-secure-code.fd") (string-append fmw "/edk2_ovmf_code_x64.secboot.fd"))
                    (copy-file #$(ovmf-local-file "edk2-i386-vars.fd")          (string-append fmw "/edk2_ovmf_vars_x64.secboot.fd")))))
              (add-after 'install 'install-qemu-firmware-metadata
                (lambda _
                  ;; The QEMU firmware metadata files are taken from the
                  ;; Fedora project (see:
                  ;; https://src.fedoraproject.org/rpms/edk2/tree/rawhide).
                  (let ((31-edk2-ovmf-2m-raw-x64-sb-enrolled.json-source #$(ovmf-local-file "31-edk2-ovmf-2m-raw-x64-sb-enrolled.json"))
                        (31-edk2-ovmf-2m-raw-x64-sb-enrolled.json-dest (string-append #$output "/share/qemu/firmware/" "31-edk2-ovmf-2m-raw-x64-sb-enrolled.json"))
                        (41-edk2-ovmf-2m-raw-x64-sb.json-source #$(ovmf-local-file "41-edk2-ovmf-2m-raw-x64-sb.json"))
                        (41-edk2-ovmf-2m-raw-x64-sb.json-dest (string-append #$output "/share/qemu/firmware/41-edk2-ovmf-2m-raw-x64-sb.json"))
                        (51-edk2-ovmf-2m-raw-x64-nosb.json-source #$(ovmf-local-file "51-edk2-ovmf-2m-raw-x64-nosb.json"))
                        (51-edk2-ovmf-2m-raw-x64-nosb.json-dest (string-append #$output "/share/qemu/firmware/51-edk2-ovmf-2m-raw-x64-nosb.json")))
                    (mkdir-p (dirname 31-edk2-ovmf-2m-raw-x64-sb-enrolled.json-dest))
                    (mkdir-p (dirname 41-edk2-ovmf-2m-raw-x64-sb.json-dest))
                    (mkdir-p (dirname 51-edk2-ovmf-2m-raw-x64-nosb.json-dest))
                    (copy-file 31-edk2-ovmf-2m-raw-x64-sb-enrolled.json-source
                               31-edk2-ovmf-2m-raw-x64-sb-enrolled.json-dest)
                    (copy-file 41-edk2-ovmf-2m-raw-x64-sb.json-source
                               41-edk2-ovmf-2m-raw-x64-sb.json-dest)
                    (copy-file 51-edk2-ovmf-2m-raw-x64-nosb.json-source
                               51-edk2-ovmf-2m-raw-x64-nosb.json-dest)
                    (substitute* 31-edk2-ovmf-2m-raw-x64-sb-enrolled.json-dest
                      (("/usr/share/edk2/ovmf/OVMF_(CODE|VARS).secboot.fd" _ kind)
                       (string-append #$output "/share/firmware/edk2_ovmf_" (string-downcase kind) "_x64.secboot.fd")))
                    (substitute* 41-edk2-ovmf-2m-raw-x64-sb.json-dest
                      (("/usr/share/edk2/ovmf/OVMF_(CODE|VARS).secboot.fd" _ kind)
                       (string-append #$output "/share/firmware/edk2_ovmf_" (string-downcase kind) "_x64.secboot.fd")))
                    (substitute* 51-edk2-ovmf-2m-raw-x64-nosb.json-dest
                      (("/usr/share/edk2/ovmf/OVMF_(CODE|VARS).fd" _ kind)
                       (string-append #$output "/share/firmware/edk2_ovmf_" (string-downcase kind) "_x64.fd")))
                    )))
              )))))))
