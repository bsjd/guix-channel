(define-module (bsjd packages geo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages image)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages perl))

(define-public postgis-3.5.3
  (package
    (name "postgis")
    (version "3.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.osgeo.org/postgis/source/postgis-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0gl9d6xy2an82ldb9sixz5blyngjryq8m3509fr38ffawvfniazv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       (list (string-append "datadir=" (assoc-ref %outputs "out") "/share")
             (string-append "docdir="(assoc-ref %outputs "out") "/share/doc")
             (string-append "pkglibdir="(assoc-ref %outputs "out") "/lib")
             (string-append "bindir=" (assoc-ref %outputs "out") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-install-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("raster/loader/Makefile" "raster/scripts/python/Makefile")
               (("\\$\\(DESTDIR\\)\\$\\(PGSQL_BINDIR\\)")
                (string-append (assoc-ref outputs "out") "/bin"))))))))
    (inputs
     (list gdal
           geos
           giflib
           json-c
           libjpeg-turbo
           libxml2
           openssl
           pcre
           postgresql
           protobuf-c
           proj))
    (native-inputs
     (list perl pkg-config))
    (home-page "https://postgis.net")
    (synopsis "Spatial database extender for PostgreSQL")
    (description "PostGIS is a spatial database extender for PostgreSQL
object-relational database.  It adds support for geographic objects allowing
location queries to be run in SQL.  This package provides a PostgreSQL
extension.")
    (license (list
               ;; General license
               license:gpl2+
               ;; loader/dbfopen, safileio.*, shapefil.h, shpopen.c
               license:expat
               ;; loader/getopt.*
               license:public-domain
               ;; doc/xsl
               license:bsd-3 ; files only say "BSD"
               ;; doc
               license:cc-by-sa3.0))))
