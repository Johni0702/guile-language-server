(use-modules
 (guix packages)
 ((guix licenses) #:prefix license:)
 (guix download)
 (guix git-download)
 (guix gexp)
 (guix utils)
 (guix build-system gnu)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages pkg-config)
 (gnu packages texinfo))

(define %source-dir (getcwd))

(package
  (name "guile-language-server")
  (version "0.1")
  (source (local-file %source-dir
                      #:recursive? #t
                      #:select? (git-predicate %source-dir)))
  (build-system gnu-build-system)
  (arguments
   `(#:modules
     ((ice-9 match)
      (ice-9 ftw)
      ,@%gnu-build-system-modules)
     #:phases
     (modify-phases
         %standard-phases
       (replace 'check
         (lambda* (#:key tests? #:allow-other-keys)
           (when tests?
             (invoke "guile" "-s" "test.scm"))
           #t))
       (add-after
           'install
           'hall-wrap-binaries
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let* ((compiled-dir
                   (lambda (out version)
                     (string-append
                      out
                      "/lib/guile/"
                      version
                      "/site-ccache")))
                  (uncompiled-dir
                   (lambda (out version)
                     (string-append
                      out
                      "/share/guile/site"
                      (if (string-null? version) "" "/")
                      version)))
                  (dep-path
                   (lambda (env modules path)
                     (list env
                           ":"
                           'prefix
                           (cons modules
                                 (map (lambda (input)
                                        (string-append
                                         (assoc-ref inputs input)
                                         path))
                                      ,''("guile-json"))))))
                  (out (assoc-ref outputs "out"))
                  (bin (string-append out "/bin/"))
                  (site (uncompiled-dir out "")))
             (match (scandir site)
               (("." ".." version)
                (for-each
                 (lambda (file)
                   (wrap-program
                       (string-append bin file)
                     `("PATH" ":" prefix (,(string-append (assoc-ref inputs "guile") "/bin")))
                     (dep-path
                      "GUILE_LOAD_PATH"
                      (uncompiled-dir out version)
                      (uncompiled-dir "" version))
                     (dep-path
                      "GUILE_LOAD_COMPILED_PATH"
                      (compiled-dir out version)
                      (compiled-dir "" version))))
                 ,''("guile-ls"))
                #t))))))))
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("pkg-config" ,pkg-config)
     ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-2.2)))
  (propagated-inputs
   `(("guile-json" ,guile-json-1)))
  (synopsis "LSP server for GNU Guile")
  (description "")
  (home-page "")
  (license license:gpl3+))

