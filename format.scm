(add-to-load-path (dirname (current-filename)))
(use-modules (language-server guile extended-scheme)
             (language-server guile formatter)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 ftw)
             (ice-9 textual-ports))

(define (format-file root file)
  (format #t "Formatting `~a'~%" file)
  (let* ((content (call-with-input-file file get-string-all))
         (escm-list (string->escm-list content))
         (formatted (escm-list->indented-string
                     escm-list
                     #:emacs-dir-locals-path root)))
    (call-with-output-file file (cut display formatted <>))))

(define (format-directory root dir)
  (file-system-fold
   (lambda (path stat acc) #t)
   (lambda (path stat acc) (if (string-suffix? ".scm" path)
                             (format-file root path)))
   (lambda (path stat acc) #f)
   (lambda (path stat acc) #f)
   (lambda (path stat acc) #f)
   (lambda (path stat errno acc) #f)
   #f
   dir))


(match (program-arguments)
  ((program dir)
   (format-directory dir dir))
  ((program dir root)
   (format-directory root dir))
  (program
   (format #t "Usage: ~a <dir/file> [root]~%" program)))
