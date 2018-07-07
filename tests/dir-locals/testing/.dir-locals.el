((nil
  . ((something . t)
     (eval . something)
     ; Expected to be overwritten by scheme section
     (eval . (put 'dir-locals-scheme 'scheme-indent-function 1))
     (eval . (put 'dir-locals-nil 'scheme-indent-function 1))))
 (scheme-mode
  .
  ((something . f)
   (eval . (put 'dir-locals-scheme 'scheme-indent-function 2))
   (something-else . t))))
