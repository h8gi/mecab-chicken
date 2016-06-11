;;; main.scm    source of mecab.scm
(use lolevel)
(foreign-declare "#include \"mecab.h\"")
(foreign-declare "#include <stdio.h>")
(define-record mecab ptr)
(define-foreign-type *mecab c-pointer)
(define-foreign-type *mecab-node c-pointer)

(define (mecab-new)
  (let* ([mcb-ptr ((foreign-lambda *mecab mecab_new2 c-string)
		   (foreign-value "\"\"" c-string))]
	 [mcb (make-mecab mcb-ptr)])
    (mecab-check mcb)
    (set-finalizer! mcb gc-collect-mecab)))

(define (gc-collect-mecab mcb)
  (when (mecab-ptr mcb)
    ((foreign-lambda void mecab_destroy *mecab) (mecab-ptr mcb))
    (mecab-ptr-set! mcb #f)))

(define (mecab-error mcb #!optional loc)
  (gc-collect-mecab mcb)
  (error loc "Mecab error"
	 ((foreign-lambda c-string mecab_strerror *mecab) (mecab-ptr mcb))))

(define (mecab-success? mcb)
  ((foreign-lambda* bool ([*mecab mcb])
		    "return (! ! mcb);")
   (mecab-ptr mcb)))

(define (mecab-check mcb #!optional (value #t))
  (unless (and (mecab-success? mcb) value)
    (mecab-error mcb 'check)))

(define (mecab-sparse->string mcb str)
  (mecab-check mcb)
  ((foreign-lambda c-string mecab_sparse_tostr *mecab (const c-string))
   (mecab-ptr mcb) str))


;; (define (mecab-sparse->node mcb str)
;;   (mecab-check mcb)
;;   ((foreign-lambda *mecab-node mecab_sparse_tonode *mecab (const c-string))
;;       (mecab-ptr mcb) str))

(let* ([mecab (mecab-new)]
       [input (foreign-value "\"太郎は次郎が持っている本を花子に渡した。\"" (const c-string))])
  (printf "INPUT: ~A~%" input)
  (printf "OUTPUT: ~%~A~%" (mecab-sparse->string mecab input))
  (mecab-check mecab))

