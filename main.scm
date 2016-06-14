;;; main.scm    source of mecab.scm
(use lolevel foreigners)
(foreign-declare "#include \"mecab.h\"")
(foreign-declare "#include <stdio.h>")
(define-record mecab ptr)
(define-foreign-type mecab* c-pointer
  mecab-ptr
  make-mecab)

(define-foreign-type mecab-path* c-pointer)
(define-foreign-type mecab-node* c-pointer)
(define-foreign-type mecab-dinfo* c-pointer)

;;; mecab_dictionary_info_t
(define-foreign-record-type (mecab-dinfo mecab_dictionary_info_t)
  [c-string	filename	dinfo-filename]
  [c-string	charset		dinfo-charset]
  [unsigned-int	size		dinfo-size]
  [int		type		dinfo-type]
  [unsigned-int	lsize		dinfo-lsize]
  [unsigned-int	rsize		dinfo-rsize]
  [unsigned-short	version	dinfo-version]
  [mecab-dinfo*	next		dinfo-next])

;;; mecab_path_t
(define-foreign-record-type (mecab-path mecab_path_t)
  [mecab-node* rnode path-rnode]
  [mecab-path* rnext path-rnext]
  [mecab-node* lnode path-lnode]
  [mecab-path* lnext path-lnext]
  [int	       cost  path-cost]
  [float       prob  path-prob])
;;; mecab_node_t
(define-foreign-record-type (mecab-node mecab_node_t)
  [mecab-node*	prev	node-prev]
  [mecab-node*	next	node-next]
  [mecab-node*	enext	node-enext]
  [mecab-node*	bnext	node-bnext]
  [mecab-path*	rpath	node-rpath]
  [mecab-path*  lpath	node-lpath]
  [(const c-string)	surface _node-surface]
  [(const c-string)	feature	node-feature]
  [unsigned-int	id  node-id]
  [unsigned-short	length node-length]
  [unsigned-short	rlength node-rlength]
  [unsigned-short	rcAttr	node-rc-attr]
  [unsigned-short	lcAttr	node-lc-attr]
  [unsigned-short	posid	node-posid]
  [unsigned-char	char_type	node-char-type]
  [unsigned-short	stat	node-stat]
  [unsigned-char	isbest	node-isbest]
  [float	alpha	node-alpha]
  [float	beta	node-beta]
  [float	prob	node-prob]
  [short	wcost	node-wcost]
  [long		cost	node-cost])

(define (node-surface node)
  (string-take (_node-surface node) (node-length node)))

(define-foreign-enum-type (node-stat int)
  (node-stat->int int->node-stat)
  [mecab-nor-node MECAB_NOR_NODE 0]
  [mecab-unk-node MECAB_UNK_NODE 1]
  [mecab-bos-node MECAB_BOS_NODE 2]
  [mecab-eos-node MECAB_EOS_NODE 3]
  [mecab-eon-node MECAB_EON_NODE 4])

(define (mecab-new #!optional (args ""))
  (let ([mecab ((foreign-lambda mecab* mecab_new2 (const c-string))
		args)])
    (mecab-check mecab #t #:loc 'mecab-new)
    (set-finalizer! mecab gc-collect-mecab)))

;; (define (gc-collect-mecab mecab)
;;   (printf "gc: ~A~%" (mecab-ptr mecab))
;;   ((foreign-lambda void mecab_destroy mecab*) mecab))

(define (gc-collect-mecab mecab)
  (when (mecab-ptr mecab)
    ((foreign-lambda void mecab_destroy mecab*) mecab)
    (mecab-ptr-set! mecab #f)))

(define (mecab-error mecab loc message)
  (error loc "MECAB ERROR" message))

(define (mecab-success? mecab)
  ((foreign-lambda* bool ([mecab* mecab])
		    "C_return(! ! mecab);")
   mecab))

(define (mecab-check mecab value #!key
		     (message "")
		     (loc #f))
  (if (and (mecab-success? mecab) value)
      value
      (mecab-error mecab loc message)))

;;; dictionary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; parse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mecab-sparse->string mecab str)
  (let ([result ((foreign-lambda c-string mecab_sparse_tostr mecab* (const c-string))
		 mecab str)])
    (mecab-check mecab result
		 #:loc 'mecab-sparse->string)))


(define (mecab-sparse->node mecab str)
  (let ([result ((foreign-lambda mecab-node* mecab_sparse_tonode mecab* (const c-string))
		 mecab str)])
    (mecab-check mecab result
		 #:loc 'mecab-sparse->node)))

(define (mecab-parse mecab str #!optional (type 'node))
  (case type
    [(node) (mecab-sparse->node mecab str)]
    [else (mecab-sparse->string mecab str)]))

;;; buggy…
(define (mecab-nbest-init! mecab str)
  (let ([result ((foreign-lambda bool mecab_nbest_init mecab* (const c-string))
		 mecab str)])
    (mecab-check mecab result
		 #:loc 'mecab-nbest-init)))
(define (mecab-nbest-next->node mecab)
  (let ([result ((foreign-lambda mecab-node* mecab_nbest_next_tonode mecab*)
		 mecab)])
    (mecab-check mecab result
		 #:loc 'mecab-nbest-next->node)))
(define (mecab-nbest-next->string mecab)
  (let ([result ((foreign-lambda c-string mecab_nbest_next_tostr mecab*)
		 mecab)])
    (mecab-check mecab result
		 #:loc 'mecab-nbest-next->string)))

(define (mecab-nbest-next! mecab #!optional (type 'node))
  (case type
    [(node) (mecab-nbest-next->node mecab)]
    [else (mecab-nbest-next->string mecab)]))

(define (node->list node)
  (define (inner node acc)
    (if node
	(inner (node-next node) (cons node acc))
	(reverse! acc)))
  (inner node '()))
