;;; main.scm    source of mecab.scm
(use lolevel foreigners extras srfi-13)
(foreign-declare "#include \"mecab.h\"")
(foreign-declare "#include <stdio.h>")
(define-record mecab ptr)
(define-foreign-type mecab* c-pointer
  mecab-ptr
  make-mecab)

(define-foreign-type mecab-path* c-pointer)
(define-foreign-type mecab-node* c-pointer)
(define-foreign-type mecab-dinfo* c-pointer)
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
  [c-string	surface _node-surface]
  [c-string	feature	node-feature]
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
  (let* ([mecab ((foreign-lambda mecab* mecab_new2 c-string)
	       args)])
    (mecab-check mecab)
    (set-finalizer! mecab gc-collect-mecab)))

(define (gc-collect-mecab mecab)
  (when (mecab-ptr mecab)
    ((foreign-lambda void mecab_destroy mecab*) mecab)
    (mecab-ptr-set! mecab #f)))

(define (mecab-error mecab #!optional loc)
  (error loc "Mecab error"
	 ((foreign-lambda c-string mecab_strerror mecab*) mecab)))

(define (mecab-success? mecab)
  ((foreign-lambda* bool ([mecab* mecab])
		    "return (! ! mecab);")
   mecab))

(define (mecab-check mecab #!optional (value #t))
  (unless (and (mecab-success? mecab) value)
    (mecab-error mecab 'check)))

;;; dictionary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (mecab-dictionary-info mecab)
;;   ())


;;; parse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mecab-sparse->string mecab str)
  (let ([result ((foreign-lambda c-string mecab_sparse_tostr mecab* (const c-string))
		 mecab str)])
    result))


(define (mecab-sparse->node mecab str)
  (let ([result ((foreign-lambda mecab-node* mecab_sparse_tonode mecab* (const c-string))
		 mecab str)])
    result))

(define (mecab-nbest-init mecab str)
  (let ([result ((foreign-lambda bool mecab_nbest_init mecab* (const c-string))
		 mecab str)])
    (assert result "failed parse nbest init")))

(define (mecab-nbest-next->node mecab)
  (let ([result ((foreign-lambda mecab-node* mecab_nbest_next_tonode mecab*)
		 mecab)])
    result))

(define (mecab-nbest-next->string mecab)
  (let ([result ((foreign-lambda (const c-string) mecab_nbest_next_tostr mecab*)
		 mecab)])
    result))

(define (node->list node)
  (if (or (= (node-stat node) mecab-nor-node)
	  (= (node-stat node) mecab-unk-node)
	  (= (node-stat node) mecab-bos-node))
      (cons (cons (node-surface node) (node-feature node))
	    (node->list (node-next node)))
      (cons (node-surface node) (node-feature node))))

(let* ([mecab (mecab-new)]
       [input "ご飯を食べたら体調が良くなった。"]
       [node (mecab-sparse->node mecab input)])
  (printf "INPUT: ~A~%" input)
  (display (mecab-sparse->string mecab input))
  (pp (node->list node))
  (mecab-check mecab)
  (mecab-nbest-init mecab "ここには何もないし、私はご飯をあなたと食べなかったりする。")
  (pp (node->list (mecab-nbest-next->node mecab)))
  (pp (node->list (mecab-nbest-next->node mecab))))
