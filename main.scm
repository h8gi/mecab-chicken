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

(define-foreign-record-type (mecab-path mecab_path_t)
  [mecab-node* rnode path-rnode]
  [mecab-path* rnext path-rnext]
  [mecab-node* lnode path-lnode]
  [mecab-path* lnext path-lnext]
  [int	       cost  path-cost]
  [float       prob  path-prob])

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

(define (mecab-new)
  (let* ([mcb ((foreign-lambda mecab* mecab_new2 c-string)
	       (foreign-value "\"\"" c-string))])
    (mecab-check mcb)
    (set-finalizer! mcb gc-collect-mecab)))

(define (gc-collect-mecab mcb)
  (when (mecab-ptr mcb)
    ((foreign-lambda void mecab_destroy mecab*) mcb)
    (mecab-ptr-set! mcb #f)))

(define (mecab-error mcb #!optional loc)
  (error loc "Mecab error"
	 ((foreign-lambda c-string mecab_strerror mecab*) mcb)))

(define (mecab-success? mcb)
  ((foreign-lambda* bool ([mecab* mcb])
		    "return (! ! mcb);")
   mcb))

(define (mecab-check mcb #!optional (value #t))
  (unless (and (mecab-success? mcb) value)
    (mecab-error mcb 'check)))

(define (mecab-sparse->string mcb str)
  (mecab-check mcb)
  ((foreign-lambda c-string mecab_sparse_tostr mecab* (const c-string))
   mcb str))


(define (mecab-sparse->node mcb str)
  (mecab-check mcb)
  ((foreign-lambda mecab-node* mecab_sparse_tonode mecab* (const c-string))
   mcb str))

(define (node->list node)
  (if (or (= (node-stat node) mecab-nor-node)
	  (= (node-stat node) mecab-unk-node)
	  (= (node-stat node) mecab-bos-node))
      (cons (cons (node-surface node) (node-feature node))
	    (node->list (node-next node)))
      (cons (node-surface node) (node-feature node))))

