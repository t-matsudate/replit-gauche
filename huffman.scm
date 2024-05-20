(define-module huffman
  (export take-while
          drop-while
          make-frequencies
          make-leaf
          leaf?
          symbol-leaf
          weight-leaf
          make-code-tree
          left-branch
          right-branch
          symbols
          weight
          adjoin-set
          make-leaf-set))
(select-module huffman)

; Huffman 圧縮の材料となる度数分布を生成する手続き群。
; take-while: 任意のリストの先頭から条件に合うものを抽出する。
; drop-while: 任意のリストの先頭から条件に合うものを切り捨てる。
; make-frequencies: 任意のリストから度数分布を生成する。

(define (take-while pred sequence)
  (if (null? sequence)
      '()
      (let ((item (car sequence)))
        (if (pred item)
            (cons item (take-while pred (cdr sequence)))
            '()))))

(define (drop-while pred sequence)
  (if (null? sequence)
      '()
      (let ((item (car sequence)))
        (if (pred item)
            (drop-while pred (cdr sequence))
            sequence))))

; NOTE: 文字列は文字のリストに変換しておくこと。
(define (make-frequencies sequence)
  (define order-by-asc (lambda (x y) (= -1 (compare x y))))
  (define order-by-desc (lambda (x y) (= 1 (compare x y))))
  (define (seq->freq ret sequence)
    (if (null? sequence)
        ret
        (let* ((item (car sequence))
               (frequency (list item (length (take-while (lambda (x) (equal? item x)) sequence)))))
          (seq->freq (cons frequency ret)
                     (drop-while (lambda (x) (equal? item x)) sequence)))))
  (seq->freq '() (sort sequence order-by-asc)))

; 度数分布から Huffman 木を生成する手続き群。
; make-leaf: 度数分布の要素を Huffman 木の葉に変換する。
; leaf?: 与えられたオブジェクトが Huffman 木における葉であるかどうかを判断する。
; symbol-leaf: Huffman 木の葉から記号を取り出す。
; weight-leaf: Huffman 木の葉から記号の重みを取り出す。
; make-code-tree: 2 つの度数から Huffman 木を生成する。
; left-branch: Huffman 木の左の枝を取り出す。
; right-branch: Huffman 木の右の枝を取り出す。
; symbols: Huffman 木または葉から記号を取り出す。葉の場合は単一要素のリストとして返す。
; weight: Huffman 木または葉から重みを取り出す。葉の場合は単一要素のリストとして返す。
; adjoin-set: 集合に対して、度数を昇順に整列された状態で追加する。
; make-leaf-set: 度数分布を Huffman 木における葉に変換する。

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

; (car object) == (list-ref object 0)
(define (leaf? object)
  (eq? (car object) 'leaf))

; (cadr x) == (list-ref x 1)
(define (symbol-leaf x) (cadr x))

; (caddr x) == (list-ref x 2)
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

; (cadddr tree) == (list-ref tree 3)
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; 記号
                               (cadr pair))  ; 頻度
                    (make-leaf-set (cdr pairs))))))