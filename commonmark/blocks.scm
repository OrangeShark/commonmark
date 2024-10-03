;; Copyright (C) 2015, 2016  Erik Edrosa <erik.edrosa@gmail.com>
;;
;; This file is part of guile-commonmark
;;
;; guile-commonmark is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; guile-commonmark is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with guile-commonmark.  If not, see <http://www.gnu.org/licenses/>.

(define-module (commonmark blocks)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (commonmark node)
  #:use-module (commonmark utils)
  #:use-module (commonmark parser)
  #:use-module (commonmark common)
  #:export (parse-blocks))


;; Port -> Document
(define (parse-blocks port)
  "Parses CommonMark blocks from PORT returning a CommonMark Document tree"
  (let loop ((root (make-document-node))
             (line (read-line-without-nul port)))
    (if (eof-object? line)
        (parse-clean-up root (lambda (doc references)
                               (if (null? references)
                                   doc
                                   (node-add-data doc 'link-references references))))
        (loop (parse-open-block root (make-parser line))
              (read-line-without-nul port)))))

;; Node Parser -> Node
(define (parse-open-block node parser)
  (cond ((node-closed? node) node)
        ((document-node? node) (parse-container-block node parser))
        ((block-quote-node? node) (parse-block-quote node parser))
        ((code-block-node? node) (parse-code-block node parser))
        ((fenced-code-node? node) (parse-fenced-code node parser))
        ((list-node? node) (parse-list node parser))
        ((html-block-node? node) (parse-html node parser))
        ((paragraph-node? node) (parse-paragraph node parser))))

(define (parse-html node parser)
  (let ((type (html-block-node-type node))
        (str (parser-rest-str parser)))
    (match (node-children node)
      ((html)
       (if (html-block-end type parser)
           (close-node
            (if (string-null? str)
                node
                (make-html-block-node type (string-append html "\n" str))))
           (make-html-block-node type (string-append html "\n" str)))))))

;; Node Parser -> Node
(define (parse-container-block node parser)
  (cond ((and (no-children? node) (empty-line parser)) ;; empty line
         node)
        ((no-children? node)                            ;; first line
         (add-child-node node (parse-line parser)))
        ((and (node-closed? (last-child node)) (not (empty-line parser))) ;; new block
         (add-child-node node (parse-line parser)))
        (else (let ((new-child (parse-open-block (last-child node) parser)))
                (cond ((and (not (empty-line parser))
                            (node-closed? new-child)
                            (not (fenced-code-node? new-child))
                            (not (heading-node? new-child))
                            (not (html-block-node? new-child)))
                       (add-child-node (replace-last-child node new-child)
                                       (parse-line parser)))
                      (else (replace-last-child node new-child)))))))

(define (parse-block-quote node parser)
  (cond ((block-quote (parser-advance-min-spaces parser 3)) => (lambda (rest)
                               (parse-container-block node (block-quote-rest rest))))
        ((open-descendant? node 'paragraph) ;; lazy continuation line
         (let ((parsed-line (parse-line parser)))
           (if (or (paragraph-node? parsed-line) (code-block-node? parsed-line))
               (parse-container-block node parser)
               (close-node node))))
        (else (close-node node))))

(define (parse-code-block node parser)
  (let ((nonspace-parser (parser-advance-next-nonspace parser)))
    (cond ((parser-indented? parser nonspace-parser)
           (add-child-node node (parser-rest-str (parser-advance parser code-indent))))
          ((empty-line parser) (add-child-node node ""))
          (else (close-node node)))))

(define (parse-paragraph node parser)
  (let ((parsed-line (parse-line parser)))
    (cond ((blank-node? parsed-line)
           (close-node node))
          ((and (setext-heading (parser-advance-min-spaces parser 3)) (= (length (node-children (last-child node))) 1))
           (make-heading-node (last-child (last-child node))
                              (if (string-any #\= (parser-rest-str parser)) 1 2)))
          ((paragraph-node? parsed-line)
           (replace-last-child node (join-text-nodes (last-child node) (last-child parsed-line))))
          ((code-block-node? parsed-line)
           (replace-last-child node (add-text (last-child node) (parser-rest-str parser))))
          ;; 1 of 7 types of HTML blocks is *not* allowed to interrupt
          ;; a paragraph.
          ((and (html-block-node? parsed-line)
                (eq? (html-block-node-type parsed-line) 'other))
           (replace-last-child node (add-text (last-child node) (last-child parsed-line))))
          (else (close-node node)))))

(define (fence-start node)
  (node-get-data node 'fence-start))

(define (fence-type node)
  (node-get-data node 'fence))

(define (parse-fenced-code node parser)
  (cond ((fenced-code-end (parser-advance-min-spaces parser 3) (fence-type node))
         (close-node node))
        ((no-children? node)
         (add-child-node node (parser-rest-str (parser-advance-min-spaces parser (fence-start node)))))
        (else (replace-last-child
               node
              (string-append (last-child node)
                             "\n"
                             (parser-rest-str (parser-advance-min-spaces parser (fence-start node))))))))

(define (list-type node)
  (node-get-data node 'type))

(define (list-bullet node)
  (node-get-data node 'bullet))

(define (list-delimiter node)
  (node-get-data node 'delimiter))

(define (eq-list-types? l1 l2)
  (or (and (eq? (list-type l1) (list-type l2) 'bullet)
           (string=? (list-bullet l1) (list-bullet l2)))
      (and (eq? (list-type l1) (list-type l2) 'ordered)
           (string=? (list-delimiter l1) (list-delimiter l2)))))

(define (parse-list node parser)
  (define (remove-blank-node item)
    (if (blank-last-child? item)
        (remove-last-child item)
        item))

  (define (parse-new-item node parser item)
    (let ((new-list (parse-line parser)))
      (cond ((and (list-node? new-list)
                  (eq-list-types? node new-list))
             (add-child-node (replace-last-child node item)
                             (last-child new-list)))
            (else (close-node (replace-last-child node (remove-blank-node item)))))))

  (if (node-closed? (last-child node))
      (parse-new-item node parser (last-child node))
      (let ((item (parse-item (last-child node) parser)))
        (cond ((and (node-closed? item) (empty-line parser))
               (close-node (replace-last-child node item)))
              ((node-closed? item)
               (parse-new-item node parser item))
              (else (replace-last-child node item))))))

(define (n-spaces? n str)
  (>= (string-index str (lambda (c) (not (char=? #\space c)))) n))

(define (item-padding node)
  (node-get-data node 'padding))

(define (blank-last-child? node)
  (and (not (no-children? node)) (blank-node? (last-child node))))

(define (parse-item node parser)
  (let ((padding (item-padding node)))
    (cond ((and (blank-last-child? node) (= (length (node-children node)) 1))
           (close-node node))
          ((and (empty-line parser) (no-children? node))
           (add-child-node node (make-blank-node)))
          ((and (empty-line parser) (blank-node? (last-child node)))
           node) ;; may be separated by more than one blank line
          ((empty-line parser)
           (cond ((open-descendant? node 'fenced-code)
                  (parse-container-block node parser))
                 ((list-node? (last-child node))
                  (let ((container (parse-container-block node parser)))
                    (if (node-closed? (last-child container))
                        (close-node container)
                        (add-child-node container (make-blank-node)))))
                 ((open-descendant? node 'code-block)
                  (add-child-node (parse-container-block node parser)
                                  (make-blank-node)))
                 (else (add-child-node (replace-last-child node (close-node (last-child node)))
                                       (make-blank-node)))))
          ((n-spaces? padding (parser-rest-str parser))
           (let* ((parser (parser-advance parser padding))
                  (n (if (and (blank-last-child? node) (or (prev-node? node 'code-block) (prev-node? node 'list)))
                         (remove-last-child node)
                         node))
                  (new-item (parse-container-block n parser)))
             (if (and (blank-last-child? node) (prev-node? new-item 'list) (prev-node-closed? new-item))
                 (add-child-node (replace-last-child new-item (last-child node)) (last-child new-item))
                 new-item)))
          ((or (no-children? node) (blank-node? (last-child node)))
           (close-node node))
          ((open-descendant? node 'paragraph)
           (let ((parsed-line (parse-line (parser-advance-min-spaces parser padding))))
             (if (paragraph-node? parsed-line)
                 (parse-container-block node parser)
                 (close-node (replace-last-child node (close-node (last-child node)))))))
          (else (close-node (replace-last-child node (close-node (last-child node))))))))


(define (heading-level str)
  (string-length str))

;; Parser -> Node
(define (parse-line parser)
  (let ((nonspace-parser (parser-advance-next-nonspace parser)))
    (cond ((empty-line nonspace-parser)              (make-blank-node))
          ((parser-indented? parser nonspace-parser) (make-code-block parser))
          ((html-block nonspace-parser)           => (match-lambda
                                                       ((type match)
                                                        (make-html-block type parser))))
          ((thematic-break nonspace-parser)          (make-thematic-break))
          ((block-quote nonspace-parser)          => make-block-quote)
          ((atx-heading nonspace-parser)          => make-atx-heading)
          ((fenced-code nonspace-parser)          => make-fenced-code)
          ((bullet-list-marker nonspace-parser)   => (cut make-bullet-list-marker parser <>))
          ((ordered-list-marker nonspace-parser)  => (cut make-ordered-list-marker parser <>))
          (else                                      (make-paragraph nonspace-parser)))))


(define (make-html-block type parser)
  (let ((node (make-html-block-node type (parser-rest-str parser))))
    (if (html-block-end type parser)
        (close-node node)
        node)))

(define (make-thematic-break)
  (make-thematic-break-node))

(define (make-block-quote match)
  (make-block-quote-node (parse-line (block-quote-rest match))))

(define (make-atx-heading match)
  (make-heading-node (atx-heading-content match)
                     (heading-level (atx-heading-opening match))))

(define (make-code-block parser)
  (make-code-block-node (parser-rest-str (parser-advance parser code-indent))))

(define (make-fenced-code match)
  (make-fenced-code-node 
   `((fence . ,(fenced-code-fence match))
     (fence-start . ,(fenced-code-start match))
     (info-string . ,(unescape-string (string-trim-both (fenced-code-info-string match)))))))

(define (make-bullet-list-marker parser match)
  (let ((rest-parser (bullet-list-rest parser match)))
    (make-list-node (make-item rest-parser
                               (bullet-list-offset parser rest-parser)
                               (bullet-list-spaces rest-parser))
                    `((type . bullet)
                      (tight . #t)
                      (bullet . ,(bullet-list-bullet match))))))

(define (make-ordered-list-marker parser match)
  (let ((rest-parser (ordered-list-rest parser match)))
    (make-list-node (make-item rest-parser
                               (ordered-list-offset parser rest-parser)
                               (ordered-list-spaces rest-parser))
                    `((type . ordered)
                      (start . ,(ordered-list-number match))
                      (tight . #t)
                      (delimiter . ,(ordered-list-delimiter match))))))

(define (make-item parser width spaces)
  (cond ((>= spaces 5)
         (make-item-node (parse-line (parser-advance parser 1))
                         (+ width 1)))
        ((< spaces 1)
         (make-item-node #f (+ width 1)))
        (else (make-item-node (parse-line (parser-advance parser spaces)) (+ width spaces)))))

(define (make-paragraph parser)
  (make-paragraph-node (parser-rest-str parser)))

(define (parse-clean-up node col)
  (cond ((not (node? node)) (col node '()))
        ((code-block-node? node) (remove-empty-lines node col))
        ((paragraph-node? node) (parse-reference-definition node col))
        ((list-node? node) (clean-list-nodes node col))
        ((item-node? node) (remove-blank-nodes node col))
        (else (filter-map&co parse-clean-up (node-children node)
                             (lambda (v d)
                               (col (make-node (node-type node)
                                               (node-data node)
                                               v)
                                    d))))))

(define (reverse-join ls)
  (reduce (cut string-append <> "\n" <>) "" ls))

(define (parse-reference-definition node col)
  (let loop ((text (reverse-join (node-children (last-child node))))
             (links '()))
    (cond ((link-definition text) =>
           (lambda (match)
             (loop (link-definition-rest match)
                   (cons (list (string-map char-downcase (link-definition-label match))
                               (link-definition-destination match)
                               (link-definition-title match))
                         links))))
          (else (col (if (= (string-length text) 0)
                         #f
                         (make-paragraph-node (string-trim-right text))) links)))))

(define (remove-empty-lines node col)
  (col (make-node (node-type node) (node-data node)
                  (list (reverse-join (drop-while (cut string-every char-set:blank <>) 
                                                  (node-children node)))))
       '()))

(define (clean-list-nodes node col)
  (define (any-blank-nodes? item)
    (any blank-node? (node-children item)))

  (let ((blank-nodes (any any-blank-nodes? (node-children node))))
    (filter-map&co parse-clean-up (node-children node)
                   (lambda (v d)
                     (col (make-node (node-type node)
                                     (if blank-nodes
                                         (acons 'tight #f (node-data node))
                                         (node-data node))
                                     v)
                          d)))))

(define (remove-blank-nodes node col)
  (cond ((no-children? node) (col node '()))
        (else (filter-map&co parse-clean-up (filter (negate blank-node?) (node-children node))
                             (lambda (v d)
                               (col (make-node (node-type node)
                                               (node-data node)
                                               v)
                                    d))))))
