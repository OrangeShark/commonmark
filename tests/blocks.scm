;; Copyright (C) 2016  Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (test-blocks)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark blocks))

(test-begin "blocks")

(test-equal "parse-blocks, empty document"
            (call-with-input-string "" parse-blocks)
            '(document ((closed . #f))))

(test-equal "parse-blocks, simple paragraph"
            (call-with-input-string "foo" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo"))))

(test-equal "parse-blocks, three space simple paragraph"
            (call-with-input-string "   foo" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo"))))

(test-equal "parse-blocks, multiline paragraph"
            (call-with-input-string "foo\nbar" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo\nbar"))))

;; not fixed yet
(test-expect-fail "parse-blocks, code block does not interrupt paragraph")
(test-equal "parse-blocks, code block does not interrupt paragraph"
            (call-with-input-string "foo\nbar" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo\nbar"))))

(test-equal "parse-blocks, multiline paragraph preserves line ending spaces"
            (call-with-input-string "foo   \nbar" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo   \nbar"))))

(test-equal "parse-blocks, thematic breaks with *"
            (call-with-input-string "***\n **  * ** * ** * **" parse-blocks)
            '(document ((closed . #f))
                       (thematic-break ((closed . #t)))
                       (thematic-break ((closed . #t)))))

(test-equal "parse-blocks, thematic breaks with -"
            (call-with-input-string "---\n - - -" parse-blocks)
            '(document ((closed . #f))
                       (thematic-break ((closed . #t)))
                       (thematic-break ((closed . #t)))))

(test-equal "parse-blocks, thematic breaks with _"
            (call-with-input-string "___\n   _______________________  " parse-blocks)
            '(document ((closed . #f))
                       (thematic-break ((closed . #t)))
                       (thematic-break ((closed . #t)))))

(test-equal "parse-blocks, thematic breaks must not have other characters"
            (call-with-input-string "---a---" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "---a---"))))

(test-assert "parse-blocks, thematic breaks can interupt a paragraph"
             (match (call-with-input-string "Foo\n***\nbar" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                      ('text text-data1 "bar"))
                           ('thematic-break (('closed . #t)))
                           ('paragraph para-data2
                                      ('text text-data2 "Foo")))
                #t)
               (x (pk 'fail x #f))))

(define (heading-level heading-data)
  (assq-ref heading-data 'level))

(test-assert "parse-blocks, atx headings"
             (match (call-with-input-string
                     "# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo" parse-blocks)
               (('document doc-data
                           ('heading heading-data6
                                    ('text text-data6 "foo"))
                           ('heading heading-data5
                                    ('text text-data5 "foo"))
                           ('heading heading-data4
                                    ('text text-data4 "foo"))
                           ('heading heading-data3
                                    ('text text-data3 "foo"))
                           ('heading heading-data2
                                    ('text text-data2 "foo"))
                           ('heading heading-data1
                                    ('text text-data1 "foo")))
                (and (eq? (heading-level heading-data6) 6)
                     (eq? (heading-level heading-data5) 5)
                     (eq? (heading-level heading-data4) 4)
                     (eq? (heading-level heading-data3) 3)
                     (eq? (heading-level heading-data2) 2)
                     (eq? (heading-level heading-data1) 1)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings not more than 6 #"
             (match (call-with-input-string "####### foo" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                      ('text text-data "####### foo")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings requires an empty space"
             (match (call-with-input-string "#hashtag" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "#hashtag")))
                #t)
               (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
