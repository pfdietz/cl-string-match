;;; -*- package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

;; Copyright (c) 2017 Victor Anyakin <anyakinvictor@yahoo.com>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of the organization nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL COPYRIGHT HOLDER BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; --------------------------------------------------------
;; Generate API reference using MGL-PAX library by Gábor Melis
;; <mega@retes.hu>
;; --------------------------------------------------------

(in-package :cl-string-match)

;; --------------------------------------------------------

(defsection @cl-string-match-manual (:title "CL-STRING-MATCH Manual")
  "CL-STRING-MATCH String and pattern matching library reference."
  (cl-string-match asdf:system)
  (@single-pattern-search section)
  (@multi-pattern-search section)
  (@regexp-pattern-search section))

(defsection @single-pattern-search (:title "Single pattern search")
  "Looking for a single pattern in a string"
  (@brute-force-section section)
  (@boyer-moore-section section)
  (@boyer-moore-horspool-section section)
  (@rabin-karp-section section)
  (@knuth-morris-pratt-section section)
  (@shift-or-section section))

(defsection @multi-pattern-search (:title "Multiple pattern search")
  "Looking for multiple patterns in a string"
  (@aho-corasick-section section))

(defsection @regexp-pattern-search (:title "Regular expressions")
  "Parsing and interpreting regular expressions"
  (@pre-regexp-section section))

;; --------------------------------------------------------

(defun update-md ()
  (let ((mgl-pax:*DOCUMENT-LINK-CODE* NIL)
	(mgl-pax:*DOCUMENT-LINK-SECTIONS* NIL))
    (with-open-file (stream (ensure-directories-exist
			     (asdf:system-relative-pathname
			      :cl-string-match "doc/md/sm-manual.md"))
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (document @cl-string-match-manual :stream stream)
      (print-markdown-footer stream))))

(defun print-markdown-footer (stream)
  (format stream "~%* * *~%")
  (format stream "###### \\[generated by ~
                   [MGL-PAX](https://github.com/melisgl/mgl-pax)\\]~%"))
;; (update-md)

;; EOF
