;; -*- mode: common-lisp; package: net.xml.generator -*-
;;
;; generalized pretty-printing xml generator

;; This software is Copyright (c) Franz Inc, 2009
;; Franz Inc grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; There is no warranty provided by Franz Inc either explicitly or implicitly as to the correctness or
;; servicability of this code.  It is provided "as is" in the hope that it may be useful.  Comments and
;; feedback welcome at <bugs@franz.com>.

;; This code originally written by smh@franz.com

;;;
;;; This single file implements the :net-xml-generator module.
;;;

;; This module is mostly a readtable hack provides the a palatable syntax for XML generation by Lisp code.  In
;; order for Lisp and XML forms to coexist and nest arbitrarily, there must be some kind of syntactic marker
;; to differentiate Lisp and XML operators/tagnames.  The #\^ reader macro marks XML tagnames in source code.
;; Both Lisp source code and XML are trees.  Using this module the logical structure of application code that
;; generates XML maps simply and clearly onto the structure of the generted XML, except that the entire
;; vocabulary of Lisp forms (iteration, conditionals, case, and function calls) can be freely mixed with XML
;; generation.  The earlier, unrelated htmlgen module used keyword symbols to denote the fixed set of html
;; tags.  But this technique does not allow arbitrary nesting of html constructs inside Lisp syntactic
;; constructs, and required adding additional operators to support Lisp conditions, etc.  The reader macro
;; approach allows much cleaner, terser, and simpler code.

;; In Allegro CL the named-readtable facility makes it easy to associate customized readtables for particular
;; files.  Place a top-level form like

;;   (eval-when (compile eval) (setq *readtable* (excl:named-readtable :xml)))

;; somewhere near the top of a source file using this syntax.  In addition, put the attribute "readtable: xml"
;; in the Emacs mode line f the file so Emacs and the ACL IDE will also use this readtable for evaluation or
;; compilation requests associated with the buffer.  Such a line would look something like this

;; -*- mode: common-lisp; package: cl-user; readtable :xml -*-

;; The #\^ character is a syntactic marker that the following form should emit an XML element.  Within a start
;; element the #\@ character reads the next two subforms and generates an attribute.  (The #\@ character idiom
;; is suggested by both XSLT usage and Lisp backquote usage.)  Both of these can appear anywhere and can be
;; freely interspersed around and inside arbitrary Lisp forms.  This use of #\@ is culturally compatible with
;; the XSL world.  It makes no sense for a #\^ to appear inside another tag, but the #\@ character can be used
;; in element content as a shorthand to princ the result of executing the following form (most often a string
;; constant) to the XML stream.  Some illustrative examples, each of which must be lexically inside a
;; with-xml-generation form.

;;   ^foo ==> <foo/>
;;   ^(foo ^(bar)) ==> <foo><bar/></foo>
;;   ^((foo @id "31415") ^(bar)) ==> <foo id="31415"><bar/></foo>
;;   ^((foo ^bar) ^(bar)) ==> illegal
;;   ^(time @"The current UT is " ^(ut @(get-universal-time))) ==>
;;       <time>The current UT is <ut>3192471502</ut></time>
;;   ; The @ before the literal string in the previous example is optional since the
;;   ; string is at top-level of the ^ element body.  See below.
;;   ^((foo @name "xy&z") ^(bar)) ==> <foo name="xy&amp;z"><bar/></foo>

;; Arbitrary lisp code can appear freely, anywhere inside an XML element form, as all attributes and internal
;; elements are flagged syntactically with the #\^ and #\@ characters.

;; The #\@ read macro inside an element start consumes exactly two successive subforms.  The first is the
;; attribute name and the second is the attribute value.  A comma preceding the attribute name causes the name
;; form to be evaluated.

;; Elsewhere, outside an element start tag but as element content, the #\@ read macro causes the following
;; form to be evaluated and the result written to the XML output stream along with any necessary escaping.
;; Other forms are simple evaluated, but may contain appearances of the #\^ and #\@ macros to generate
;; respectively nested elements or element content.

;; As a syntatic convenience, a literal string at the top level of element content is treated as if prefaced
;; by the #\@ read macro.  Thus ^(foo @"bar") and ^(foo "bar") and ^(foo @'bar) all generate the same thing.
;; Any other form prefixed with the #\@ read macro is executed normally and then the result is printed to the
;; generated XML as if by princ.

;; The generated XML-generation code uses the pretty printer.  This may seem strange since pretty whitespace
;; is not only useless in XML source, it also both slows the generation speed and increases the length of the
;; generated XML.  However, pretty printed XML with proper indenting can greatly enhance human readability
;; during debugging.  Once an application goes into production the pretty printer can be turned off by binding
;; *print-pretty* nil, although some slight runtime cost still remains in the printer functions.  Someday we
;; may provide a read-time switch that generates XML-writing code without testing for *print-pretty* at all
;; and therefore completely avoids any residual overhead of the pretty printing capability, but so far this
;; residue appears quite small and probably not worth addressing.

;; All use of these xml generation reader macros must appear lexically inside a with-xml-generation macro
;; form.  One important thing this macro does is to bind the variable .xml-stream. to the output stream.  This
;; variable can be lambda bound by application code if, for instance, that code wants to generate multiple
;; related pages in parallel.  These pages might be a text page and an index or table of contents.  Each page
;; such should have its own with-xml-generation.

;; The two characters that trigger these reader macros default to #\^ and #\@ and are added to the any
;; readtable using the set-xml-generator-macro-chars function.  The default characters can be overridden.  A
;; standard Lisp readtable with the two character macros added is available as the value of *xml-readtable*.
;; See the readtable interface further down in this file.

;; For generating segments of XML where whitespace is significant, see the pre macro below.

;; The ACL XML parser can support XML namespaces by mapping them onto Lisp packages.  It was originally
;; intended for the XML generator to support this convention, but using the package machinery for XML
;; namespaces is rather ponderous and exposes application code to huge storage leaks, packages and the symbols
;; interned in them being persistent until the package is explicitly deleted.  So currently the XML generator
;; does not support element and attribute qnames as symbols in packages.  The #\^ reader macro simply reads
;; the follwing token as a string that may contain the #\: package marker.  This is usually simply much more
;; convenient when writing Lisp code that uses the XML generator.  See the explanastion of emit-lxml-as-xml
;; below for advice how to handle namespaces in lxml.

;;;
;;; To do:
;;;

;; What about DTD generation?  Integrate with the parser.

;; Implement namespace support.

;; This code can easily generate illegal XML if (for instance) illegal characters appear in Lisp symbols used
;; as XML element or attribute names.  This would be an application error, not the fault of this module, but
;; should this code check and signal error?  There would be a run time cost, of course.  Perhaps there should
;; be two versions of this code or a mode switch so the programmer can use the suspicious version during
;; development.  Could also verify against a DTD at generation time, preventing user code from generating
;; invalid XML.

;;;
;;; The implementation code begins here.
;;;

(defparameter *net-xml-generator-version* "1.0.1")

;;; Change history
;;;
;;; *** Version 1.0.1
;;;
;;; with-xml-generation binds *print-level* nil to protect against aserve worker thread binding.
;;;
;;; Missing (read-char) in xml-at caused ^((foo @,bar "boo")) to signal read-time error.
;;;
;;; *** Initial 2010-01-14 release.


(defpackage :net.xml.generator
  (:export :with-xml-generation :xml-write :emit-lxml-as-xml :*xml-readtable* :.xml-stream.
	   :set-xml-generator-macro-chars
	   :*netscape4-empty-element-compatibility*
	   :cdata :pre :write-xmldecl :write-doctype :*xml-line-break-style*))

(in-package :net.xml.generator)

;;;
;;; Some old browsers don't understand xhtml.
;;;

;; Netscape 4 doesn't understand XML empty-element tag syntax such as <br/>.  In XML this is entirely
;; equivalent to <br></br>.  Setting this variable true causes this slightly-more-verbose form to be used
;; instead.  Netscape4 and other ancient browsers have become extinct, but any applications that need to
;; generate xhtml for consumption by these dinosaurs want to bind variable this true.

(defparameter *netscape4-empty-element-compatibility* nil) ; changed from t 2009-09-25

;;;
;;; line break style
;;;

(defvar *xml-line-break-style* :linear)

;; Must be one of :linear, :mandatory, or :fill.  This kind of newline is emitted at the start of each
;; element.  :linear allows an element to print on a single line if all of its content fits.  :mandatory
;; causes each element start to break to a new line.  :fill allows multiple small inner elements to print on a
;; single line even if the entire containing element spans multiple lines.  This variable can be lambda bound
;; around the generation of local portions of an XML document.  See the following example.

#|

(loop with *print-right-margin = 70
    for lf in '(:linear :mandatory :fill)
    do (with-xml-generation (*standard-output*)
	 (let ((net.xml.generator::*xml-line-break-style* :linear))
	   ^(table
	     ^((tr @bg "green") ^(th @"Name") ^(th @"Id"))
	     ^(tr ^(td "Joe") ^(td @12345))
	     ^(tr ^(td "Xavier") ^(td @54321)))))
       (terpri))
<table>
  <tr bg="green"><th>Name</th><th>Id</th></tr>
  <tr><td>Joe</td><td>12345</td></tr>
  <tr><td>Xavier</td><td>54321</td></tr>
</table>
<table>
  <tr bg="green">
    <th>Name</th>
    <th>Id</th>
  </tr>
  <tr>
    <td>Joe</td>
    <td>12345</td>
  </tr>
  <tr>
    <td>Xavier</td>
    <td>54321</td>
  </tr>
</table>
<table><tr bg="green"><th>Name</th><th>Id</th></tr><tr><td>Joe</td><td>12345</td></tr>
  <tr><td>Xavier</td><td>54321</td></tr>
</table>

|#

(eval-when (compile load eval)

(defparameter *allow-xml-generator-optimization* t)

  )					; eval-when

(defvar .xml-stream.)

;; This macro returns nil.
(defmacro with-xml-generation ((stream-var &key) &body body)
  `(let ((stream-var ,stream-var)
	 ;; The next binding is prophylactic against aserve which limits *print-level* to prevent
	 ;; circular log output.
	 (*print-level* nil))
     (pprint-logical-block (stream-var nil)
       ;; This is necessary to work around a bug that ACL socket streams don't properly
       ;; support detection of column position.
       #+allegro (setf (slot-value stream-var 'excl::charpos) nil)
       (let ((.xml-stream. stream-var))
	 ,@body))))

(defun read-xml-tag (stream)
  (let ((evalp (eql (peek-char t stream) #\,)))
    (if evalp
	(progn (read-char stream)
	       (read stream t nil t))
      (with-output-to-string (s)
	(loop while (xml-namechar-p (peek-char nil stream nil nil))
	    do (write-char (read-char stream) s))))))

(defvar *attribute-context* nil)

(defun xml-caret (stream char)
  (declare (ignore char))
  (unless (eql (peek-char t stream) #\()
    ;; Simple ^foo empty element.
    (return-from xml-caret
      `(pprint-element ,(read-xml-tag stream) nil nil)))
  (read-char stream)			; Eat the open paren.
  (let (element-name attribute-continuation)
    ;; Check for ^((foo @bar "..") ...) form with attributes.
    (cond ((eql (peek-char t stream) #\()
	   (read-char stream)		; Eat the open paren.
	   (setq element-name (read-xml-tag stream))
	   (let* ((*attribute-context* t)
		  (attribute-body (read-delimited-list #\) stream t)))
	     (when attribute-body
	       (setq attribute-continuation
		 `(lambda () ,@attribute-body)))))
	  ;; Simple non-list element ^(foo ...).
	  (t (setq element-name (read-xml-tag stream))))
    ;; Now process the body.
    `(pprint-element
      ,element-name
      ,attribute-continuation
      ,(let ((body (read-delimited-list #\) stream t)))
	 (when body
	   ;; Wrap any top-level body strings with an automatic xml-write, except that
	   ;; we'll write as xml-write-1 so that the printer can recognize it as top level
	   ;; and suppress the #\@ for print-read consistency.
	   `(lambda () ,@(loop for form in body
			     collect (if (stringp form)
					 `(xml-write-1 ,form)
				       form))))))))

#+notyet
(defun string-constant-p (form environment)
  (when (atom form)
    (multiple-value-bind (new changedp) (macroexpand form)
      (when changedp (return-from string-constant-p (string-constant-p new)))))
  (let ((compiler-macro (compiler-macro-function (car form) environment)))
    (when compiler-macro
      (let ((new (funcall *macroexpand-hook* compiler-macro form environment)))
	(when (eq new form)
	  (return-from string-constant-p (string-constant-p new))))))
  ;; All macroexpansion has been done at top level.
  (when (constantp form environment)

    ))

(defun xml-at (stream char)
  (declare (ignore char))
  (if *attribute-context*
      (let* ((evalp (eql (peek-char t stream) #\,))
	     (name (if evalp
		       (progn (read-char stream) (read stream))
		     (read-xml-tag stream)))
	     (val (read stream)))
	`(write-xml-attribute .xml-stream. ,name ,val))
    `(xml-write ,(read stream))))

(defun write-xml-attribute (stream attribute value)
  (when value				;mt: if value is nil, do nothing
    (write-char #\space stream)
    (when *print-pretty* (pprint-newline :fill stream))
    (pprint-logical-block (stream nil)
      (pprint-indent :block 2 stream)	; parameterize?
      (princ attribute stream)
      (pprint-newline :fill stream)
      (write-char #\= stream)
      (pprint-newline :fill stream)
      (let ((val (if (stringp value)
		     value
		     (princ-to-string value))))
	;; Now find and eliminate any appearances the three forbidden attval characters: &lt;
	;; &amp; &quot;.  This could be both smarter and faster, but perhaps not both.  A
	;; smarter version would be clever about choosing between &quot; or &apos;.  But this
	;; would require traversing the string an extra time, or doing more bookkeeping.  It
	;; might also be a lot more efficient to accumulate a string and then print it once,
	;; avoiding individual writes to the stream.
	(loop for c across val
	   initially (write-char #\" stream)
	   do (case c
		(#\< (write-string "&lt;" stream))
		(#\& (write-string "&amp;" stream))
		(#\" (write-string "&quot;" stream))
		(t (write-char c stream)))
	   finally (write-char #\" stream))))))

;; This will someday usually be bypassed by the pprint-element compiler macro, but that isn't yet completely
;; implemented.
(define-compiler-macro write-xml-attribute (&whole whole stream attribute value &environment e)
  (if (and *allow-xml-generator-optimization*
	   (constantp attribute e)
	   (constantp value e))
      `(write-string-xx ,(with-output-to-string (s) (write-xml-attribute s attribute value))
		     ,stream)
    whole))

#+unused
(defun attributize (value stream)
  ;; Ensure the attribute is a string.
  (let ((val (if (stringp value)
		 value
	       (princ-to-string value))))
    ;; Now find and eliminate any appearances the three forbidden attval characters: &lt;
    ;; &amp; &quot;.  This could be both smarter and faster, but perhaps not both.  A
    ;; smarter version would be clever about choosing between &quot; or &apos;.  But this
    ;; would require traversing the string an extra time, or doing more bookkeeping.  It
    ;; might also be a lot more efficient to accumulate a string and then print it once,
    ;; avoiding individual writes to the stream.
    (loop for c across val
	initially (write-char #\" stream)
	do (case c
	     (#\< (write-string "&lt;" stream))
	     (#\& (write-string "&amp;" stream))
	     (#\" (write-string "&quot;" stream))
	     (t (write-char c stream)))
	finally (write-char #\" stream))))

;; This princs an arbitrary Lisp value (typically a string) to the XML stream,
;; substituting the &lt; and &amp; entities.
(defun xml-write (value)
  (declare (optimize speed))
  ;; Ensure the data is a string.
  (let ((stream .xml-stream.)
	(val (if (simple-string-p value)
		 value
	       (princ-to-string value))))
    ;; Find and eliminate any appearances the two forbidden character data characters:
    ;; &lt; &amp;.  This code could be faster, perhaps by accumulating a stream and then
    ;; making only a single write-string call.
    (loop for c across (the simple-string val)
	do (case c
	     (#\< (write-string "&lt;" stream))
	     (#\& (write-string "&amp;" stream))
	     (t (write-char c stream))))))

;; This macro is equivalent to xml-write, except that the pretty printer recognizes it and
;; suppresses the #\@ character.
(defmacro xml-write-1 (form)
  `(progn (pprint-newline :fill .xml-stream.)
	  (xml-write ,form)))

#+unused
(define-compiler-macro cformat (&whole whole stream control &rest args &environment e)
  ;; Slightly nonportable owing to fuzzy requirements on constantp.
  (if (and (constantp control) (every (lambda (x) (constantp x e)) args))
      (let ((s (gensym)))
	`(let ((,s ,stream))
	   (write-string ,(apply #'format nil control args) ,s)))
    whole))

(defmacro write-string-xx (string stream)
  `(write-string ,string ,stream))

(defun optimize-attribute-continuation (forms xml-stream e)
  (let* ((simplifiedp nil)
	 (rewritten-forms (loop for form in forms
			      do (format t "Form is ~s~%" form)
			      if (and (consp form)
				      (eq (car form) 'write-xml-attribute)
				      (constantp (caddr form) e)
				      (constantp (cadddr form) e))
			      do (setf simplifiedp t)
			      and collect `(z ,(with-output-to-string (s)
						 (write-xml-attribute s
								      (caddr form)
								      (cadddr form))))
			      else collect form)))
    (unless simplifiedp (return-from optimize-attribute-continuation (values forms nil)))
    (loop as x = rewritten-forms
	while (cdr x)
	if (and (consp (first  x)) (eq (car (first  x)) 'z)
		(consp (second x)) (eq (car (second x)) 'z))
	do (setf (second (first x)) (concatenate 'string (second (first x)) (second (second x)))
		 (cdr x) (cddr x))
	else do (pop x))
    (values (loop for form in rewritten-forms
		if (and (consp form) (eq (car form) 'z))
		collect `(write-string-xx ,(cadr form) ,xml-stream)
		else collect form)
	    t)))

(defun optimize-body-continuation (forms xml-stream e)
  (let* ((simplifiedp nil)
	 (rewritten-forms (loop for form in forms
			      do (format t "Form is ~s~%" form)
			      if (and (consp form)
				      (eq (car form) 'write-xml-attribute)
				      (constantp (caddr form) e)
				      (constantp (cadddr form) e))
			      do (setf simplifiedp t)
			      and collect `(z ,(with-output-to-string (s)
						 (write-xml-attribute s
								      (caddr form)
								      (cadddr form))))
			      else collect form)))
    (unless simplifiedp (return-from optimize-body-continuation (values forms nil)))
    (loop as x = rewritten-forms
	while (cdr x)
	if (and (consp (first  x)) (eq (car (first  x)) 'z)
		(consp (second x)) (eq (car (second x)) 'z))
	do (setf (second (first x)) (concatenate 'string (second (first x)) (second (second x)))
		 (cdr x) (cddr x))
	else do (pop x))
    (values (loop for form in rewritten-forms
		if (and (consp form) (eq (car form) 'z))
		collect `(write-string-xx ,(cadr form) ,xml-stream)
		else collect form)
	    t)))

(defun pprint-element (tag-name attribute-continuation body-continuation)
  (let ((xml-stream .xml-stream.)
	(name tag-name))
    (cond
     (*print-pretty*
      (pprint-newline *xml-line-break-style* xml-stream)
      (pprint-logical-block (xml-stream nil) ; l-b for the element start/content/end
	(pprint-logical-block (xml-stream nil ; l-b for the element tag
					  :prefix "<"
					  :suffix (if body-continuation ">"
						    (if *netscape4-empty-element-compatibility*
							(format nil "></~a>" name)
						      "/>")))
	  (write-string name xml-stream)
	  ;;(pprint-indent :block 4)
	  (when attribute-continuation
	    (cond ((stringp attribute-continuation)
		   (write-string attribute-continuation xml-stream))
		  (t #+nop (pprint-indent :block 0 xml-stream) ; parameterize?
		     (funcall attribute-continuation)))))
	(when body-continuation
	  (pprint-indent :block 2 xml-stream) ; parameterize?
	  ;; (pprint-newline :linear xml-stream)
	  (unwind-protect
	      (funcall body-continuation)
	    (pprint-indent :block 0 xml-stream)
	    (format xml-stream "~_</~a>" name)))))
     (t
      (write-char #\< xml-stream)
      (write-string name xml-stream)
      (unwind-protect
	  (when attribute-continuation
	    (cond ((stringp attribute-continuation)
		   (write-string attribute-continuation xml-stream))
		  (t #+nop (pprint-indent :block 0 xml-stream) ; parameterize?
		     (funcall attribute-continuation))))
	(cond (body-continuation
	       (write-char #\> xml-stream)
	       (unwind-protect
		   (funcall body-continuation)
		 (write-char #\< xml-stream)
		 (write-char #\/ xml-stream)
		 (write-string name xml-stream)
		 (write-char #\> xml-stream)))
	      (*netscape4-empty-element-compatibility*
	       (write-string "></" xml-stream)
	       (write-string name xml-stream)
	       (write-char #\> xml-stream))
	      (t (write-string "/>" xml-stream)))))))
  (values))

;;;
;;; <pre> and other elements woth significant whitespace
;;;

;; Support for elements where whitespace is significant, e.g. HTML <pre>.  The pre Lisp macro forces alignment
;; back to the first column and turns off pretty printing around its body.  The name "pre" is only suggestive
;; as this macro does not itself actually emit an HTML <pre> element or any other markup.  That is the
;; responsibility of the code body.  The implementation depends upon behavior of the ACL implementation and
;; might not be portable to other versions of the CL pretty printer.  The intention is that the pre Lisp macro
;; gets wrapped around the ^(pre ...) element to turn off additional writespace.  See the example below.

;; Interpretation of initial and final line breaks in HTML is complex.  See this note in
;; http://www.w3.org/TR/html4/appendix/notes.html#notes-line-breaks

;;    SGML (see [ISO8879], section 7.6.1) specifies that a line break immediately following a start tag must
;;    be ignored, as must a line break immediately before an end tag. This applies to all HTML elements
;;    without exception.

;; It would be hard to get the pretty printer to emit line breaks (and no additional indentation) immediately
;; after the next start tag, and immediately before its matching end tag, but the pre macro eliminates
;; entirely line breaks around whatever is emitted by the code body.  This should suffice.

#|

  (with-xml-generation (*standard-output*)
    ^(html
      ^(head ^(title "factorial in Lisp"))
      ^(body
	^(p "Here is the" ^(b "factorial") " function in Lisp:"
	    (pre ^(pre (let ((*print-pretty* t)
			     (*print-right-margin* 40)
			     (*print-miser-width* 20))
			 @(princ-to-string
			   '(defun factorial (n)
			     (if (< n 2)
				 n
			       (* n (factorial (1- n)))))))))
	    "Call it this way: "
	    ^ (tt "(factorial 10)")))))

emits:

  <html>
    <head><title>factorial in Lisp</title></head>
    <body>
      <p>Here is the
	<b>factorial</b>
	 function in Lisp:
  <pre>(defun factorial (n)
    (if (&lt; n 2)
	n
      (* n (factorial (1- n)))))</pre>
	Call it this way:
	<tt>(factorial 10)</tt>
      </p>
    </body>
  </html>



|#

(defmacro pre (&body body)
  `(let ((xml-stream .xml-stream.))
     (pprint-logical-block (xml-stream nil)
       (pprint-indent :current -1000 xml-stream)
       (pprint-newline :mandatory xml-stream)
       (pprint-logical-block (xml-stream nil)
	 (let ((*print-pretty* nil)) ,@body)))))

(defmacro cdata (&body body)
  `(progn (write-string "<![CDATA[" .xml-stream.)
	  (progn ,@body)
	  (write-string "]]>" .xml-stream.)))

;;;
;;;
;;;

;; A utility for reading element names.  This depends on the implementation of ACL readtables, that all
;; constituent characters happen to have the same function as their macro-character dispatch.  It does
;; approximately the right thing (accepting some invalid characters) but should be replaced with a serious
;; XML-compliant definition.

(defun xml-namechar-p (char)
  (and char				; Handle eof elegantly.
       (eql (get-macro-character char)
	    (load-time-value (get-macro-character #\A)))))

(defun write-xmldecl (stream &optional version)
  (format stream "<?xml~@[ version=\"~a\"~]?>~%" version))

;; <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
;;  ExternalID     ::=          'SYSTEM' S  SystemLiteral
;;                              | 'PUBLIC' S PubidLiteral S SystemLiteral

(defun write-doctype (stream name system-literal &optional public-literal)
  ;; Note that system-literal and public-literal appear in reverse order than in the doctype statement.
  (format stream "<!DOCTYPE ~a~:[ SYSTEM~; ~:*PUBLIC ~s~] ~s>~%"
	      name public-literal system-literal))

;;;
;;; pretty-printing (reconstructing) these reader macro forms like cl:quote.
;;;

;; A pprint dispatch that can reconstruct the #\^ source form.  This is hazardous in that it signals error if
;; a putative pprint-element form is malformed.
(defun print-pprint-element (stream form)
  ;; Must bulletproof all this destructuring!!!
  (destructuring-bind (op tag-name attribute-continuation body-continuation)
      form
    (declare (ignore op))
    ;;(write body-continuation :stream *trace-output* :pretty nil) (terpri *trace-output*)
    (cond (attribute-continuation
	   (pprint-logical-block (stream attribute-continuation :prefix "^(" :suffix ")")
	     (pprint-logical-block (stream attribute-continuation :prefix "(" :suffix ")")
	       (write tag-name :stream stream :escape nil)
	       (loop for first = t then nil
		   for attr in (cddr attribute-continuation)
		   do (write-char #\space stream)
		      (if first
			  (progn (pprint-indent :current 0 stream)
				 (pprint-newline :miser stream))
			(pprint-newline :fill stream))
		      (write attr :stream stream)))
	     (loop for body-form in (cddr body-continuation)
		 do (write-char #\space stream)
		    (pprint-newline :linear stream)
		    (write body-form :stream stream))))
	  (body-continuation		; but no attribute continuation
	   (pprint-logical-block (stream body-continuation :prefix "^(" :suffix ")")
	     (write tag-name :stream stream :escape nil)
	     (loop for body-form in (cddr body-continuation)
		 do (write-char #\space stream)
		    (pprint-newline :linear stream)
		    (write body-form :stream stream))))
	  (t (write-char #\^ stream)
	     (princ tag-name stream)))))

(defun print-write-xml-attribute (stream form)
  ;; Must bulletproof all this destructuring!!!
  (destructuring-bind (op stm attribute-form value-form)
      form
    (declare (ignore op stm))
    (format stream "~@<@~a~2I ~_~w~:>" attribute-form value-form)))

(defun print-xml-write (stream form)
  ;; Must bulletproof all this destructuring!!!
  (destructuring-bind (op arg)
      form
    (unless (eql op 'xml-write-1) (write-char #\@ stream))
    (write arg :stream stream)))

;; These pprint-dispatch entries assume that the :xml readtable will be in effect if the
;; printed forms are reread.  This is no different than what is done for backquote, except
;; that backquote is defined in the standard readtable.

(progn
  (set-pprint-dispatch '(cons (member pprint-element))        #'print-pprint-element)
  (set-pprint-dispatch '(cons (member xml-write xml-write-1)) #'print-xml-write)
  (set-pprint-dispatch '(cons (member write-xml-attribute))   #'print-write-xml-attribute)
  )

;;;
;;; Define a readtable that handles the ^ and @ chars.  Interface with the ACL
;;; named-readtable facility so Emacs and IDE tools can find the right readtable.
;;;

(defun set-xml-generator-macro-chars (element-char attribute-char &optional (rt *readtable*))
  ;; These are less likely to cause compatibility problems if non-terminating-p.  Changed 2008-06-02 smh.
  (set-macro-character element-char #'xml-caret t rt)
  (set-macro-character attribute-char #'xml-at t rt)
  rt)

(defparameter *xml-readtable*
    (let ((rt (or (excl:named-readtable :xml nil)
		  (setf (excl:named-readtable :xml) (copy-readtable)))))
      (set-xml-generator-macro-chars #\^ #\@ rt)
      rt))

;;;
;;; Support for printing lxml trees.
;;;

;; This reconstructs an lxml tree into the original XML.  It does not deal with the ACL Sax package
;; conventions.  To parse and reconstruct some XML through lxml, the parse should be done with :namespace nil.
;; That way a qname like "ex:foo" will be parted as a symbol in the parser's default package (typically the
;; keyword package) with the colon as a plain character in the symbol name.

;; emit-lxml-as-xml does _not_ need to be wrapped in a with-xml-generation macro

;; Unfortunately, at this writing released versions of net.xml.sax:parse-to-lxml doesn't implement the
;; :namespace keyword argument, so the parse-to-lxml call in the following example will not work.  This should
;; be patched soon after the release of ACL 8.2.

#|

  (let ((*print-pretty* t)
	(*print-right-margin* 50)
	(*print-miser-width* 20)
	(lxml (net.xml.sax:parse-to-lxml "<a:one><b:two radix='16'>face&amp;neck</b:two></a:one>"
					 :namespace nil)))
    ;; parse-to-lxml returns a list.  Although there can be only a single element in an xml document, that
    ;; element can be followed by any number of pi or comment.
    (pprint lxml) (terpri)
    (loop for xml in lxml
	do (emit-lxml-as-xml *standard-output* xml)))

prints

  ((:|a:one|
    ((:|b:two| :radix "16") "face" "&" "neck")))
  <a:one>
    <b:two radix="16">face&amp;neck</b:two>
  </a:one>

|#

(defun emit-lxml-as-xml (.xml-stream. lxml)
  (flet ((stringify (thing)
	   (etypecase thing
	     (string thing)
	     (symbol (symbol-name thing)))))
    (pprint-logical-block (.xml-stream. nil)
      (destructuring-bind (tag &rest contents) lxml
	(let ((body-continuation (and contents
				      (lambda ()
					(loop for content in contents
					    do (pprint-newline *xml-line-break-style* .xml-stream.)
					       (cond ((stringp content)
						      (xml-write content))
						     ((atom content)
						      (xml-write content))
						     (t (emit-lxml-as-xml .xml-stream. content))))))))
	  (if (consp tag)
	      (pprint-element (stringify (car tag))
			      (lambda ()
				(loop for (attribute value) on (cdr tag) by #'cddr
				    do (write-xml-attribute .xml-stream. attribute value)))
			      body-continuation)
	    (pprint-element (stringify tag) nil body-continuation)))))))

;;;
;;;
;;;

(provide :net-xml-generator)
