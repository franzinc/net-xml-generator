; -*- mode: common-lisp; package: cl-user; readtable: xml -*-

(in-package :user)

(eval-when (compile load eval)
  ;; This require won't work until xml-generator is included in the ACL code directory.  So we'll give a
  ;; useful error if it hasn't yet been loaded.
  (ignore-errors (require :net-xml-generator))
  (unless (find-package :net.xml.generator)
    (error "This file ~a cannot be loaded unless the :net.xml.generator module is loaded first."
	   (or *compile-file-pathname* *load-pathname*)))
  (use-package :net.xml.generator)
  (setq *readtable* (named-readtable :xml t)))

(defparameter *this-source-file* (macrolet ((x ()
					      (or *compile-file-pathname*
						  (load-time-value *load-pathname*))))
				   (x)))

(defun generate-this-page (&key (out-path "./xml-generator-blurb.html"))
  (let ((*print-right-margin* 92))
    (with-open-file (out out-path :direction :output :if-exists :supersede)
      (with-xml-generation (out)
	(write-xmldecl out "1.0")
	^(html
	  ^(head
	    ^(title "The Net-Xml-Generator Blurb")
	    ^((style @type "text/css")
	      "pre.example  { color: rgb(20, 20, 0); background-color: rgb(200, 200, 255);
 			    font-size:85%; margin-left:50px; margin-right:50px }"
	      "pre.result   { color: rgb(20, 0, 20); background-color: rgb(200, 255, 200);
 			    font-size:85%; margin-left:70px; margin-right:50px }"
	      "div.rendered { color: rgb(0, 20, 20); background-color: rgb(255, 200, 200);
 			    font-size:85%; margin-left:90px; margin-right:50px }"
	      ))
	  ^(body

	    ^(h1 "The Net-Xml-Generator Blurb")

	    ^(p ^(b "Allegro Common Lisp") " has a new, open-source module named "
		^(b "xml-generator") " available from "
		^((a @href "http://opensource.franz.com/") "opensource.franz.com") ".  "
		"

The module uses the Common Lisp pretty printer and a modified readtable to integrate the generation of
pretty-printed XML (intuitive indentation and line breaks) with arbitrary Lisp code.  In many dialects of XML
white space is not significant, but when developing applications that emit long XML documents, human
readability is essential.  Everyone has had the pain of trying of understand the source for a web page in a
browser where all the (x)html is on a single unreadable looooooong line.  Once an application is debugged, of
course, the pretty printer can simply be turned off to reduce the size of the generated output.")

	    ^(p "

Even more important, the module employs a customized readtable so that XML elements are be marked
lexicographically.  Both Lisp source code and XML are trees.  Using this module the logical structure of
application code that generates XML maps simply and clearly onto the structure of the generated XML, except
that the entire vocabulary of Lisp forms (iteration, conditionals, case, and function calls) can be freely
mixed with XML generation.  This is a significant difference from the unrelated htmlgen module.  See the
examples below.")

	    ^(p "

The two additional macro characters default to `^' and `@'.  The `^' character marks the start of an XML
element.  The `@' character inside a element start tag marks the next two subforms as an attribute/value pair,
and inside element content marks the next subform as content.  A string as a top-level subform of an XML
element will generate element content, even withoute `@' reader macro.")

	    ^(p "

The detailed documentation for the module is in the net-xml-generator.cl source file itself, but here are some
examples that show use of these syntax extensions and the generated XML.  These examples all happen to
generate XHTML, so we'll also show the rendered example:" )
	    (let ((examples
		   '(
		     ^(p "Hello, world!")
		     ^(p @"Hello, world!")
		     ^(p @@"Hello,&nbsp;world!")
		     ^((p @id 42) "Hello, world!")
		     ^(center "Above the line." ^hr "Below the line.")
		     ^((table @rules "all" @frame "box")
		       ^((tr @bgcolor "Silver") ^(th @"Name") ^(th @"Phone")) ; Use of bgcolor is deprecated.
		       ^(tr ^(td "Joe")    ^(td @"555-1234"))
		       ^(tr ^(td "Xavier") ^(td @"555-5678")))
		     ^(table
		       ^((tr @bgcolor "Silver") ^(th @"operator") ^(th @"arglist"))
		       (do-external-symbols (op :net.xml.generator)
			 (when (fboundp op)
			   ^(tr ^(td @(symbol-name op))
				^(td @(format nil "~{~a~^ ~}" (excl:arglist op)))))))
		     ))
		  (*print-right-margin* 70)
		  (*print-miser-width*  20))
	      (dolist (example examples)
		(pre ^((pre @class "example")
		       @(with-output-to-string (str)
			  (pprint example str))))
		(pre ^((pre @class "result")
		       @(let ((*print-pretty* t)) ; The surrounding pre turns it off!
			  (with-output-to-string (str)
			    (with-xml-generation (str)
			      (eval example))))))
	       ^((div @class "rendered")
		 (eval example))))

	    ^hr
	    ^(h2 "How this page was generated")
	    ^(p "

Through use of a clever self-referential hack, here is the actual Common Lisp source file that generated this
xhtml page."  )

	    (pre ^((pre @class "example")
		   ^(tt @(file-contents *this-source-file*))))
	    ))))))

(eval-when (load eval)
  (format t
	  "~&;;~%;; To generate this xhtml page, execute ~
           (generate-this-page :out-path \"./xml-generator-blurb-copy.html\") .~%;;~%"))
