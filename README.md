net-xml-generator - A Pretty Printing XML Generator for Common Lisp
===================================================================

Table of contents
-----------------

 * Description
 * Author
 * Author comments
 * Documentation
 * Platforms
 * Dependencies
 * Installation
 * Configuration
 * Licence
 * Notes
 * Examples
 * Franz Inc. Open Source Info

Description
-----------

This module consists of a single source file net-xml-generator.cl.

It provides a modified readtable that allows normal Lisp source code
to contain possibly-nested marked subforms that emit an XML element.
The tree that is Lisp source code integrates transparently with the
tree that is the intended XML, and the entire panoply of Lisp
operators (iteration, special forms, function calls) can be mixed with
XML generation.  The Lisp code that emits XML has a structure
paralleling the XML.

The Common Lisp pretty printer is optionally used to indent the
emitted XML for human readability.  See the file
xml-generator-blurb.html distributed along with this module for a
gentle introduction, and xml-generator-blurb.cl which shows how that
html file was itself generated using the module.

Author
------

Steven Haflich, Franz Inc.

Author comments
---------------

Platforms
----------

All Allegro Common Lisp versions.  It mostly should work on other
implementations, but uses the ACL named-readtable facility (which
could trivially be added to any other implementation) and more
importantly may depend on subtle details of the pretty printer.

Dependencies
------------

Allegro Common Lisp 

The Allegro Common Lisp excl:named-readtable facility.

Installation
------------

Start your lisp and compile and load net-xml-generator.cl which is
part of this project:

    (load (compile-file "/path/to/your/net-xml-generator.cl")

To test, 

    (load "/path/to/your/xml-generator-blurb.cl")
    (generate-this-page :out-path "./xml-generator-blurb-copy.html" )

and that named file will be generated which should be an exact copy of
xml-generator-blurb.html

Configuration
-------------

No configuration is necessary, but see the documentation in the source
file how to use the customized readtable in your Lisp code.

Documentation
-------------

The full documentation is contained in block comments in the
net-xml-generator.cl source file itself.

License
-------

The net-xml-generator source code is licensed under the terms of the
[Lisp Lesser GNU Public License](http://opensource.franz.com/preamble.html),
known as the LLGPL. The LLGPL consists of a preamble and the
LGPL. Where these conflict, the preamble takes precedence.  This
project is referenced in the preamble as the LIBRARY.

Notes
-----

An earlier version of this code was first released as part of the Ray
Tracing example in the Franz Inc Dynamic Learning Center.

Examples and Information
------------------------

The xml-generator-blurb.cl is an odd, self-referential example of Lisp
code using the generator.

Franz Open Source Info
----------------------

This project's homepage is <http://opensource.franz.com>. There is an 
informal community support and development mailing list 
[opensource@franz.com](http://opensource.franz.com/mailinglist.html) 
for these open source projects. We encourage you to take advantage by 
subscribing to the list.  Once you're subscribed, email to 
<opensource@franz.com> with your questions, comments, suggestions, 
and patches.
