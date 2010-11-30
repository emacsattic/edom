;;; edom.el --- Minimal DOM support on top of xml.el

;; Copyright (C) 2007  Edward O'Connor

;; Author: Edward O'Connor <hober0@gmail.com>
;; Keywords: xml
;; URL: http://edward.oconnor.cx/elisp/edom.el
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Very basic, very primitive DOM methods that operate on the output of
;; xml.el's XML parsing routines.

;; Not to be confused with the biblical nation of the same name.
;; Ironically, they may have worshipped El, so "edom.el" is particularly
;; rich.

;;; History:
;; 2007-08-10: Initial version.

;;; Code:

(require 'xml)

;; Utilities

(defun edom-element-p (thingy)
  "Non-nil if THINGY is an element."
  (and (consp thingy)
       (symbolp (car thingy)) ;; element name
       (consp (cdr thingy))
       (listp (cadr thingy)) ;; attribute list
       (listp (cddr thingy)))) ;; children

(defun edom-getattr (element attribute &optional default)
  "Return ELEMENT's value of ATTRIBUTE, or DEFAULT if none exists."
  (or (cdr (assoc attribute (xml-node-attributes element))) default))

(defun edom-has-class-p (element class)
  "Non-nil if ELEMENT has CLASS."
  (member class (split-string (edom-getattr element 'class ""))))

;; DOM methods

(defun edom-by-tag-name (node tag-name) ; getElementsByTagName
  "Return descendents of NODE whose tag is named TAG-NAME."
  (let ((elements '()))
    (dolist (child (xml-node-children node))
      (when (edom-element-p child)
        (when (eq (xml-node-name child) tag-name)
          (push child elements))
        (setq elements
              (append elements (edom-by-tag-name child tag-name)))))
    elements))

(defun edom-by-class-name (node &rest class-names) ; getElementsByClassName
  "Return descendents of NODE which have all classes named in CLASS-NAMES."
  (let ((elements '()))
    (dolist (child (xml-node-children node))
      (when (edom-element-p child)
        (when (reduce (lambda (a b) (and a b))
                      (mapcar (lambda (class-name)
                                (edom-has-class-p child class-name))
                              class-names))
          (push child elements))
        (setq elements
              (append elements
                      (apply 'edom-by-class-name child class-names)))))
    elements))

(defun edom-text-content (node) ; textContent
  "Return the text content of NODE."
  (let ((acc '()))
    (dolist (child (xml-node-children node))
      (cond ((stringp child)
             (push child acc))
            ((edom-element-p child)
             (push (edom-text-content child) acc))))
    (mapconcat 'identity (nreverse acc) "\n")))

(defun edom-by-id (node id) ; getElementById
  "Return the descendent of NODE which has ID, if it exists."
  (catch 'found
    (edom-by-id-1 node id)))

(defun edom-by-id-1 (node id)
  "Throw the descendent of NODE which has ID, if it exists."
  (dolist (child (xml-node-children node))
    (when (edom-element-p child)
      (when (equal (edom-getattr child 'id) id)
        (throw 'found child))
      (edom-by-id-1 child id)))
  nil)

(provide 'edom)
;;; edom.el ends here
