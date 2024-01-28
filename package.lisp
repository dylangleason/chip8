;;;; package.lisp

(defpackage #:chip8
  (:use #:cl)
  (:export :chip8
	   :clear
	   :cycle
	   :*instructions-per-update*
           :load-program
	   :pc
	   :memory
	   :stack
	   :v))
