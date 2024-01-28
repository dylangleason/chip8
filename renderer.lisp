;;;; renderer.lisp

(in-package #:chip8)

(defgeneric clear (display)
  (:documentation "Clear the emulator output"))
