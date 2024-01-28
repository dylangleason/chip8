;;;; bitwise.lisp

(in-package #:chip8)

(defun clear-lo-nibble (byte)
  "Mask the lo nibble of BYTE off."
  (logand byte #xF0))

(defun clear-hi-nibble (byte)
  "Mask the high nibble of BYTE off."
  (logand byte #xF))

(defun clear-lo-byte (word)
  "Mask the high byte of WORD off."
  (logand word (mask-hi-nibbles-on 2)))

(defun clear-hi-byte (word)
  "Mask the high byte of WORD off."
  (logand word (mask-lo-nibbles-on 2)))

(defun enable-after-hi-nibble (word n)
  "Enable nibbles in big endian order from WORD starting at N."
  (logand word (mask-hi-nibbles-on n)))

(defun enable-after-lo-nibble (word n)
  "Enable nibbles in little endian order from WORD starting at N."
  (logand word (mask-lo-nibbles-on n)))

(defun read-lo-nibble (byte)
  "Read the lo nibble of BYTE."
  (clear-hi-nibble byte))

(defun read-hi-nibble (byte)
  "Read the high nibble of BYTE."
  (ash (clear-lo-nibble byte) -4))

(defun read-lo-byte (word)
  "Read the low byte of WORD."
  (clear-hi-byte word))

(defun read-hi-byte (word)
  "Read the high byte of WORD."
  (ash (clear-lo-byte word) -8))

(defun read-nibble (word n)
  "Read nibble N from a 16-bit WORD."
  (let ((offset (offset-to-hi-nibble n)))
    (ash (logand word (ash #xF offset)) (- offset))))

(defun mask-lo-nibbles-on (n)
  "Compute mask to turn on first N least significant nibbles."
  (let ((offset 0) (mask #xF))
    (dotimes (i n)
      (setf mask (logior (ash #xF offset) mask))
      (incf offset 4))
    mask))

(defun mask-hi-nibbles-on (n)
  "Compute mask to turn on first N most significant nibbles."
  (let ((mask #xF000))
    (dotimes (i n)
      (let ((offset (offset-to-hi-nibble (+ i 1))))
	(setf mask (logior (ash #xF offset) mask))
	(decf offset 4)))
    mask))

(defun format-nibble (value)
  "Format an integer VALUE expressed as a half byte."
  (format-binary value 4))

(defun format-byte (value)
  "Format an integer VALUE expressed as a single byte."
  (format-binary value 8))

(defun format-word (value)
  "Format an integer VALUE expressed as two bytes."
  (format-binary value 16))

(defun format-binary (value n)
  (format nil "~v,'0b" n value))

(defun offset-to-hi-nibble (n)
  (- 16 (* n 4)))
