;;;; chip8.lisp

(in-package #:chip8)

(defconstant +max-memory+ 4096
  "Maximum memory capacity, specified in bytes.")

(defconstant +max-stack+ 16
  "Maximum number of stack subroutines.")

(defconstant +num-registers+ 16
  "Number of variable registers.")

(defconstant +program-start-address+ #x200
  "Address in memory where program starts.")

(defconstant +updates-per-cycle+ (/ 1.0 60)
  "Rate at which the delay timer decrements and instructions are executed.")

(defvar *instructions-per-update* 12)

(defvar *sprites*
  '(#xF0 #x90 #x90 #x90 #xF0		; 0
    #x20 #x60 #x20 #x20 #x70		; 1
    #xF0 #x10 #xF0 #x80 #xF0		; 2
    #xF0 #x10 #xF0 #x10 #xF0		; 3
    #x90 #x90 #xF0 #x10 #x10		; 4
    #x20 #x60 #x20 #x20 #x70		; 5
    #x20 #x60 #x20 #x20 #x70		; 6
    #x20 #x60 #x20 #x20 #x70		; 7
    #x20 #x60 #x20 #x20 #x70		; 8
    #x20 #x60 #x20 #x20 #x70		; 9
    #x20 #x60 #x20 #x20 #x70		; A
    #x20 #x60 #x20 #x20 #x70		; B
    #x20 #x60 #x20 #x20 #x70		; C
    #x20 #x60 #x20 #x20 #x70		; D
    #x20 #x60 #x20 #x20 #x70		; E
    #x20 #x60 #x20 #x20 #x70)		; F
  "Sprites used for rendering fonts to the display. Sprites
are 4 pixels wide by 5 pixels tall. Each pixel is stored as a single
byte, with the least significant nibble of the byte used for padding.")

(defclass chip8 ()
  ((v
    :type array
    :reader v
    :initform (make-array +num-registers+ :element-type '(unsigned-byte 8))
    :documentation
    "General purpose registers V0-VF.")
   (i
    :type (typexpand '(unsigned-byte 16))
    :initform 0
    :documentation
    "Index register used to point to locations in memory.")
   (dt
    :type (typexpand '(unsigned-byte 8))
    :initform 0
    :documentation
    "Delay timer that decrements until it reaches zero.")
   (st
    :type (typexpand '(unsigned-byte 8))
    :initform 0
    :documentation
    "Sound timer that beeps when set to a non-zero value.")
   (pc
    :type (typexpand '(unsigned-byte 16))
    :reader pc
    :initform +program-start-address+
    :documentation
    "Program counter points to the current instruction in memory.")
   (memory
    :type array
    :reader memory
    :initform
    (make-array +max-memory+ :element-type '(unsigned-byte 8))
    :documentation
    "Available memory. The first 512 bytes are reserved for the interpreter.")
   (stack
    :type array
    :reader stack
    :initform
    (make-array +max-stack+ :element-type '(unsigned-byte 16) :fill-pointer 0)
    :documentation
    "Stack data structure used to store address of last subroutine.")
   (display
    :initarg :display
    :reader display
    :documentation
    "Display to render the emulator output.")))

(defun load-sprites (emulator)
  "Load sprite data defined by *SPRITES* into the memory segment reserved
for the EMULATOR interpreter. By convention, the sprite data is stored
starting at address #x50."
  (with-slots (memory) emulator
    (loop for d in *sprites*
	  for i from #x50
	  do (setf (aref memory i) d))))

(defun load-program (emulator data)
  "Load a program DATA into the EMULATOR memory."
  (with-slots (memory) emulator
    (loop for d across data
	  for i from 0
	  do (setf (aref memory (+ i +program-start-address+)) d))))

(defun cycle (emulator)
  "Fetch, decode and execute a number of EMULATOR instructions."
  (dotimes (i *instructions-per-update*)
    (let ((instruction (fetch emulator)))
      (cond ((= #x00E0 instruction) (cls emulator))
	    ((= #x00EE instruction) (ret emulator))
	    (t (execute emulator instruction))))))

(defun fetch (emulator)
  "Fetch will read the instruction from the address in EMULATOR memory pointed to
by the program counter, then increment the program counter by 2 bytes."
 (with-slots (memory pc) emulator
    (let ((b1 (aref memory pc))
	  (b2 (aref memory (+ pc 1))))
      (incf pc 2)
      (logior (ash b1 8) b2))))

(defun execute (emulator instruction)
  "Execute an INSTRUCTION for the EMULATOR."
  (ccase (read-nibble instruction 1)
    ((#x1) (jp emulator (create-nnn instruction)))
    ((#x2) (call emulator (create-nnn instruction)))
    ((#x3) (se emulator (create-xnn instruction)))
    ((#x4) (sne emulator (create-xnn instruction)))
    ((#x5) (se emulator (create-xyn instruction)))
    ((#x6) (ld emulator (create-xnn instruction) #x6))
    ((#x7) (add emulator (create-xnn instruction) #x7))
    ((#x8) (execute-8 emulator instruction))
    ((#x9) (sne emulator (create-xyn instruction)))
    ((#xA) (ld emulator (create-nnn instruction) #xA))
    ((#xB) (jp emulator (create-nnn instruction)))
    ((#xC) (rnd emulator (create-xnn instruction)))
    ((#xD) (drw emulator (create-xyn instruction)))
    ((#xE) (execute-e emulator instruction))
    ((#xF) (execute-f emulator instruction))))

(defun execute-8 (emulator instruction)
  (let ((op (create-xyn instruction)))
    (ccase (xyn-n op)
      ((#x0) (ld emulator op #x8))
      ((#x1) (bor emulator op))
      ((#x2) (band emulator op))
      ((#x3) (bxor emulator op))
      ((#x4) (add emulator op #x8))
      ((#x5) (sub emulator op))
      ((#x6) (shr emulator op))
      ((#x7) (subn emulator op))
      ((#xE) (shl emulator op)))))

(defun execute-e (emulator instruction)
  (let ((op (create-xnn instruction)))
    (ccase (xnn-nn op)
      ((#xA1) (sknp emulator op))
      ((#x9E) (skp emulator op)))))

(defun execute-f (emulator instruction)
  (let ((op (create-xnn instruction)))
    (ccase (xnn-nn op)
      ((#x07) (sknp emulator op))
      ((#x0A) (skp emulator op))
      ((#x15) (skp emulator op))
      ((#x18) (skp emulator op))
      ((#x1E) (skp emulator op))
      ((#x29) (skp emulator op))
      ((#x33) (skp emulator op))
      ((#x55) (skp emulator op))
      ((#x65) (skp emulator op))
      )))

(defstruct xyn
  "Opcode where X and Y are addresses in the variable register, and N is
the remaining nibble of data."
  x y n)

(defun create-xyn (word)
  "Create an xyn instruction given a WORD."
  (make-xyn
   :x (read-nibble word 2)
   :y (read-nibble word 3)
   :n (read-nibble word 4)))

(defstruct xnn
  "Opcode where X is an address in the variable register, and NN is the
remaining byte of data."
  x nn)

(defun create-xnn (word)
  "Create an xnn instruction given a WORD."
  (make-xnn
   :x (read-nibble word 2)
   :nn (read-lo-byte word)))

(defun create-nnn (word)
  "Create an nnn instruction given a WORD."
  (enable-after-lo-nibble word 3))
