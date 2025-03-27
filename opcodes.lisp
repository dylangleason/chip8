;;;; opcodes.lisp

(in-package #:chip8)

(defun cls (emulator)
  "Clear the screen. Clears the EMULATOR display."
  (with-slots (display) emulator
    (clear display)))

(defun ret (emulator)
  "Return from subroutine. Pops an element from the EMULATOR stack and
assigns it to the program counter."
  (with-slots (pc stack) emulator
    (setf pc (vector-pop stack))))

(defun jp (emulator nnn)
  "Jump to an instruction at address NNN by setting EMULATOR program
counter to it."
  (setf (slot-value emulator 'pc) nnn))

(defun call (emulator nnn)
  "Call an EMULATOR subroutine at memory location NNN. Push the current
program counter to the stack, then set the program counter to NNN."
  (with-slots (pc stack) emulator
    (vector-push pc stack)
    (setf pc nnn)))

(defgeneric se (emulator operands))

(defmethod se ((emulator chip8) (operands xnn))
  "Given OPERANDS x and nn, skip next instruction if value at Vx equals
nn. Set the EMULATOR program counter if the check evaluates to true."
  (when (vx-equal-nn-p emulator operands)
    (incf (slot-value emulator 'pc) 2)))

(defmethod se ((emulator chip8) (operands xyn))
  "Given OPERANDS x and y, skip next instruction if value at Vx equals
value at Vy. Set the EMULATOR program counter if the check evaluates
to true."
  (when (vx-equal-vy-p emulator operands)
    (incf (slot-value emulator 'pc) 2)))

(defgeneric sne (emulator operands)
  (:documentation "Skip an EMULATOR instruction if two values specified by OPERANDS are
not equal."))

(defmethod sne ((emulator chip8) (operands xnn))
  "Given OPERANDS x and nn, skip next EMULATOR instruction if value at Vx
does not equal nn."
  (when (not (vx-equal-nn-p emulator operands))
    (incf (slot-value emulator 'pc) 2)))

(defmethod sne ((emulator chip8) (operands xyn))
  "Given OPERANDS x and y, skip next EMULATOR instruction if value at Vx
does not equal value at Vy."
  (when (not (vx-equal-vy-p emulator operands))
    (incf (slot-value emulator 'pc) 2)))

(defgeneric ld (emulator operands prefix)
  (:documentation "Load a value into an EMULATOR register."))

(defmethod ld ((emulator chip8) (operands xnn) (prefix (eql #x6)))
  "Given OPERANDS xnn, set the EMULATOR value at Vx to byte nn."
  (with-slots (v) emulator
    (setf (aref v (xnn-x operands)) (xnn-nn operands))))

(defmethod ld ((emulator chip8) (operands xyn) (prefix (eql #x8)))
  "Given OPERANDS xyn, set the EMULATOR value at Vx to the value of Vy."
  (with-slots (v) emulator
    (setf (aref v (xyn-x operands))
	  (aref v (xyn-y operands)))))

(defgeneric add (emulator operands prefix)
  (:documentation "Add two OPERANDS and store in an EMULATOR register."))

(defmethod add ((emulator chip8) (operands xnn) (prefix (eql #x7)))
  "Given OPERANDS xnn, set the EMULATOR value at Vx to the sum of itself
and byte nn."
  (incf (aref (slot-value emulator 'v) (xnn-x operands))
	(xnn-nn operands)))

(defmethod add ((emulator chip8) (operands xyn) (prefix (eql #x8)))
  "Given OPERANDS, add values of EMULATOR registers Vx and Vy, writing
sum back to Vx. If the sum is larger than 8 bits, set EMULATOR
register at VF to 1, indicating the carry flag is set, otherwise set
it to 0. Only the lowest bits of the result are kept and stored in Vx."
  (with-slots (v) emulator
    (let* ((x (xyn-x operands))
	   (y (xyn-y operands))
	   (sum (+ (aref (v emulator) x)
		   (aref (v emulator) y))))
      (setf (aref (v emulator) x)
	    (clear-hi-byte sum))
      (setf (aref (v emulator) #xF)
	    (if (> sum #xFF) 1 0)))))

(defun sub (emulator operands)
  "Given OPERANDS, subract values of EMULATOR registers Vx and Vy,
writing difference back to Vx. If the value in Vx is greater than
register Vy, set EMULATOR register at VF to 1, indicating the borrow
flag is not set. Otherwise set to 0."
  (with-slots (v) emulator
    (let* ((x (xyn-x operands))
	   (y (xyn-y operands))
	   (val-x (aref (v emulator) x))
	   (val-y (aref (v emulator) y)))
      (setf (aref (v emulator) #xF)
	    (if (> val-x val-y) 1 0))
      (setf (aref (v emulator) x)
	    (abs (- val-x val-y))))))

(defun shr (emulator operands)
  "Given OPERANDS, shift value of EMULATOR register Vx to the right by
1. If the least-significant bit of value at Vx is 1, then set VF to 1,
otherwise set to 0. Then divide value at Vx by 2."
  (with-slots (v) emulator
    (let* ((x (xyn-x operands))
	   ;; TODO: in the below implementation, the value of Y is
	   ;; ignored. Consider optionally setting the value of Vx to
	   ;; Vy, and making it configurable, as some implementations
	   ;; rely on this behavior. See:
	   ;; https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#8xy6-and-8xye-shift
	   (val-x (aref (v emulator) x)))
      (setf (aref (v emulator) #xF)
	    (if (= 1 (read-byte-lsb val-x)) 1 0))
      (setf (aref (v emulator) x)
	    (ash val-x -1)))))

(defun subn (emulator operands)
  "Set EMULATOR register VF to 1 if Vy is greater than Vx, otherwise set
to 0. Then subtract Vx from Vy and store results in Vx."
  (with-slots (v) emulator
    (let* ((x (xyn-x operands))
	   (y (xyn-y operands))
	   (val-x (aref (v emulator) x))
	   (val-y (aref (v emulator) y)))
      (setf (aref (v emulator) #xF)
	    (if (> val-y val-x) 1 0))
      (setf (aref (v emulator) x)
	    (abs (- val-y val-x))))))

(defun shl (emulator operands)
  "Given OPERANDS, shift value of EMULATOR register Vx to the left by
1. If the most significant-bit of value at Vx is 1, then set VF to 1,
otherwise set to 0. Then multiply value at Vx by 2."
  (with-slots (v) emulator
    (let* ((x (xyn-x operands))
	   (y (xyn-y operands))
	   (val-x (aref (v emulator) x)))
      (setf (aref (v emulator) #xF)
	    (if (= 1 (read-byte-msb val-x)) 1 0))
      (setf (aref (v emulator) x)
	    (ash val-x 1)))))

(defun bor (emulator operands)
  "Set the EMULATOR register V at address X to the bitwise OR of the
values stored at register addresses X and Y."
  (bitwise-op emulator operands #'logior))

(defun band (emulator operands)
  "Set the EMULATOR register V at address X to the bitwise AND of the
values stored at register addresses X and Y."
  (bitwise-op emulator operands #'logand))

(defun bxor (emulator operands)
  "Set the EMULATOR register V at address X to the bitwise XOR of the
values stored at register addresses X and Y."
  (bitwise-op emulator operands #'logxor))

(defun bitwise-op (emulator operands f)
  (with-slots (v) emulator
    (let ((x (xyn-x operands))
	  (y (xyn-y operands)))
      (setf (aref v x) (funcall f (aref v x) (aref v y))))))

(defun vx-equal-nn-p (emulator xnn)
  (eql (aref (slot-value emulator 'v) (xnn-x xnn))
       (xnn-nn xnn)))

(defun vx-equal-vy-p (emulator xyn)
  (eql (aref (v emulator) (xyn-x xyn))
       (aref (v emulator) (xyn-y xyn))))
