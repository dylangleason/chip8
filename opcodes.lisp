;;;; opcodes.lisp

(in-package #:chip8)

(defun cls (emulator)
  "Clear the screen will clear the EMULATOR display."
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
  "Skip next instruction if register x = nn. Set the EMULATOR program
counter if the check evaluates to true."
  (when (vx-equal-nn-p emulator operands)
    (incf (slot-value emulator 'pc) 2)))

(defmethod se ((emulator chip8) (operands xyn))
  "Skip next instruction if register x = register y. Set the EMULATOR
program counter if the check evaluates to true."
  (when (vx-equal-vy-p emulator operands)
    (incf (slot-value emulator 'pc) 2)))

(defgeneric sne (emulator operands)
  (:documentation "Skip an EMULATOR instruction if not equal to a value."))

(defmethod sne ((emulator chip8) (operands xnn))
  "Skip next EMULATOR instruction if register X != NN."
  (when (not (vx-equal-nn-p emulator operands))
    (incf (slot-value emulator 'pc) 2)))

(defmethod sne ((emulator chip8) (operands xyn))
  "Skip next EMULATOR instruction if register X != register Y."
  (when (not (vx-equal-vy-p emulator operands))
    (incf (slot-value emulator 'pc) 2)))

(defgeneric ld (emulator operands prefix)
  (:documentation "Load a value into an EMULATOR register."))

(defmethod ld ((emulator chip8) (operands xnn) (prefix (eql #x6)))
  "Set the EMULATOR variable register at address X to a byte NN."
  (with-slots (v) emulator
    (setf (aref v (xnn-x operands)) (xnn-nn operands))))

(defmethod ld ((emulator chip8) (operands xyn) (prefix (eql #x8)))
  "Set the EMULATOR register V at addres X to the value of register V at
address Y."
  (with-slots (v) emulator
    (setf (aref v (xyn-x operands))
	  (aref v (xyn-y operands)))))

(defgeneric add (emulator operands prefix)
  (:documentation "Add two OPERANDS and store in an EMULATOR register."))

(defmethod add ((emulator chip8) (operands xnn) (prefix (eql #x7)))
  "Set the EMULATOR register V at address X to the sum of the
value stored at address X and byte NN."
  (incf (aref (slot-value emulator 'v) (xnn-x operands))
	(xnn-nn operands)))

(defmethod add ((emulator chip8) (operands xyn) (prefix (eql #x8)))
  "Set the EMULATOR variable register X to the sum of the
values stored in registers X and Y. If the result is greater than 8
bits, set VF to 1, indicating the carry flag is set, otherwise set it
to 0. Only the lowest bits of the result are kept and stored in
register X."
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
  "Set the EMULATOR variable register X to the difference of values stored in
registers X and Y. If the value in register X is greater than register
Y, set VF to 1, indicating the borrow flag is not set. Otherwise set
to 0."
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
  "If the least-significant bit of Vx is 1, then set VF to 1, otherwise
set to 0. Then divide Vx by 2."
  (with-slots (v) emulator
    (let* ((x (xyn-x operands))
	   (y (xyn-y operands))
	   (val-x (aref (v emulator) x)))
      (setf (aref (v emulator) #xF)
	    (if (= 1 (logand 1 val-x)) 1 0))
      (setf (aref (v emulator) x)
	    (ash x -1)))))

(defun subn (emulator operands)
  "Set EMULATOR register VF to 1 if Vy is greater than Vx, otherwise set
to 0. Then subtract Vx from Vy and store results in Vx."
  ;; TODO
  )

(defun shl (emulator operands)
  ;; TODO
  )

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
