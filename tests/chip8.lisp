(in-package #:chip8/tests)

(defstruct test-display
  message)

(defmethod clear ((display test-display))
  (setf (test-display-message display) 'display-cleared))

(defvar *test-cpu* nil
  "Emulated CPU under test.")

(defvar *test-display* nil
  "Test double for the display.")

(defvar *test-program* nil
  "Test program to simulate an emulated instruction.")

(defun init ()
  (setf *test-display* (make-test-display))
  (setf *test-cpu* (make-instance 'chip8 :display *test-display*)))

(defhook :before (init))

(deftest test-call
  (testing "calls a subroutine"
    (let ((chip8::*instructions-per-update* 1)
	  (current-pc (chip8::pc *test-cpu*)))
      (chip8::load-program *test-cpu* #(#x21 #x0A))
      (chip8::cycle *test-cpu*)
      (ok (equalp (vector (+ current-pc 2)) (chip8::stack *test-cpu*))))))

(deftest test-cls
  (testing "clears the display"
    (let ((chip8::*instructions-per-update* 1))
      (chip8::load-program *test-cpu* #(#x00 #xE0))
      (chip8::cycle *test-cpu*)
      (ok (eq 'display-cleared (test-display-message *test-display*))))))

(deftest test-ld
  (testing "load byte into variable register x"
    (let ((chip8::*instructions-per-update* 1))
      (chip8::load-program *test-cpu* #(#x60 #x1A))
      (chip8::cycle *test-cpu*)
      (ok (eq #x1A (aref (chip8::v *test-cpu*) 0)))))

  (init)

  (testing "load value of vy into vy"
    (let ((chip8::*instructions-per-update* 3))
      (chip8::load-program
       *test-cpu* #(#x62 #x1A
		    #x65 #x40
		    #x82 #x50))
      (chip8::cycle *test-cpu*)
      (ok (eq #x40 (aref (chip8::v *test-cpu*) 2))))))

(deftest test-ret
  (testing "return from a subroutine"
    (let ((chip8::*instructions-per-update* 2)
	  (current-pc (chip8::pc *test-cpu*)))
      (chip8::load-program *test-cpu* #(#x22 #x02 #x00 #xEE)) ; subroutine immediately returns
      (chip8::cycle *test-cpu*)
      (ok (eq (+ current-pc 2) (chip8::pc *test-cpu*))))))

(deftest test-se
  (testing "skips next instruction when vx = nn"
    (let ((chip8::*instructions-per-update* 2)
	  (current-pc (chip8::pc *test-cpu*)))
      (chip8::load-program
       *test-cpu* #(#x60 #x1F   ; load x1F into v[0]
		    #x30 #x1F   ; skip if v[0] = x1F 
		    #x12 #x00   ; jmp
		    #x00 #xE0)) ; cls
      (chip8::cycle *test-cpu*)
      (ok (eq (+ current-pc 6) (chip8::pc *test-cpu*)))))

  (init)

  (testing "skips next instruction when vs = vy"
    (let ((chip8::*instructions-per-update* 3)
	  (current-pc (chip8::pc *test-cpu*)))
      (chip8::load-program
       *test-cpu* #(#x60 #x1F   ; load x1F into x
		    #x61 #x1F   ; load x1F into y
		    #x50 #x10   ; skip if vx, vy
		    #x12 #x00   ; jmp
		    #x00 #xE0)) ; cls
      (chip8::cycle *test-cpu*)
      (ok (eq (+ current-pc 8) (chip8::pc *test-cpu*))))))

(deftest test-sne
  (testing "skip next instruction when vx != nn"
    (let ((chip8::*instructions-per-update* 2)
	  (current-pc (chip8::pc *test-cpu*)))
      (chip8::load-program
       *test-cpu* #(#x6B #x1F
		    #x40 #x2A
		    #x12 #x00
		    #x00 #xE0))
      (chip8::cycle *test-cpu*)
      (ok (eq (+ current-pc 6) (chip8::pc *test-cpu*))))))

(deftest test-add
  (testing "add value in vx to nn and store back to vx"
    (let ((chip8::*instructions-per-update* 2))
      (chip8::load-program
       *test-cpu* #(#x60 #xA   ; load #xA into v[0]
		    #x70 #x4)) ; add #x4 value stored in v[0] and set
      (chip8::cycle *test-cpu*)
      (ok (eq 14 (aref (chip8::v *test-cpu*) 0)))))

  (init)

  (testing "add vx to vy, where sum > 255, sets carry flag"
    (let ((chip8::*instructions-per-update* 3))
      (chip8::load-program
       *test-cpu* #(#x60 #xFF   ; load #xFF into v[0]
		    #x61 #x1F   ; load #x1F into v[1]
		    #x80 #x14)) ; add values stored in v[0] and v[1]
      (chip8::cycle *test-cpu*)
      (ok (eq 1 (aref (chip8::v *test-cpu*) #xF)))))

  (init)

  (testing "add vx to vy, where sum <= 255, unsets carry flag"
    (let ((chip8::*instructions-per-update* 3))
      (chip8::load-program
       *test-cpu* #(#x60 #xF0   ; load #xF0 into v[0]
		    #x61 #xE    ; load #xE into v[1]
		    #x80 #x14)) ; add values stored in v[0] and v[1]
      (chip8::cycle *test-cpu*)
      (ok (eq 0 (aref (chip8::v *test-cpu*) #xF))))))

(deftest test-or
  (testing "calculate bitwise OR of vx and vy"
    (let ((chip8::*instructions-per-update* 3))
      (chip8::load-program
       *test-cpu* #(#x60 #x1
		    #x61 #x2
		    #x80 #x11))
      (chip8::cycle *test-cpu*)
      (ok (eq 3 (aref (chip8::v *test-cpu*) 0))))))

(deftest test-and
  (testing "calculate bitwise AND of vx and vy"
    (let ((chip8::*instructions-per-update* 3))
      (chip8::load-program
       *test-cpu* #(#x60 #x1
		    #x61 #x2
		    #x80 #x12))
      (chip8::cycle *test-cpu*)
      (ok (eq 0 (aref (chip8::v *test-cpu*) 0))))))

(deftest test-xor
  (testing "calculate bitwise XOR of vx and vy"
    (let ((chip8::*instructions-per-update* 3))
      (chip8::load-program
       *test-cpu* #(#x60 #x1
		    #x61 #x3
		    #x80 #x13))
      (chip8::cycle *test-cpu*)
      (ok (eq 2 (aref (chip8::v *test-cpu*) 0))))))

(deftest test-sub
  (testing "subtract value in vx by vy and store back to vx"
    (let ((chip8::*instructions-per-update* 3))
      (chip8::load-program
       *test-cpu* #(#x60 #xFF   ; load #xFF into v[0]
		    #x61 #xDF   ; load #xDF into v[1]
		    #x80 #x15)) ; sub values stored in v[0] and v[1]
      (chip8::cycle *test-cpu*)
      (ok (eq 32 (aref (chip8::v *test-cpu*) 0)))))

  (init)

  (testing "subtract value in vx by vy and set borrow flag to 1"
    (let ((chip8::*instructions-per-update* 3))
      (chip8::load-program
       *test-cpu* #(#x60 #xFF
		    #x61 #xDF
		    #x80 #x15))
      (chip8::cycle *test-cpu*)
      (ok (eq 1 (aref (chip8::v *test-cpu*) #xF)))))

  (init)

  (testing "subtract value in vx by vy and set borrow flag to 0"
    (let ((chip8::*instructions-per-update* 3))
      (chip8::load-program
       *test-cpu* #(#x60 #xDF
		    #x61 #xFF
		    #x80 #x15))
      (chip8::cycle *test-cpu*)
      (ok (eq 0 (aref (chip8::v *test-cpu*) #xF))))))

(deftest subn
  (testing "subtract value in vy by vx and store back to vx"
    (let ((chip8::*instructions-per-update* 3))
      (chip8::load-program
       *test-cpu* #(#x60 #xDF
		    #x61 #xFF
		    #x80 #x17))
      (chip8::cycle *test-cpu*)
      (ok (eq 32 (aref (chip8::v *test-cpu*) 0)))))

  (init)

  (testing "subtract value in vy by vx and set borrow flag to 0"
    (let ((chip8::*instructions-per-update* 3))
      (chip8::load-program
       *test-cpu* #(#x60 #xFF
		    #x61 #xDF
		    #x80 #x17))
      (chip8::cycle *test-cpu*)
      (ok (eq 0 (aref (chip8::v *test-cpu*) #xF)))))

  (init)

  (testing "subtract value in vy by vx and set borrow flag to 1"
    (let ((chip8::*instructions-per-update* 3))
      (chip8::load-program
       *test-cpu* #(#x60 #xDF
		    #x61 #xFF
		    #x80 #x17))
      (chip8::cycle *test-cpu*)
      (ok (eq 1 (aref (chip8::v *test-cpu*) #xF))))))

(deftest test-shr
  (testing "shift value in vx to the right by 1"
    (let ((chip8::*instructions-per-update* 2))
      (chip8::load-program
       *test-cpu* #(#x60 #xFF
		    #x80 #x06))
      (chip8::cycle *test-cpu*)
      (ok (eq #x7F (aref (chip8::v *test-cpu*) 0))))))

(deftest test-shl
  (testing "shift value in vx to the left by 1"
    (let ((chip8::*instructions-per-update* 2))
      (chip8::load-program
       *test-cpu* #(#x60 #x7F
		    #x80 #x0E))
      (chip8::cycle *test-cpu*)
      (ok (eq #xFE (aref (chip8::v *test-cpu*) 0))))))
