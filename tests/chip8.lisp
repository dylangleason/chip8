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
    (let ((*instructions-per-update* 1)
	  (current-pc (pc *test-cpu*)))
      (load-program *test-cpu* #(#x21 #x0A))
      (cycle *test-cpu*)
      (ok (equalp (vector (+ current-pc 2)) (stack *test-cpu*))))))

(deftest test-cls
  (testing "clears the display"
    (let ((*instructions-per-update* 1))
      (load-program *test-cpu* #(#x00 #xE0))
      (cycle *test-cpu*)
      (ok (eq 'display-cleared (test-display-message *test-display*))))))

(deftest test-ld
  (testing "load byte into variable register x"
    (let ((*instructions-per-update* 1))
      (load-program *test-cpu* #(#x60 #x1A))
      (cycle *test-cpu*)
      (ok (eq #x1A (aref (v *test-cpu*) 0)))))

  (init)

  (testing "load value of vy into vy"
    (let ((*instructions-per-update* 3))
      (load-program
       *test-cpu* #(#x62 #x1A
		    #x65 #x40
		    #x82 #x50))
      (cycle *test-cpu*)
      (ok (eq #x40 (aref (v *test-cpu*) 2))))))

(deftest test-ret
  (testing "return from a subroutine"
    (let ((*instructions-per-update* 2)
	  (current-pc (pc *test-cpu*)))
      (load-program *test-cpu* #(#x22 #x02 #x00 #xEE)) ; subroutine immediately returns
      (cycle *test-cpu*)
      (ok (eq (+ current-pc 2) (pc *test-cpu*))))))

(deftest test-se
  (testing "skips next instruction when vx = nn"
    (let ((*instructions-per-update* 2)
	  (current-pc (pc *test-cpu*)))
      (load-program
       *test-cpu* #(#x60 #x1F   ; load x1F into v[0]
		    #x30 #x1F   ; skip if v[0] = x1F 
		    #x12 #x00   ; jmp
		    #x00 #xE0)) ; cls
      (cycle *test-cpu*)
      (ok (eq (+ current-pc 6) (pc *test-cpu*)))))

  (init)

  (testing "skips next instruction when vs = vy"
    (let ((*instructions-per-update* 3)
	  (current-pc (pc *test-cpu*)))
      (load-program
       *test-cpu* #(#x60 #x1F   ; load x1F into x
		    #x61 #x1F   ; load x1F into y
		    #x50 #x10   ; skip if vx, vy
		    #x12 #x00   ; jmp
		    #x00 #xE0)) ; cls
      (cycle *test-cpu*)
      (ok (eq (+ current-pc 8) (pc *test-cpu*))))))

(deftest test-sne
  (testing "skip next instruction when vx != nn"
    (let ((*instructions-per-update* 2)
	  (current-pc (pc *test-cpu*)))
      (load-program
       *test-cpu* #(#x6B #x1F
		    #x40 #x2A
		    #x12 #x00
		    #x00 #xE0))
      (cycle *test-cpu*)
      (ok (eq (+ current-pc 6) (pc *test-cpu*))))))

(deftest test-add
  (testing "add value in vx to nn and store back to vx"
    (let ((*instructions-per-update* 2))
      (load-program
       *test-cpu* #(#x60 #xA   ; load #xA into v[0]
		    #x70 #x4)) ; add #x4 value stored in v[0] and set
      (cycle *test-cpu*)
      (ok (eq 14 (aref (v *test-cpu*) 0)))))

  (init)

  (testing "add vx to vy, where sum > 255, sets carry flag"
    (let ((*instructions-per-update* 3))
      (load-program
       *test-cpu* #(#x60 #xFF   ; load #xFF into v[0]
		    #x61 #x1F   ; load #x1F into v[1]
		    #x80 #x14)) ; add values stored in v[0] and v[1]
      (cycle *test-cpu*)
      (ok (eq 1 (aref (v *test-cpu*) #xF)))))

  (init)

  (testing "add vx to vy, where sum <= 255, unsets carry flag"
    (let ((*instructions-per-update* 3))
      (load-program
       *test-cpu* #(#x60 #xF0   ; load #xFF into v[0]
		    #x61 #xE    ; load #xE into v[1]
		    #x80 #x14)) ; add values stored in v[0] and v[1]
      (cycle *test-cpu*)
      (ok (eq 0 (aref (v *test-cpu*) #xF))))))
