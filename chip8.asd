;;;; chip8.asd

(asdf:defsystem #:chip8
  :description "Emulator for the Chip-8 architecture."
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2)
  :components ((:file "package")
	       (:file "bitwise")
	       (:file "renderer")
               (:file "chip8")
	       (:file "opcodes")))

(asdf:defsystem #:chip8/tests
  :depends-on (#:rove)
  :components ((:module "tests"
		:serial t
		:components ((:file "package")
			     (:file "chip8")))))
