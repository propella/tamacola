;; Library boot loader
;;
;; This file should be loaded before all other library files.  It
;; defines minimal necessary functions to load other library in the
;; library path *load-path*. Some functions may be defined later in
;; cleaner way in other libraries.
;;
;; Function names are took from test.k
;; Other function signature are borrowed from various LISPs
;;
;; [r6rs], [r6rs-lib] : R6RS and R6RS library standard
;; [r5rs] : R5RS
;; [plt] : PLT Scheme
;; [srfi-?] : SRFI
;; [gauche] : Gauche
;; (-) denotes subset or deviation

;;;; Systam calls

(define exit		(extern "exit"		'(-> void int)))

(define fopen		(extern "fopen"		'(-> (* (struct FILE))	(* char) (* char))))
(define fclose		(extern "fclose"	'(-> int 		(* (struct FILE)))))

(define errno		(extern "errno"		'(* int)))
(define strerror	(extern "strerror"	'(-> (* char) int)))

(define strlen		(extern "strlen"	'(-> int		(* char))))
(define strcpy		(extern "strcpy"	'(-> (* char)		(* char) (* char))))
(define memcpy		(extern "memcpy"	'(-> (* void)		(* void) (* void) long)))

;;;; Rename primitive subr

(define _peek peek)
(define peek ())

;;;; List functions

(define list (lambda args args))

(define constant (form (lambda (c f expr) (list 'quote (eval expr global)))))
(define pair?	(lambda (expr) (= (typeof expr) (constant (typeof '(1))))))

(define car	(lambda (expr)		(and (pair? expr) (    tuple-at expr 0    ))))
(define cdr	(lambda (expr)		(and (pair? expr) (    tuple-at expr 1    ))))

(define assert
  (form
   (lambda (compiler form expr)
     (list 'or expr
	   (list 'let ()
		 (list 'princ "assertion failed: " (list 'quote expr) "\n")
		 (list 'exit 1))))))

(define memq?
  (lambda (key list)
    (and (pair? list)
	 (if (== key (car list))
	     list
	     (memq? key (cdr list))))))

(define fold-left ;; [r6rs-lib(-)]
  (lambda (func z lst)
    (if (pair? lst)
	(fold-left func (func z (car lst)) (cdr lst))
	z)))

(assert (= '(((((() . 1) . 2) . 3) . 4) . 5) (fold-left cons '() '(1 2 3 4 5))))

(define exists ;; [r6rs-lib(-)]
  (lambda (func ls)
    (and (pair? ls)
	(or (func (car ls))
	    (exists func (cdr ls))))))

(assert (= "here" (exists (lambda (x) (and (= x "here") x)) '(1 () "here" 3.14))))

;;;; String functions

(define string-ref ;; [r6rs]
  (lambda (str i) (_peek (+ (addrof str) i))))

(define string-set! ;; [plt]
  (lambda (str i char) (poke (+ (addrof str) i) char)))

(define *concat-string
  (lambda (str1 str2)
    (let ((l1 (strlen str1))
	  (l2 (strlen str2)))
      (let ((dst (new (typeof str1) (+ (+ l1 l2) 1))))
	(memcpy (addrof dst) (addrof str1) l1)
	(memcpy (+ (addrof dst) l1) (addrof str2) l2)
	(string-set! dst (+ l1 l2) 0)
	dst))))

(define concat-string
  (lambda strings
    (fold-left *concat-string "" strings)))

(assert (= "resistance is futile" (concat-string "resistance " "is " "futile")))

(define dirname
  (lambda (path)
    (let ((i (strlen path)))
      (while (and (< 0 i)
                  (!= #"/" (_peek (+ (addrof path) (- i 1)))))
             (set i (- i 1)))
      (let ((dst (new (typeof "") (+ i 1))))
        (memcpy (addrof dst) (addrof path) i)
        (poke (+ (addrof dst) i) 0)
        dst))))

(assert (= "" (dirname "")))
(assert (= "/" (dirname "/path")))
(assert (= "dir/" (dirname "dir/path")))

;;;; Environment

(define *program-name*
  (let ((cstr (_peek *argv*)))
    (let ((buf (new (typeof "") (+ (strlen cstr) 1))))
      (strcpy buf cstr)
      buf)))

(define *program-dir* (dirname *program-name*))

;;;; Library loader

(define *load-path* (list "./"
                          (concat-string *program-dir* "../")
                          *program-dir*))

(define *loaded-files*)

;; Return p unless it is a null pointer
(define valid-pointer?
  (lambda (p)
    (and (!= 0 (convert 'long p)) p)))

(define load ;; [r5rs]
  (lambda (path)
;    (println 'load: path)
    (let ((fp  (or (valid-pointer? (fopen path "r"))
		   (error path ": " (strerror (_peek errno)))))
	  (obj ()))
      (while (!= '*EOF* (set obj (read fp)))
	     (if (> *verbose* 0) (println obj))
	     (set obj (eval obj global))
	     (if (> *verbose* 1) (princ " => " obj "\n")))
      (fclose fp)
      obj)))

;; Return path, or () if path is not exists.
(define file-exists? ;; [r6rs-lib(-)]
  (lambda (path)
    (let ((fp (fopen path "r")))
      (and (valid-pointer? fp)
	   (let () (fclose fp) path)))))

; (assert (file-exists? "abc-boot.k"))
; (assert (= (file-exists? "/etc/services") "/etc/services"))
; (assert (= () (file-exists? "/etc/**nonexist file**")))

;; Return a file name if the file is found, or () if not found

(define find-load-file-name
  (lambda (name)
    (if (= (string-ref name 0) #"/")
	(file-exists? name)
	(exists
	 (lambda (dir)
	   (file-exists? (concat-string dir name)))
	 *load-path*))))

;(assert (= (find-load-file-name "std.k") "lib/std.k"))

;; Require searches *load-path* to find a library file and evaluate
;; its contents only once.

(define require
  (lambda (name)
    (let ((path (or (find-load-file-name name)
		    (error "Library not found: " name))))
      (let ((*path (intern path)))
	(or (memq? *path *loaded-files*)
	    (let ()
	      (load path)
	      (set *loaded-files* (cons *path *loaded-files*))))))))
