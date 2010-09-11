;; Object method send test

(send avmplus/System write "Hello ")
(send avmplus/System write "World\n")
(print (send avmplus/System getAvmplusVersion))
(send avmplus/System exec "uname -a")
