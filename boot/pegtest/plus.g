; Create a left recursive structure by PEG

digit = [01234567890]
number = digit+ $#10

plus = number :x ("+" number)* :xs -> (make-plus x (->list xs))
