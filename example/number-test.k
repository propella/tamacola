(check (parse-collection number/number "3") => 3)
(check (parse-collection number/number "345") => 345)

(check (parse-collection number/number1 "3") => 3)
(check (parse-collection number/number1 "345") => 345)

(check (parse-collection number/number2s "3") => 3)
(check (parse-collection number/number2s "345") => 345)
