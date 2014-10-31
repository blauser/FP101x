import Lab2
-- q0 --
q0 = toDigits 12321

-- q1 --
q1 = toDigits (-531)

-- q2 --
-- q2 = toDigits 7.6
-- type error

-- q3 --
q3 = toDigits 0

-- q4 --
-- q4 = toDigits "123"
-- type error

-- q5 --
q5 = toDigits 666

-- q6 --
q6 = toDigitsRev 12321

-- q7 --
q7 = toDigitsRev (-531)

-- q8 --
-- q8 = toDigitsRev 7.6
-- type error

-- q9 --
q9 = toDigitsRev 0

-- q10 --
-- q10 = toDigitsRev "123"
-- type error

-- q11 --
q11 = toDigitsRev 666

-- q12 --
q12 = doubleSecond []

-- q13 --
q13 = doubleSecond [5]

-- q14 --
q14 = doubleSecond [2, 5]

-- q15 --
q15 = doubleSecond [1..]

-- q16 --
q16 = doubleSecond [1, 0, 1, 0, 1]

-- q17 --
q17 = sumDigits []

-- q18 --
q18 = sumDigits [-12, 12]

-- q19 --
q19 = sumDigits [0, 0, 0]

-- q20 --
q20 = sumDigits [6, 66, 666]

-- q21 --
q21 = sumDigits [1, 3 ..]

-- q22 --
q22 = isValid (-12786592316)

-- q23 --
-- q23 = isValid 231753.65121
-- type error

-- q24 --
q24 = isValid 5256283618614517

-- q25 --
q25 = isValid 4556945538735694

-- q26 --
q26 = isValid 0000000000000000

-- q27 --
q27 = numValid creditcards