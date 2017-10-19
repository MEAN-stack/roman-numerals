-- Arabic - Roman numeral converter

-- Convert Roman to Arabic
-- We are pretty relaxed about what is allowed
--
arabic :: String -> Int
arabic [] = 0
arabic ('C':'M':xs) = 900 + (arabic xs)
arabic ('C':'D':xs) = 400 + (arabic xs)
arabic ('X':'L':xs) = 40 + (arabic xs)
arabic ('X':'C':xs) = 90 + (arabic xs)
arabic ('I':'X':xs) = 9 + (arabic xs)
arabic ('I':'V':xs) = 4 + (arabic xs)
arabic ('I':xs) = 1    + (arabic xs)
arabic ('V':xs) = 5    + (arabic xs)
arabic ('X':xs) = 10   + (arabic xs)
arabic ('L':xs) = 50   + (arabic xs)
arabic ('C':xs) = 100  + (arabic xs)
arabic ('D':xs) = 500  + (arabic xs)
arabic ('M':xs) = 1000 + (arabic xs)
arabic _ = error "Invalid numeral"

-- Convert Arabic to minimal Roman form
roman :: Int -> String
roman n = (thousands n) ++ (hundreds n) ++ (tens n) ++ (units n)

-- thousands are easy - just print lots of Ms
thousands n = replicate (n `div` 1000) 'M'

-- hundreds
hundreds n  = hundreds' ((n `mod` 1000) `div` 100)
-- special cases
hundreds' 9 = "CM"
hundreds' 4 = "CD"
hundreds' n
  -- 500 is 'D'
  | n>=5 = 'D':(hundreds' (n-5))
  -- otherwise print lots of Cs
  | otherwise = replicate n 'C'

-- tens
tens n = tens' ((n `mod` 100) `div` 10)
-- special cases
tens' 9 = "XC"
tens' 4 = "XL"
tens' n
  -- 50 is 'L'
  | n>=5 = 'L':(tens' (n-5))
  -- otherwise print lots of Xs
  | otherwise = replicate n 'X'

-- units
units n     = units' (n `mod` 10)

units' 0 = []    -- finished
-- special cases
units' 9 = "IX"
units' 4 = "IV"
units' n
  -- 5 is 'V'
  | n>=5 = 'V':(units' (n-5))
  -- otherwise print Is
  | otherwise  = replicate n 'I'

-- convert a Roman nbumeral into minimal form
--
minimise :: String -> Int
-- just convert it to Arabic and then back to Roman
minimise x = (length x) - (length $ roman $ arabic x)

