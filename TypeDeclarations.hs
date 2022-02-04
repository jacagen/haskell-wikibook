type Name = String

data Anniversary = Birthday Name Date       -- name, date
                 | Wedding Name Name Date -- spouse name 1, spouse name 2, date


data Date = Date Int Int Int

showDate :: Date -> String
showDate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

showAnniversary :: Anniversary -> String

showAnniversary (Birthday name date) =
   name ++ " born " ++ showDate date

showAnniversary (Wedding name1 name2 date) =
   name1 ++ " married " ++ name2 ++ " on " ++ showDate date
