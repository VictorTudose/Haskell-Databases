# Haskell-Databases

![](./misc/logo.png)

This project implements a SQL-like set of functions that work on table 

Functions:
* Select
* SelectLimit
* Filter

Examples of use :
```
Select ["user_id"] (Filter (inZones zones) (Filter (inOccup occup) (Filter (lth age) (Atom user_info))))
```
```
Select ["occupation","zone"] (Filter (ge x)(Filter (Not(Eq "age" (show x))) (Filter (lst y) (Filter (Eq "sex" "M") (Atom user_info))) ))
```
Notes :
* any table must be preceded by ```Atom``` type constuctor
* ```Select``` reorders the columns

Tables are read from a string in the following manner
```
user_info = read_table '|' '\n' user_info_str
movie = read_table '|' '\n' movie_str
rating = read_table ' ' '\n' rating_str
```
Where the first argument is the column separator and the second is the line separator

