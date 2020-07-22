module Query where

import UserInfo
import Rating
import Movie

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

--------------------------------Set Operations--------------------------------
intersect set1 set2 = filter (\x -> elem x set1) set2
set_diff set1 set2 = filter (\x -> not (elem x set1)) set2
union set1 set2 = set1 ++ (set_diff (intersect set1 set2) (set2))

--------------------------------Table methods---------------------------------
getTS (Table ts en) = ts
getEN (Table ts en) = en
cloneTable table = Table (getTS table) (getEN table)
flatT table = (getTS table):(getEN table)

--------------------------------Matrix operations-----------------------------
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

split :: Char -> String -> [String]
split c = foldr op [[]] 
              where op x (y:ys)
                      | x /= c = (x:y):ys
                      | otherwise = []:(y:ys)

m_split _ _ [] = []
m_split cs ls string =  init (map (split cs )(split ls string))

--------------------------------General-purpouse Functions--------------------

rank :: Eq a => a->[a]->Int
rank x [] =1
rank x (y:xs)
    | x==y =1
    | otherwise = 1+(rank x xs)

reorder :: [Int]->[a]->[a]
reorder [] ar = []
reorder is [] = []
reorder is ar = (ar!!(head is)):(reorder (tail is) ar)

-- maps the ranks of the selected columns
toR :: [String]->[String]->[Int]
toR [] _ = []
toR _ [] = []
toR l1 l2 = map (\x -> ((rank x l2) -1) ) l1

indices :: [String]->Table->[Int]
indices s (Table h e) = toR s h

reorderE :: [Int]->[[String]]->[[String]]
reorderE is en = map (reorder is) en

nreorderE :: Integer->[Int]->[[String]]->[[String]]
nreorderE n is en = map (reorder is) (take (fromIntegral n)en)

-- function imediatelly called before select
pre_select :: [Int]->Table->Table
pre_select is (Table ts en) = Table (reorder is ts) (reorderE is en)

npre_select :: Integer->[Int]->Table->Table
npre_select n is (Table ts en) = Table (reorder is ts) (nreorderE n is en)

--------------------------------Variables-------------------------------------
user_info = read_table '|' '\n' user_info_str
movie = read_table '|' '\n' movie_str
rating = read_table ' ' '\n' rating_str

--------------------------------Tasks-----------------------------------------

-- TODO 1

read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table cs ls string = Table (head (m_split cs ls string) ) (tail (m_split cs ls string) )

-- TODO 2
pad n string 
    | (length string <= n) = string ++ (replicate ( n-(length string) ) ' ')++"|"
    | otherwise = string

-- max length for each column
toL :: [String]->[[String]]-> [Int]
toL _ [] = [0]
toL ts en = foldr (zipWith max) (map length ts) (map (map length) en)

pad_entries :: [[String]]->[Int]->String
pad_entries [] is = []
pad_entries en is = "|"++(concat(zipWith pad is (head en)) )++['\n']++(pad_entries (tail en) is)

pre_format :: String -> [Int] -> Table -> String
pre_format fr is (Table ts en) = ( fr++"|"++(concat(zipWith pad is ts ) )++['\n']++fr ) ++ ( pad_entries en is )++fr

-- last line of the table and the lines that separate the TableSchema
frr :: [Int]->String
frr is = ( ( (sum is)+(length is) +1) `replicate` '-' )++['\n']

pre_format2 :: [Int]->Table -> String
pre_format2 is (Table ts en) = pre_format (frr is) is (Table ts en)

format ::Table -> String
format (Table ts en)=pre_format2 (toL ts en) (Table ts en)

instance Show Table where
    show (Table header entries) = format (Table header entries)

-- TODO 3

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition
    deriving Show

lt :: String->Integer->TableSchema->Entry->Bool
lt fl it ts en
    | (fl `rank` ts) > (length en) = False
    | otherwise = (read (en!!(fl `rank` ts -1))) < it

eql :: String->String->TableSchema->Entry->Bool
eql fl it ts en
    | (fl `rank` ts) > (length en) = False
    | otherwise = (en!!(fl `rank` ts -1)) == it

inSt :: String->[String]->TableSchema->Entry->Bool
inSt fl it ts en
    | (fl `rank` ts) > (length en) = False
    | otherwise = (en!!(fl `rank` ts -1)) `elem` it

getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter (Not fc) ts = ( \x -> (not ( (getFilter fc ts) x ) ) )
getFilter (Lt fl it) ts = (\x -> (lt fl it ts x))
getFilter (Eq fl it) ts = (\x -> (eql fl it ts x))
getFilter (In fl it) ts = (\x -> (inSt fl it ts x))

-- TODO 4
data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

select :: [String]->Table->Table
select fl (Table ts en) = pre_select (toR fl ts) (Table ts en)

nselect :: Integer->[String]->Table->Table
nselect n fl (Table ts en) = npre_select n (toR fl ts) (Table ts en)

-- aux for filter
selectR :: ([String]->Bool)->[[String]]->[[String]]
selectR _ [] = []
selectR fun table = tail (filter fun (tail table)) 

eval :: Query -> Table

eval (Atom table)= table
eval (Filter fc q) = Table (getTS (eval q)) ((\x->(filter (getFilter fc (getTS x) ) (getEN x) )) (eval q) )

eval (Select strs q) = select strs (eval q)
eval (SelectLimit strs n q) = nselect n strs (eval q)

eval (q1 :|| q2) = Table (getTS (eval q1)) (union (getEN (eval q1)) (getEN (eval q2)))


-- TODO 5

first_occurence :: Int->String->[[String]]->[String]
first_occurence idx id [] = []
first_occurence idx id en| ((head en)!!idx)==id = (head en)  
first_occurence idx id en = first_occurence idx id (tail en)

-- selects the right zone
aux_same_zone :: String->Table->String
aux_same_zone id (Table ts en) = (first_occurence ((rank "user_id" ts) -1) id en)!!((rank "zone" ts) -1)

same_zone :: String -> Query
same_zone id = Select ["user_id","occupation"] (Filter (Not (Eq "user_id" id)) ( Filter (Eq "zone" (aux_same_zone id user_info)) (Atom user_info)))

-- less (strict)
lst :: Integer->FilterCondition
lst i = Lt "age" i
-- greater or equal
ge :: Integer->FilterCondition
ge i = Not (Lt "age" i)

male_within_age :: Integer -> Integer -> Query
male_within_age x y= Select ["occupation","zone"] (Filter (ge x)(Filter (Not(Eq "age" (show x))) (Filter (lst y) (Filter (Eq "sex" "M") (Atom user_info))) ))


inZones :: [String]-> FilterCondition
inZones zones = In "zone" zones

inOccup :: [String]-> FilterCondition
inOccup occup = In "occupation" occup

lth :: Integer->FilterCondition
lth age = Lt "age" age

mixed :: [String] -> [String] -> Integer -> Query
mixed zones occup age = Select ["user_id"] (Filter (inZones zones) (Filter (inOccup occup) (Filter (lth age) (Atom user_info))))
