import Language.Haskell.TH (safe)
import Data.Char
import Data.IntMap (member)



-- Funktionen fungerar genom att hämta de två första elementen i en lista
-- Och sedan addera de och lägga till nya resultatet i listan
-- Man gör detta tills x blir 1 eftersom man redan börjar på en lista med
-- [1, 0] VIKTIGT ATT PASSA DETTA TILL FUNKTIONEN
fib 1 l = head l

fib x l = fib (x-1) newlist
    where
        newlist = newNum:l
        newNum = (head l) + (l !! 1)
        

isVowel c =  c `elem` "aeiouy"




rovarsprak :: String -> String
rovarsprak "" = ""

rovarsprak s
    | isVowel (head s) = head s : rovarsprak (tail s)
    | otherwise = xox ++ rovarsprak (tail s)
    where
        xox = [head s] ++ ['o'] ++ [head s]


karpsravor :: String -> String
karpsravor "" = ""

karpsravor (x:'o':y:xs)
    --detta kolla ifall x och y är samma och ifall "o" är mellan
    -- då hanterar vi det, annars är det bara ett vanligt "o" men det är viktigt att skilja på fallen
    | x == y = x : karpsravor xs
    | otherwise = x : karpsravor ('o':y:xs) -- här skickar vi med "o:et" till nästa iteration eftersom x:o:y inte matchade


karpsravor (x:xs) = x : karpsravor xs -- för saker som är vokaler :)




--Dela upp orden, en funktion



splitter "" = []
splitter (x:xs)
    | isAlpha x = [x] : splitter xs
    | otherwise = [] : splitter xs




samman :: [Char] -> [a]
samman [] = []
samman list 
    | isAlpha x = samman(added)
    | otherwise = samman (tail list)
    where 
        x = head list
        added = x:(head list) : tail list


-- hi me -> h : -> i: -> [" "]: -> m: -> e: -> [] 

--räkna hur många bokstäver per ord
--räkna hur många ord





--Skyffla funktionen jobbade jag och Sofia tillsammans
--Koden fungerar genom att man hämtar varannat element
--I listan "lista" genom funktionen "first" för att få varannat element
--Vi använder second för att få alla andra element och sedan skyfflar vi de i nästa iteration
--Vi skickar med resultatet av varje skyffling tills "ovriglista" blir tom då vi returnar result
skyffla [] result = result
skyffla lista result = skyffla ovriglista nylista
    where 
        nylista = result ++ first lista
        ovriglista = second lista


--First och second använder pattern matching för att hitta patterns i en lista
--First kommer då spara varrant element i en lista
--Alltså: [1, 2, 3, 4, 5] -> [1, 3, 5]
first (x:y:xs) = x : first xs; 
first (x:xs) = x : first xs;
first _  = []

-- Second fungerar på samma sätt men börjar med andra elementet

--Alltså:  [1, 2, 3, 4, 5] -> [2, 4]
second (x:y:xs) = y : second xs;
second _ = []