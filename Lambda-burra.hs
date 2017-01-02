import System.IO.Error
import Data.Char
import Data.List
import System.Random

-------------------------------------------- Creacion de tipos de datos ------------------------------------------------
data Value = Numeric Int | Sota | Caballo | Rey | As
                          deriving (Eq, Show, Ord);

data Suit = Oro | Copas | Espadas| Bastos
                         deriving (Eq, Show, Ord);

data Card = Card {
                  value :: Value,
                  suit :: Suit
                  } deriving (Eq, Show , Ord);

newtype Hand = H Mallet deriving (Eq, Show);

data Player = Lambda | You deriving (Eq, Show);

type Mallet = [Card] --------------------------------- Sinonimo --------------------------------------------------------

mallet :: Mallet
mallet = [card1,card2,card3,card4,card5,card6,card7,card8,card9,card10,card11,card12,card13,card14,
          card15,card16,card17,card18,card19,card20,card21,card22,card23,card24,card25,card26,card27,
          card28,card29,card30,card31,card32,card33,card34,card35,card36,card37,card38,card39,card40]

{-[card1,card25,card39,card17,card22,card16,card37,card28,card4,card30,card4,card29,card13,
		card5,card31,card6,card27,card38,card9,card40,card21,card12,card32,card10,card2,card3,
		card17,card8,card29,card10,card8,card23,card33,card34,card35,card12,card7,card18,card30,
		card20,card11,card24,card26,card36,card14,card31,card19,card15,card5]-}

table :: Mallet
table = [card12]

----------------------------------------------- Definicion de Cartas ---------------------------------------------------
card1 = Card As Oro
card2 = Card (Numeric 2) Oro
card3 = Card (Numeric 3) Oro
card4 = Card (Numeric 4) Oro
card5 = Card (Numeric 5) Oro
card6 = Card (Numeric 6) Oro
card7 = Card (Numeric 7) Oro
card8 = Card Sota Oro
card9 = Card Caballo Oro
card10 = Card Rey Oro
card11 = Card As Copas
card12 = Card (Numeric 2) Copas
card13 = Card (Numeric 3) Copas
card14 = Card (Numeric 4) Copas
card15 = Card (Numeric 5) Copas
card16 = Card (Numeric 6) Copas
card17 = Card (Numeric 7) Copas
card18 = Card Sota Copas
card19 = Card Caballo Copas
card20 = Card Rey Copas
card21 = Card As Espadas
card22 = Card (Numeric 2) Espadas
card23 = Card (Numeric 3) Espadas
card24 = Card (Numeric 4) Espadas
card25 = Card (Numeric 5) Espadas
card26 = Card (Numeric 6) Espadas
card27 = Card (Numeric 7) Espadas
card28 = Card Sota Espadas
card29 = Card Caballo Espadas
card30 = Card Rey Espadas
card31 = Card As Bastos
card32 = Card (Numeric 2) Bastos
card33 = Card (Numeric 3) Bastos
card34 = Card (Numeric 4) Bastos
card35 = Card (Numeric 5) Bastos
card36 = Card (Numeric 6) Bastos
card37 = Card (Numeric 7) Bastos
card38 = Card Sota Bastos
card39 = Card Caballo Bastos
card40 = Card Rey Bastos

--------------------------------------------------- Manos de Prueba ----------------------------------------------------
hand1 = H [card14, card28, card3, card4, card38, card17, card8]
hand2 = H [card26, card5, card32, card16]
hand3 = H [card40, card5, card30]
hand4 = H [card3, card31, card15]
hand5 = H [card3]
hand6 = H [card15, card36, card23]
hand7 = H [card27, card20, card13, card1, card40]
hand8 = H [card37, card14, card21]
hand9 = H [card29, card24, card18, card10]



------------------------------------------------- Tamaño de la mano ----------------------------------------------------
sizeHand :: Hand -> Int
sizeHand (H xs)  = length xs 

------------------------------------------------ Genera una mano vacia -------------------------------------------------
empty :: Hand
empty = (H [])


------------------------------------------------- Lambda Juega Segundo -------------------------------------------------



---------------------------------------------- Busca una pinta en una mano ---------------------------------------------
searchSuitHand :: Hand -> Suit -> Bool
searchSuitHand (H []) s = False
searchSuitHand (H (x:xs)) s = if su == s then True else searchSuitHand (H xs) s
    where su = getSuit x

------------------------------------- Carga cartas del mazo hasta encontrar una pinta ----------------------------------
loadUpMallet :: Mallet -> Suit -> Mallet
loadUpMallet (Card v su:xs) s = if s /= su then [Card v su]++loadUpMallet xs s else [Card v su]

----------------------------------------------------- Tamaño del mazo --------------------------------------------------
sizeMallet :: Mallet -> Int
sizeMallet m = length m

--------------------------------------------------- Buscar Pinta en el mazo --------------------------------------------
searchSuitMallet :: Mallet -> Suit -> Bool
searchSuitMallet [] _ = False
searchSuitMallet (x:xs) s = if su == s then True else searchSuitMallet xs s
    where su = getSuit x

----------------------------------------------- Carga las cartas que estan en la mesa ----------------------------------
loadUpTable :: Mallet -> Mallet
loadUpTable m = m

------------------------------------------------------- Carga las cartas -----------------------------------------------
loadUp :: Mallet -> Mallet -> Mallet
loadUp m t = if (c == False) && (sizeMallet m > 0) then m ++ t else if c == False then t else loadUpMallet m s
    where c = searchSuitMallet m s
          s = getSuit card
          card = head t
-------------------------------------------------- Obtiene la pinta de una carta ---------------------------------------
getSuit :: Card -> Suit
getSuit (Card _ s) = s

-------------------------------------------------- Une las cartas cargadas a la mano -----------------------------------
joinHand :: Mallet -> Hand -> Hand
joinHand x (H cards) = (H (cards ++ x))

------------------------------------ Devuelve una mano con cartas de la pinta que esta en la mesa ----------------------
turnLambda :: Mallet -> Hand -> Mallet -> Hand
turnLambda m h t = if s == False then joinHand (loadUp m t) h else h
    where s = searchSuitHand h suit
          suit = getSuit $ head t

------------------------------------------- Verifica si una carta es de una pinta --------------------------------------
checkSuit :: Card -> Suit -> Bool
checkSuit c su = if getSuit c == su then True else False

------------------------------------------- Verifica si una carta es mayor que otra ------------------------------------
checkValue :: Card -> Value -> Bool
checkValue (Card v _) va = if v > va then True else False

------------------------ Genera una lista con las cartas que matan la carta que esta en la mesa ------------------------
killcards :: Hand -> Card -> Mallet
killcards (H card) (Card v su) = [x|x<-card, checkSuit x su, checkValue x v]

---------------------------------- Devuelve la menor carta de una lista ------------------------------------------------
lowercard :: Mallet -> Card
lowercard [x] = x
lowercard (x:y:xs) = if x < y then lowercard (x:xs) else lowercard (y:xs)

------------------------------------- Devuelve una lista con las cartas de una pinta -----------------------------------
cardSuit :: Hand -> Card -> Mallet
cardSuit (H card) (Card v su) = [x|x<-card, checkSuit x su]

----------------------------------- Devuelve la mejor carta a jugar ----------------------------------------------------
cardToPlay :: Hand -> Card -> Card
cardToPlay h c = if  killcards h c /= [] then lowercard $ killcards h c else lowercard $ cardSuit h c



---------------------------------------- Lambda Juega Primero ----------------------------------------------------------



------------------------------------- Devuelve la carta mayor de la mano -----------------------------------------------
greaterCard :: Hand -> Card
greaterCard (H [x]) = x
greaterCard (H (x:y:xs)) = if x > y then greaterCard (H (x:xs)) else greaterCard (H (y:xs))


---------------------------------------- Coloca una carta en la mesa ---------------------------------------------------
addCardToTable :: Card -> Mallet -> Mallet
addCardToTable c m = c:m

------------------------------------- Devuelve la carta que gana la ronda ----------------------------------------------
winRound :: Mallet -> Card
winRound m = max (head m) $ head $ tail m



------------------------------------------------ Jugador ---------------------------------------------------------------


--------------------------------------- Busca una carta en la mano -----------------------------------------------------
searchCard :: Hand -> Card -> Card
searchCard (H []) _ = (Card (Numeric 0) Oro)
searchCard (H (x:xs)) c = if x == c then x else searchCard (H xs) c


-------------------------------------- Muestra la Pinta de una carta ---------------------------------------------------
showSuit :: Card -> String
showSuit (Card _ s)
    | s == Oro = "Oro"
    | s == Copas = "Copas"
    | s == Espadas = "Espadas"
    | s == Bastos = "Bastos"

--------------------------------------- Muestra el valor de una carta --------------------------------------------------
showValue :: Card -> String
showValue (Card v _)
    | v == As = "1"
    | v == Sota = "10"
    | v == Caballo = "11"
    | v == Rey = "12"
    | v == (Numeric 2) = show 2
    | v == (Numeric 3) = show 3
    | v == (Numeric 4) = show 4
    | v == (Numeric 5) = show 5
    | v == (Numeric 6) = show 6
    | v == (Numeric 7) = show 7

--------------------------------------------- Muestra una carta --------------------------------------------------------
showCard :: Card -> String
showCard c = showValue c ++ " " ++ showSuit c


--------------------------------------------- Muestra una mano ---------------------------------------------------------
showHand :: Hand -> String
showHand (H []) = ""
showHand (H (x:[])) = showCard x
showHand (H c) = showCard (head c) ++ ", " ++ showHand (H (tail c))


------------------------------------------- Seleccionar una carta ------------------------------------------------------
selectCard :: Hand -> Int -> Card
selectCard h i = m !! i
    where m = getMallet h


------------------------------------------- Crea un mazo aleatorio -----------------------------------------------------
randomMallet :: StdGen -> Mallet -> Mallet
randomMallet gen m = let (r,g) = randomR (0, (sizeMallet m)-1) gen
    in (m !! r: if sizeMallet m > 1 then randomMallet g $ delete (m !! r) m else [])

-------------------------------------- Crea las manos de los jugadores -------------------------------------------------
createHands :: Mallet -> (Hand,Hand)
createHands m = (H ([x|x<-m,y<-[1,3..13],checkIndex x m y]), H ([x|x<-m,y<-[0,2..12],checkIndex x m y]))

-------------------------------------- Verifica el indice de la carta --------------------------------------------------
checkIndex :: Card -> Mallet -> Int -> Bool
checkIndex c m x = (head $ c `elemIndices` m) == x

-------------------------------------- Obtine la lista de cartas de la mano --------------------------------------------
getMallet :: Hand -> Mallet
getMallet (H x) = x

-------------------------------------- Verifica la carta con la que esta en la mesa ------------------------------------
--checkCard :: Card
-------------------------------------------------------- Main ----------------------------------------------------------

main :: IO ()
main = do
    gen <- getStdGen
    putStrLn "Bienvenido al Juego Carga la Burra."
    let rand_mallet = randomMallet gen mallet
    let a = createHands rand_mallet
    let table = addCardToTable (head $ drop 14 rand_mallet) []
    let hand_lambda = fst a
    let hand_you = snd a
    let mallet_play = drop 15 rand_mallet
    {-print "Mano de Lambda"
    putStrLn $ showHand hand_lambda
    print "Mano You"-}
    --print $ showHand (H rand_mallet)
    print "Mesa"
    print $ showCard $ head table
    print "Juega You"
    putStrLn $ showHand hand_you
    print "Mazo a Jugar"
    --print $ showHand (H mallet_play)
    print ("Introduzca el numero de la carta a jugar: (1-" ++ (show $ sizeHand hand_you) ++ ")")
    c <- getLine
    let card = selectCard hand_you ((read c) - 1)
   -- if (checkSuit card $ getSuit $ head table) == True then addCardToTable card table else print "Esa carta de es de la pinta."
    gen' <- newStdGen
    print "Hasta luego"


{-	Prueba de las funciones.
	addCardToTable (cardToPlay hand1 $ head table) table
	winRound $ addCardToTable (cardToPlay hand1 $ head table) table
	winRound $ addCardToTable (cardToPlay (turnLambda mallet hand5 table) $ head table) table
-}

