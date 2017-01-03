module Cards
 where

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
type Mallet = [Card]

------------------------------------------------- Creacion de Mazo -----------------------------------------------------
mallet :: Mallet
mallet = [card1,card2,card3,card4,card5,card6,card7,card8,card9,card10,card11,card12,card13,card14,
          card15,card16,card17,card18,card19,card20,card21,card22,card23,card24,card25,card26,card27,
          card28,card29,card30,card31,card32,card33,card34,card35,card36,card37,card38,card39,card40]

{-[card1,card25,card39,card17,card22,card16,card37,card28,card4,card30,card4,card29,card13,
		card5,card31,card6,card27,card38,card9,card40,card21,card12,card32,card10,card2,card3,
		card17,card8,card29,card10,card8,card23,card33,card34,card35,card12,card7,card18,card30,
		card20,card11,card24,card26,card36,card14,card31,card19,card15,card5]-}

------------------------------------------------- Creacion de Mesa -----------------------------------------------------
table_test :: Mallet
table_test = [card12]

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
loadUp m t = if c == False then m ++ t else loadUpMallet m s
    where c = searchSuitMallet m s
          s = getSuit card
          card = head t
-------------------------------------------------- Obtiene la pinta de una carta ---------------------------------------
getSuit :: Card -> Suit
getSuit (Card _ s) = s

------------------------------------------- Verifica si una carta es de una pinta --------------------------------------
checkSuit :: Card -> Suit -> Bool
checkSuit c su = if getSuit c == su then True else False

------------------------------------------- Verifica si una carta es mayor que otra ------------------------------------
checkValue :: Card -> Value -> Bool
checkValue (Card v _) va = if v > va then True else False

-------------------------------------------------- Une las cartas cargadas a la mano -----------------------------------
joinHand :: Mallet -> Hand -> Hand
joinHand x (H cards) = (H (cards ++ x))

--------------------------------------- Busca una carta en la mano -----------------------------------------------------
searchCard :: Hand -> Card -> Card
searchCard (H []) _ = (Card (Numeric 0) Oro)
searchCard (H (x:xs)) c = if x == c then x else searchCard (H xs) c

------------------------------------------- Seleccionar una carta ------------------------------------------------------
selectCard :: Hand -> Int -> Card
selectCard h i = m !! i
    where m = getMallet h

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

-------------------------------------- Obtine la lista de cartas de la mano --------------------------------------------
getMallet :: Hand -> Mallet
getMallet (H x) = x
