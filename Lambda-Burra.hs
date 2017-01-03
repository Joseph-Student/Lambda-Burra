import Data.List
import System.Random
import Cards

data Player = Lambda | You deriving (Eq, Show);

------------------------------------------------- Lambda Juega Segundo -------------------------------------------------


------------------------------------ Devuelve una mano con cartas de la pinta que esta en la mesa ----------------------
turnLambda :: Mallet -> Hand -> Mallet -> Hand
turnLambda m h t = if s == False then joinHand (loadUp m t) h else h
    where s = searchSuitHand h suit
          suit = getSuit $ head t

------------------------ Genera una lista con las cartas que matan la carta que esta en la mesa ------------------------
killcards :: Hand -> Card -> Mallet
killcards (H card) (Card v su) = [x|x<-card, checkSuit x su, checkValue x v]

---------------------------------- Devuelve la menor carta de una lista ------------------------------------------------
lowercard :: Mallet -> Card
lowercard [x] = x
lowercard (x:y:xs) = if x < y then lowercard (x:xs) else lowercard (y:xs)

------------------------------------- Devuelve una lista con las cartas de una pinta -----------------------------------
cardsSuit :: Hand -> Card -> Mallet
cardsSuit (H card) (Card v su) = [x|x<-card, checkSuit x su]

----------------------------------- Devuelve la mejor carta a jugar ----------------------------------------------------
cardToPlay :: Hand -> Card -> Card
cardToPlay h c = if  killcards h c /= [] then lowercard $ killcards h c else lowercard $ cardsSuit h c

---------------------------------------- Lambda Juega Primero ----------------------------------------------------------

------------------------------------- Devuelve la carta mayor de la mano -----------------------------------------------
greaterCard :: Hand -> Card
greaterCard (H [x]) = x
greaterCard (H (x:y:xs)) = if x >= y then greaterCard (H (x:xs)) else greaterCard (H (y:xs))

---------------------------------------- Coloca una carta en la mesa ---------------------------------------------------
addCardToTable :: Card -> Mallet -> Mallet
addCardToTable c m = c:m

------------------------------------- Devuelve la carta que gana la ronda ----------------------------------------------
winRound :: Mallet -> Card
winRound m = max (head m) $ head $ tail m

------------------------------------------------ Jugador ---------------------------------------------------------------

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
    --print "Mazo a Jugar"
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

