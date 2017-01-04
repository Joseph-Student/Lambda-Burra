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

--------------------------------------- Actualiza el mazo y la mano del jugador--------------------------------
updateHandMallet :: Hand -> Mallet -> Mallet -> (Hand,Mallet)
updateHandMallet h m t =     
    if t == [] || (searchSuitHand h $ getSuit $ head t) == True then
        (h,m)
    else do
        let h' = joinHand (loadUp m t) h
        let m' = drop (sizeHand h' - sizeHand h) m
        (h',m')

---------------------------------------------Devuelve la carta que el usuario jugará--------------------------------
playUser :: Hand -> Int -> Mallet -> Card
playUser h c t = if v == True then card else (Card (Numeric 0) Oro)
    where card = selectCard h (c - 1)
          v = checkSuit card $ getSuit $ head t

----------------------------------------------- Verifica si el usuario carga ---------------------------------------
{-checkLoadUp :: Hand -> Hand -> Bool
checkLoadUp h h' = if h == h' then True else False-}
-------------------------------------------------------- Main ----------------------------------------------------------

main :: IO ()
main = do
    gen <- getStdGen
    putStrLn "Bienvenido al Juego Carga la Burra."
    --let rand_mallet = randomMallet gen mallet
    let rand_mallet = mallet
    let a = createHands rand_mallet
    --let table = addCardToTable (head $ drop 14 rand_mallet) []
    let table = table_test
    --let hand_lambda = fst a
    let hand_lambda = hand1
    --let hand_you = snd a
    let hand_you = hand5
    --let mallet_play = drop 15 rand_mallet
    let mallet_play = rand_mallet
    print "Mano de Lambda"
    putStrLn $ showHand hand_lambda
    --print "Mano You"
    --print $ showHand (H rand_mallet)
    print "Mesa"
    print $ showCard $ head table
    print "Tu Mano de Cartas:"
    putStrLn $ showHand hand_you
    --nuevo_you <- hand_you
    --print "Mazo a Jugar"
    --print $ showHand (H mallet_play)

playGame :: Hand -> Hand -> Mallet -> Mallet -> Player -> IO()
playGame hu hl m t p = do

	if p == Player then do
        let a = updateHandMallet hu m t
        
        if fst a /= hand_you then do
            print "Tu mano con cartas cargadas:"
            print $ showHand $ fst a
        else
            print ""

        let hu = fst a
        let m = snd a
   
        if t == [] then do
   	        print ("Introduzca el numero de la carta a jugar: (1-" ++ (show $ sizeHand hu) ++ ")")
            c <- getLine
            let card_you = playUser hu (read c) t
            --------se debe verificar que la carta seleccionada este en el rango de cartas disponibles.------------
            let new_mesa = addCardToTable card_you t
            print $ "Carta jugada por ti: " ++ (showCard $ head new_mesa)
            let card_lambda = cardToPlay (turnLambda mallet_play hand_lambda new_mesa) $ head new_mesa
            print $ "Carta jugada por Lambda: " ++ (showCard $ card_lambda)
            print $ "Carta ganadora de la ronda: " ++ (showCard $ winRound $ init $ addCardToTable (card_lambda) new_mesa)
            ---------------se debe verificar el ganador y devolver la llamada del metodo con las manos actualizadas eliminando
            ---------------las cartas lanzadas por cada jugador y el ganador
            if card_you > card_lambda then playGame hu hl m [] You else playGame hu hl m [] Lambda                	
        else 
            if (searchSuitMallet m $ getSuit $ head t) == False then do
                print "No hay cartas en el mazo tuvo que cargar la carta de la mesa."
                print "Su turno ha terminado. *La ronda fue ganada por Lambda*"
                playGame hu hl m [] Lambda
            else do
                print ("Introduzca el numero de la carta a jugar: (1-" ++ (show $ sizeHand hand_you) ++ ")")
                c <- getLine
                let card = playUser hand_you (read c) table
                if (card) == (Card (Numeric 0) Oro) then
                    print "Esa carta no es de la pinta que esta en la mesa."
                    ------se debería repetir hasta que ingrese una carta correcta---------------------
                else
                    print $ "Carta jugada por ti: "++ showCard card
                    
	
	else do



    

    
        --------------------------- Funcion para el usuario ---------------------------------
    --let new_mesa = addCardToTable card_you table
    --print $ "Nueva carta en la mesa: " ++ (showCard $ head new_mesa)
    --print $ "Carta ganadora de la ronda: " ++ (showCard $ winRound $ init $ addCardToTable (cardToPlay (turnLambda mallet_play hand_lambda new_mesa) $ head new_mesa) new_mesa)

    -----------------------------------------------------------------------------------
    gen' <- newStdGen
    print "Hasta luego"


{-	Prueba de las funciones.
	addCardToTable (cardToPlay hand1 $ head table) table
	winRound $ addCardToTable (cardToPlay hand1 $ head table) table
	winRound $ addCardToTable (cardToPlay (turnLambda mallet hand5 table) $ head table) table
-}

