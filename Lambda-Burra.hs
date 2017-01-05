import Data.List
import System.Random
import Cards


data Player = Lambda | You deriving (Eq, Show);

------------------------------------------------- Lambda Juega Segundo -------------------------------------------------


------------------------------------ Devuelve una mano con cartas de la pinta que esta en la mesa ----------------------
{-turnLambda :: Mallet -> Hand -> Mallet -> (Hand,Mallet)
turnLambda m h t = if s == False then do
    let h = joinHand (loadUp m t) h else h
    where s = searchSuitHand h suit
          suit = getSuit $ head t-}

--------------------------------------- Actualiza el mazo y la mano del jugador--------------------------------
updateHandMallet :: Hand -> Mallet -> Mallet -> (Hand,Mallet)
updateHandMallet h m t =
    if (searchSuitHand h $ getSuit $ head t) == True then
        (h,m)
    else do
        let h' = joinHand (loadUp m t) h
        let m' = drop (sizeHand h' - sizeHand h) m
        (h',m')

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


------------------------------------------Devuelve la carta que el usuario jugará--------------------------------
playUserTwo :: Hand -> Int -> Mallet -> Card
playUserTwo h c t = if v == True then card else (Card (Numeric 0) Oro)
    where card = selectCard h (c - 1)
          v = checkSuit card $ getSuit $ head t

-------------------------------------------  -------------------------------------------------------------------
playUserOne :: Hand -> Int -> Card
playUserOne h c = selectCard h (c - 1)

----------------------------------------------Jugar-------------------------------------------------------------
playGame :: Hand -> Hand -> Mallet -> Mallet -> Player -> IO()
playGame hu hl m t p = do
    if p == You then do
        print "Tu Mano de Cartas:"
        putStrLn $ showHand hu
        if t == [] then do
		    print $ "Introduzca el numero de la carta a jugar: (1-" ++ (show $ sizeHand hu) ++ ")"
		    c <- getLine
		     --------se debe verificar que la carta seleccionada este en el rango de cartas disponibles.------------
		    let card_you = playUserOne hu (read c)
		    let new_hand_you = H $ delete card_you $ getMallet hu
		    let new_mesa = addCardToTable card_you t
		    print $ "Carta jugada por ti: " ++ (showCard $ card_you)
		    if sizeHand new_hand_you > 0 then do
		        let a = updateHandMallet hl m new_mesa
		        let hand_lambda = fst a
		        let mallet_play = snd a
		        if (searchSuitMallet m $ getSuit $ head t) == False then do
		            print "Lambda cargó de la mesa."
		            print "El turno de lambda ha terminado. *La ronda fue ganada por Usted*"
		            playGame new_hand_you hand_lambda mallet_play [] You
		        else do
		            let card_lambda = cardToPlay hand_lambda $ head new_mesa
		            let new_hand_lambda = H $ delete card_lambda $ getMallet hand_lambda
		            let table = addCardToTable card_lambda new_mesa
		            print $ "Carta jugada por Lambda: " ++ (showCard $ card_lambda)
		            let card_win = winRound $ table
		            print $ "Carta ganadora de la ronda: " ++ (showCard $ card_win)
		            if sizeHand new_hand_lambda > 0 then
		                if card_you == card_win then
		                    playGame new_hand_you new_hand_lambda mallet_play [] You
		                else
		                    playGame hu hl m [] Lambda
		                else
		                    print "**Lo Sentimos, ha Perdido la Partida.**"
		    else
		        print "**Felicitaciones Usted ha Ganado la Partida.**"
	    else do
    	    let a = updateHandMallet hu m t
    	    if fst a /= hu then do
    	    	print "Tu mano con cartas cargadas:"
    	    	print $ showHand $ fst a
    	    else
    	    	print ""
    	    let hu = fst a
    	    let m = snd a
    	    {-if (searchSuitMallet m $ getSuit $ head t) == False then do
    	    	print "No hay cartas en el mazo tuvo que cargar la carta de la mesa."
    	    	print "Su turno ha terminado. *La ronda fue ganada por Lambda*"
    	    	playGame hu hl m [] Lambda
    	    else do-}
    	    	print ("Introduzca el numero de la carta a jugar: (1-" ++ (show $ sizeHand hu) ++ ")")
    	    	c <- getLine
    	    	let card_you = playUserTwo hu (read c) t
    	    	let new_mesa = addCardToTable card_you t
    	    	if card_you == (Card (Numeric 0) Oro) then
    	    		print "Esa carta no es de la pinta que esta en la mesa."
    	    		------se debería repetir hasta que ingrese una carta correcta---------------------
    	    	else do
    	    	    print $ "Carta jugada por ti: "++ showCard card_you
    	    	    --let card_lambda = cardToPlay (turnLambda m hl new_mesa) $ head new_mesa
    	    	    let table = addCardToTable card_lambda new_mesa
    	    	    print $ "Carta jugada por Lambda: " ++ (showCard $ card_lambda)
    	    	    let card_win = winRound $ init $ table
    	    	    print $ "Carta ganadora de la ronda: " ++ (showCard card_win)
    	    	    if card_you == card_win then
    	    	        playGame hu hl m [] You
    	    	    else
    	    	        playGame hu hl m [] Lambda

    else do
    	let card_lambda = greaterCard hl
    	let new_mesa = addCardToTable card_lambda t
    	print $ "Carta jugada por Lambda: " ++ (showCard $ card_lambda)
    	print $ "Introduzca el numero de la carta a jugar: (1-" ++ (show $ sizeHand hu) ++ ")"
    	c <- getLine
    	let card = playUserTwo hu (read c) t
    	if card == (Card (Numeric 0) Oro) then
    		print "Esa carta no es de la pinta que esta en la mesa."
    		-----se debería repetir hasta que ingrese una carta correcta---------------------
    	else do
    		print $ "Carta jugada por ti: "++ showCard card
    		print $ "Carta ganadora de la ronda: " ++ (showCard $ winRound $ init $ addCardToTable card_lambda new_mesa)

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
    --nuevo_you <- hand_you
    --print "Mazo a Jugar"
    --print $ showHand (H mallet_play)
    playGame hand_you hand_lambda mallet_play table You


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

