import Data.List
import System.Random
import System.IO.Unsafe
import Cards

data Player = Lambda | You deriving (Eq, Show);

------------------------------------------------- Lambda Juega Segundo -------------------------------------------------

--------------------------------------- Actualiza el mazo y la mano del jugador--------------------------------
updateHandMallet :: Hand -> Mallet -> Mallet -> (Hand,Mallet)
updateHandMallet h m t =
    if (searchSuitHand h $ getSuit $ head t) then
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
createHands m = (H [x|x<-m,y<-[1,3..13],checkIndex x m y], H [x|x<-m,y<-[0,2..12],checkIndex x m y])

-------------------------------------- Verifica el indice de la carta --------------------------------------------------
checkIndex :: Card -> Mallet -> Int -> Bool
checkIndex c m x = (head $ c `elemIndices` m) == x


------------------------------------------Devuelve la carta que el usuario jugará--------------------------------
playUserTwo :: Hand -> Int -> Mallet -> Card
playUserTwo h c t = if v then card else (Card (Numeric 0) Oro)
    where card = selectCard h (c - 1)
          v = checkSuit card $ getSuit $ head t

------------------------------------------- Devuelve la carta seleccionada por el usuario ------------------------------
playUserOne :: Hand -> Int -> Card
playUserOne h c = selectCard h (c - 1)

----------------------------------- Verifica si algún jugador se quedado sin cartas -------------------------------------------------
winPlayerRoundLambda :: Hand -> Hand -> Card -> Card -> Mallet -> IO()
winPlayerRoundLambda hu hl c cw m =
    if ((sizeHand hl > 0) || (sizeHand hu > 0)) then
        winPlayer hu hl c cw m
    else
        if sizeHand hl == 0 then
            putStrLn "              **Perdiste la Partida, Lambda se ha quedado sin cartas.**"
        else
            putStrLn "                    **Ganaste la Partida, te has quedado sin cartas.**"

---------------------------------- Verifica quien gana la ronda -------------------------------------------------------
winPlayer :: Hand -> Hand -> Card -> Card -> Mallet -> IO()
winPlayer hu hl c cw m =
    if c == cw then do
        putStrLn "              Usted ha ganado la ronda. Juega Primero en la siguiente."
        nextRound hu hl m [] You
    else do
        putStrLn "              Lambda ha ganado la ronda. Juega Lambda Primero en la siguiente."
        nextRound hu hl m [] Lambda

---------------------------------- Continua a la siguiente ronda ---------------------------------------------
nextRound :: Hand -> Hand -> Mallet -> Mallet -> Player -> IO()
nextRound hu hl m t p = do
    putStrLn "              Presiona enter para continuar a la siguiente ronda."
    x <- getLine
    putStrLn ""
    playGame hu hl m t p

---------------------------------- Verifica si el jugador ha cargado cartas ------------------------------
checkHand :: Hand -> Hand -> Player -> IO()
checkHand hm h p = do
    if ((hm /= h) && (p == You)) then 
        loadCarts h 
    else 
        if ((hm /= h) && (p == Lambda)) then
            putStrLn "         Lambda no tiene cartas de la pinta de la mesa. Ha cargado del mazo"
        else putStrLn ""

---------------------------------- Mensaje para cargar cartas ---------------------------------------
loadCarts :: Hand -> IO()
loadCarts h = do
    mostrarMano h
    putStrLn "                     No tienes cartas de la pinta de la mesa, debes cargar del mazo."
    putStrLn "Presiona enter para cargar."
    x <- getLine
    putStrLn ""

---------------------------------- Pedir carta ------------------------------------------------------
pedirCarta :: Hand -> IO String
pedirCarta h = do
    mostrarMano h
    putStrLn $ "        Introduzca el numero de la carta a jugar: (1-" ++ (show $ sizeHand h) ++ ")"
    c <- getLine
    if (read c < 1) || (read c > sizeHand h) then do
        putStrLn "          Debe introducir un numero que este en el rango."
        pedirCarta h
    else
        return c

----------------------------------------- Mostrar la mano de cartas -------------------------------------------
mostrarMano :: Hand -> IO ()
mostrarMano h = do
    putStrLn ""
    putStrLn "                          Tu Mano de Cartas:"
    putStrLn $ "    "++showHand h

----------------------------------------- Limpiar pantalla para seguir el juego ------------------------------------
{-clearScreen :: Mallet -> IO ()
clear t = do
    if null t then :!cls else putStrLn ""-}

--------------------------------------------------Jugar-------------------------------------------------------------
playGame :: Hand -> Hand -> Mallet -> Mallet -> Player -> IO()
playGame hu hl m t p = do
    --clearScreen t
    if p == You then do
        if null t then do
            let c = unsafePerformIO (pedirCarta hu)
            let card_you = playUserOne hu (read c)
            let new_hand_you = H $ delete card_you $ getMallet hu
            let new_mesa = addCardToTable card_you t
            if sizeHand new_hand_you > 0 then do
                let a = updateHandMallet hl m new_mesa
                let hand_lambda = fst a
                let mallet_play = snd a
                --putStrLn "Mano de Lambda"
                --putStrLn $ showHand hand_lambda
                if (not (searchSuitHand hl $ getSuit $ head new_mesa) && not (searchSuitMallet m $ getSuit $ head new_mesa)) then do
                    putStrLn "                      Lambda carga de la mesa."
                    putStrLn "                   El turno de lambda ha terminado. *La ronda fue ganada por Usted*"
                    nextRound new_hand_you hand_lambda mallet_play [] You
                else do
                    let card_lambda = cardToPlay hand_lambda $ head new_mesa
                    let new_hand_lambda = H $ delete card_lambda $ getMallet hand_lambda
                    let table = addCardToTable card_lambda new_mesa
                    putStrLn $ "                Carta jugada por ti: " ++ (showCard $ card_you)
                    putStrLn $ "                Carta en la mesa: " ++ (showCard $ card_you)
                    checkHand hand_lambda hl Lambda
                    putStrLn $ "                Lambda tiene en la mano " ++ (show $ sizeHand hand_lambda) ++ " cartas sin jugar."
                    putStrLn $ "                Carta jugada por Lambda: " ++ (showCard $ card_lambda)
                    let card_win = winRound $ table
                    putStrLn $ "                Carta ganadora de la ronda: " ++ (showCard $ card_win)
                    winPlayerRoundLambda new_hand_you new_hand_lambda card_you card_win mallet_play
            else
                putStrLn "                      **Felicitaciones Usted ha Ganado la Partida.**"
        else do
            let a = updateHandMallet hu m t
            checkHand (fst a) hu You
            let hu = fst a
            let m = snd a
            let c = unsafePerformIO (pedirCarta hu)
            let card_you = playUserTwo hu (read c) t
            if card_you == (Card (Numeric 0) Oro) then
                putStrLn "              Esa carta no es de la pinta que esta en la mesa."
                ------ todo esto se debería repetir hasta que ingrese una carta correcta---------------------
            else do
                let new_hand_you = H $ delete card_you $ getMallet hu
                let new_mesa = addCardToTable card_you t
                putStrLn $ "                        Carta jugada por ti: " ++ (showCard $ card_you)
                let a = updateHandMallet hl m new_mesa
                checkHand (fst a) hl Lambda
                let hand_lambda = fst a
                let mallet_play = snd a
                let card_lambda = cardToPlay hand_lambda $ head new_mesa
                let new_hand_lambda = H $ delete card_lambda $ getMallet hand_lambda
                let table = addCardToTable card_lambda new_mesa
                putStrLn $ "                Lambda tiene en la mano " ++ (show $ sizeHand hand_lambda) ++ " cartas sin jugar."
                putStrLn $ "                Carta jugada por Lambda: " ++ (showCard $ card_lambda)
                let card_win = winRound $ table
                putStrLn $ "                Carta ganadora de la ronda: " ++ (showCard $ card_win)
                winPlayerRoundLambda new_hand_you new_hand_lambda card_you card_win mallet_play
    else do
        let card_lambda = greaterCard hl
        let new_hand_lambda = H $ delete card_lambda $ getMallet hl
        let new_mesa = addCardToTable card_lambda t
        putStrLn ""
        putStrLn $ "                Lambda tiene en la mano " ++ (show $ sizeHand hl) ++ " cartas sin jugar."
        putStrLn $ "                Carta jugada por Lambda: " ++ (showCard $ card_lambda)
        putStrLn $ "                Carta en la mesa: " ++ (showCard $ card_lambda)
        if sizeHand new_hand_lambda > 0 then do
            --mostrarMano hu
            let a = updateHandMallet hu m new_mesa
            checkHand (fst a) hu You
            let hand_you = fst a
            let mallet_play = snd a
            if (not (searchSuitHand hu $ getSuit $ head new_mesa) && not (searchSuitMallet m $ getSuit $ head new_mesa)) then do
                putStrLn "                  Usted ha cargado de la mesa."
                putStrLn "                  Su turno ha terminado. *La ronda fue ganada por Lambda*"
                nextRound hand_you new_hand_lambda mallet_play [] Lambda
            else do 
                let c = unsafePerformIO (pedirCarta hand_you)
                let card_you = playUserTwo hand_you (read c) new_mesa
                if card_you == (Card (Numeric 0) Oro) then
                    putStrLn "              Esa carta no es de la pinta que esta en la mesa."
                    ------ todo esto se debería repetir hasta que ingrese una carta correcta---------------------
                else do
                    let new_hand_you = H $ delete card_you $ getMallet hand_you
                    let table = addCardToTable card_you new_mesa
                    putStrLn $ "                Carta jugada por ti: " ++ (showCard $ card_you)
                    let card_win = winRound $ table
                    putStrLn $ "                Carta ganadora de la ronda: " ++ (showCard $ card_win)
                    winPlayerRoundLambda new_hand_you new_hand_lambda card_you card_win mallet_play
        else do
            putStrLn "                      **Perdiste la Partida, Lambda se ha quedado sin cartas.**"

-------------------------------------------------------- Main ----------------------------------------------------------

main :: IO ()
main = do
    gen <- getStdGen
    putStrLn "                      Bienvenido al Juego Carga la Burra."
    putStrLn ""
    putStrLn ""
    let rand_mallet = randomMallet gen mallet
    let a = createHands rand_mallet
    let table = addCardToTable (head $ drop 14 rand_mallet) []
    let hand_lambda = fst a
    let hand_you = snd a
    let mallet_play = drop 15 rand_mallet
    {- Prueba
    let rand_mallet = mallet
    let hand_lambda = hand1
    let hand_you = hand5
    let table = table_test
    let mallet_play = rand_mallet
    -}
    putStrLn "                                  Mesa"
    putStrLn $ "                                "++(showCard $ head table)
    playGame hand_you hand_lambda mallet_play table You
    gen' <- newStdGen
    putStrLn "                          Desea Jugar de Nuevo? S(Si) o N(No)."
    r <- getLine
    if r == "S" || r == "s" then main else putStrLn "                   Hasta luego"


