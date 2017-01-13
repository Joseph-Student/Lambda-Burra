module Main where

import LambdaBurra
import System.IO.Unsafe
import System.Random

------------------------------------------- Crea un mazo aleatorio -----------------------------------------------------
randomMallet :: StdGen -> Mallet -> Mallet
randomMallet gen m = let (r,g) = randomR (0, sizeMallet m-1) gen
    in (m !! r: if sizeMallet m > 1 then randomMallet g $ delete (m !! r) m else [])


-------------------------------------- Crea las manos de los jugadores -------------------------------------------------
createHands :: Mallet -> (Hand,Hand)
createHands m = (H [x|x<-m,y<-[1,3..13],checkIndex x m y], H [x|x<-m,y<-[0,2..12],checkIndex x m y])

-------------------------------------- 
optionGame :: IO String
optionGame = do
    putStrLn "Indique la opcion de su preferencia:"
    putStrLn "1. Jugar"
    putStrLn "2. Leer reglas de Carga Lambda-Burra"
    putStrLn "3. Salir"
    o <- getLine
    if ((read o /= 1) && (read o /= 2) && (read o /= 3)) then do
        putStrLn "**ERROR: DEBE INTRODUCIR UNA OPCION VALIDA.**"
        optionGame
    else
        return o

main :: IO ()
main = do
    gen <- getStdGen
    putStrLn ""
    putStrLn ""
    putStrLn "---------------------BIENVENIDO AL JUEGO CARGA LAMBDA-BURRA---------------------"
    putStrLn ""
    putStrLn ""
    let rand_mallet = randomMallet gen mallet
    let a = createHands rand_mallet
    let table = addCardToTable (rand_mallet !! 14) []
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
    let o = unsafePerformIO optionGame
    if o == "1" then do
        putStrLn ""
        putStrLn "    -------------------------EL JUEGO HA EMPEZADO-------------------------"
        putStrLn ""
        putStrLn "                               Carta en la Mesa:"
        putStrLn $ "                                   "++showCard (head table)
        playGame hand_you hand_lambda mallet_play table You
        gen' <- newStdGen
        putStrLn ""
        putStrLn "                        Desea Jugar de Nuevo? S(Si) o N(No)."
        r <- getLine
        if r == "S" || r == "s" then main else putStrLn "                   ¡¡¡Gracias por jugar. Hasta luego!!!"
    else do
        if o == "2" then do
            putStrLn ""
            putStrLn "El primero en jugar siempre es Ud. que debe jugar una carta de la misma pinta de la baraja de la mesa. La mano la gana el jugador"
                {-que lance la carta con mayor valor y, por consiguiente, debe lanzar una carta sobre la mesa para continuar el juego. Si un jugador
                en su turno no tiene carta alguna de la «pinta» de la mesa, debe «cargar» del mazo de cartas restantes hasta encontrar la primera
                ocurrencia de una carta de la misma «pinta» y jugarla; en caso de que se acabe el mazo, y al que le toca el turno no tiene una carta
                para lanzar, debe entonces recoger todas las cartas lanzadas a la mesa. El jugador que primero se deshaga de todas sus cartas (se
                quede sin cartas), gana la partida."-}
            r <- getLine
            main
        else
            putStrLn "                   ¡¡¡Gracias por jugar. Hasta luego!!!"