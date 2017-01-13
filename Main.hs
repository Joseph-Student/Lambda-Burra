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
    if null o || ((o >= "A") && (o <= "z")) || (not ((read o > 0) && (read o < 4))) then do
        putStrLn "**ERROR: DEBE INTRODUCIR UNA OPCION VALIDA.**"
        putStrLn ""
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
            putStrLn "1. El primero en jugar es Ud. debe jugar una carta de la pinta de la mesa."
            putStrLn "2. La mano la gana el jugador que lance la carta con mayor valor."
            putStrLn "3. El ganador lanza una carta sobre la mesa en la sig. ronda."
            putStrLn "4. Si un jugador no tiene carta de la pinta de la mesa, debe cargar del mazo."
            putStrLn "5. Si se acaba el mazo al cargar, debe entonces cargar recoger la mesa."
            putStrLn "6. El jugador que primero se deshaga de todas sus cartas, gana la partida."
            r <- getLine
            main
        else
            putStrLn "                   ¡¡¡Gracias por jugar. Hasta luego!!!"