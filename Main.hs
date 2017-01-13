module Main where

import LambdaBurra
import System.Random

------------------------------------------- Crea un mazo aleatorio -----------------------------------------------------
randomMallet :: StdGen -> Mallet -> Mallet
randomMallet gen m = let (r,g) = randomR (0, sizeMallet m-1) gen
    in (m !! r: if sizeMallet m > 1 then randomMallet g $ delete (m !! r) m else [])


-------------------------------------- Crea las manos de los jugadores -------------------------------------------------
createHands :: Mallet -> (Hand,Hand)
createHands m = (H [x|x<-m,y<-[1,3..13],checkIndex x m y], H [x|x<-m,y<-[0,2..12],checkIndex x m y])

main :: IO ()
main = do
    gen <- getStdGen
    putStrLn ""
    putStrLn ""
    putStrLn "-----------------------BIENVENIDO AL JUEGO CARGA LA BURRA-----------------------"
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
    putStrLn "                                  Mesa"
    putStrLn $ "                                "++showCard (head table)
    playGame hand_you hand_lambda mallet_play table You
    gen' <- newStdGen
    putStrLn "                          Desea Jugar de Nuevo? S(Si) o N(No)."
    r <- getLine
    if r == "S" || r == "s" then main else putStrLn "                   Hasta luego"

