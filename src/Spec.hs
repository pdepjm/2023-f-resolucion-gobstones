module Spec where
import PdePreludat
import Library
import Test.Hspec

tablero :: Tablero
tablero = inicializarTablero 3 5

correrTests :: IO ()
correrTests = hspec $ do
  describe "Punto 3" $ do
    it "Mover el cabezal en una direccion" $ do
      (cabezal . mover Arriba . mover Derecha $ tablero) `shouldBe` (2,2) --Indice 1

    it "Poner una bolita la agrega en el cabezal actual" $ do
      (celdaEnPosicion (1,2) . poner Roja . mover Arriba $ tablero)  `shouldMatchList` [Roja]

    it "Poner varias bolitas en una celda" $ do
      (celdaEnPosicion (2,1) . poner Azul . poner Roja . mover Derecha $ tablero)  `shouldMatchList` [Roja, Azul] --Uso should match list porque no me interesa el orden de los elementos

    it "Sacar una bolita de un color de una celda con bolitas de ese color" $ do
      (celdaEnPosicion (1,1) . sacar Roja . poner Verde . poner Roja . poner Roja $ tablero) `shouldMatchList` [Verde, Roja]

    it "Sacar una bolita de un color de una celda sin ese color no afecta la celda" $ do
      (celdaEnPosicion (1,1) . sacar Azul . poner Verde . poner Roja $ tablero) `shouldMatchList` [Verde, Roja]

  describe "Punto 4" $ do
    it "Repetir ejecuta sentencias n veces" $ do
      (celdaActual . repetir 3 [poner Verde, poner Azul] $ tablero) `shouldMatchList` [Verde, Azul, Verde, Azul, Verde, Azul]

    it "Alternativa ejecuta ciertas sentencias si condicion es verdadera" $ do
      ( celdaActual . alternativa (hayBolita Roja) [poner Azul] [poner Verde] . poner Roja $ tablero) `shouldMatchList` [Roja, Azul]

    it "Alternativa ejecuta ciertas sentencias si condicion es falsa" $ do
      ( celdaActual . alternativa (hayBolita Negra) [poner Azul] [poner Verde] . poner Roja $ tablero) `shouldMatchList` [Roja, Verde]

    it "'si' ejecuta sentencia si condicion es verdadera" $ do
      ( celdaActual . si (hayBolita Roja) [poner Azul] . poner Roja $ tablero) `shouldMatchList` [Roja, Azul]

    it "'si' no ejecuta sentencia si condicion es falsa" $ do
      ( celdaActual . si (hayBolita Negra) [poner Azul] . poner Roja $ tablero) `shouldMatchList` [Roja]

    it "'si no' ejecuta sentencia si condicion es falsa" $ do
      ( celdaActual . siNo (hayBolita Negra) [poner Azul] . poner Roja $ tablero) `shouldMatchList` [Roja, Azul]

    it "'si no' ejecuta sentencia si condicion es falsa" $ do
      ( celdaActual . siNo (hayBolita Roja) [poner Azul] . poner Roja $ tablero) `shouldMatchList` [Roja]

    it "'mientras' ejecuta sentencia mientras que condicion se cumpla" $ do
      ( cantidadBolitas Verde . mientras ( (<= 9) . cantidadBolitas Verde) [poner Verde] $ tablero) `shouldBe` 10

    it "'ir al borde' se mueve en direccion mientras que pueda hacerlo" $ do
      (cabezal . irAlBorde Arriba $ tablero) `shouldBe` (1,5)

  describe "Punto 5" $ do
    it "Puede moverse si no se cae del tablero" $ do
      tablero `shouldSatisfy` puedeMoverse Derecha

    it "No puede moverse si se cae del taberlo" $ do
      tablero `shouldNotSatisfy` puedeMoverse Izquierda

    it "Hay bolita si hay bolita de color en la celda actual" $ do
      poner Roja tablero `shouldSatisfy` hayBolita Roja
    
    it "No hay bolita si no hay bolita de color en la celda actual" $ do
      poner Verde tablero `shouldNotSatisfy` hayBolita Roja 

    it "Cantidad de bolitas de un color en celda actual" $ do
      (cantidadBolitas Verde . poner Azul . poner Verde . poner Verde $ tablero) `shouldBe` 2

  describe "Punto 6" $ do
    it "'programa' ejecuta sentencias sobre un tablero" $ do
      ( celdaActual . programa [poner Verde, mover Arriba, poner Azul] $ tablero) `shouldMatchList` [Azul] 