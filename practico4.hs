-- Project Config
data Color = Rojo | Amarillo | Azul | Verde
  deriving(Show, Eq)

data Forma = Triangulo | Cuadrado | Rombo | Circulo
  deriving(Show, Eq)

type Figura = (Forma, Color, Int)

-- Elementals Functions 

pert :: Eq a => a -> [a] -> Bool
pert e [] = False
pert e (x:xs) = (e == x) || pert e xs

rojo :: Figura -> Bool
rojo (f, c, t) = c == Rojo

verde :: Figura -> Bool
verde (f, c, t) = c == Verde

azul :: Figura -> Bool
azul (f, c, t) = c == Azul

amarillo :: Figura -> Bool
amarillo (f, c, t) = c == Amarillo

circulo :: Figura -> Bool
circulo (f, c, t) = f == Circulo

rombo :: Figura -> Bool
rombo (f, c, t) = f == Rombo

cuadrado :: Figura -> Bool
cuadrado (f, c, t) = f == Cuadrado

triangulo :: Figura -> Bool
triangulo (f, c, t) = f == Triangulo

tam :: Figura -> Int
tam (f, c, t) = t
