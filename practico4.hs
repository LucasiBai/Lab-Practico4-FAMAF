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

-- Props

propA :: [Figura] -> Bool
propA [] = True
propA (x:xs) = rojo x && propA xs

propB :: [Figura] -> Bool
propB [] = True
propB (x:xs) = tam x < 5 && propB xs

propC :: [Figura] -> Bool
propC [] = True
propC (x:xs) = (not (triangulo x) || rojo x) && propC xs

propD :: [Figura] -> Bool
propD [] = False
propD (x:xs) = cuadrado x && verde x || propD xs

propE :: [Figura] -> Bool
propE [] = True
propE (x:xs) = (not (circulo x) || (azul x && tam x < 10)) && propE xs

propF :: [Figura] -> Bool
propF [] = True
propF (x:xs) = (not (triangulo x) || not (azul x)) && propF xs

propG :: [Figura] -> Bool
propG [] = True
propG (x:xs) =((not (circulo x) || (not (amarillo x) && not (verde x))) && propG xs)


propH :: [Figura] -> Bool
propH [] = False
propH (x:xs) = (cuadrado x && tam x < 5) || propH xs

hayCirRojos :: [Figura] -> Bool
hayCirRojos [] = False
hayCirRojos (x:xs) = (circulo x && rojo x) || hayCirRojos xs

hayCuaRojos :: [Figura] -> Bool
hayCuaRojos [] = False
hayCuaRojos (x:xs) = (cuadrado x && rojo x) || hayCuaRojos xs

propI :: [Figura] -> Bool
propI xs = not (hayCirRojos xs) || hayCuaRojos xs
