import EnigmaRotor
import Data.Char (chr, ord, toUpper)

patchBoard = Rotor 0 0 (trans patch)
reflector = Rotor 0 0 (trans reflect)
rots = [Rotor  (cor 'V') (cor 'Z') (trans rot1), Rotor  (cor 'Q') (cor 'Z') (trans rot2), Rotor  (cor 'I') (cor 'Z') (trans rot3)]

cor :: Char -> Int
cor ch = ord ch - ord 'A'

reflect = "IMETCGFRAYSQBZXWLHKDVUPOJN"
patch = "JWULCMNOHPQZYXIRADKEGVBTSF"

rot1 = "LPGSZMHAEOQKVXRFYBUTNICJDW"
rot2 = "SLVGBTFXJQOHEWIRZYAMKPCNDU"
rot3 = "CJGDPSHKTURAWZXFMYNQOBVLIE"
drot1 = decr (Rotor  (cor 'V') (cor 'Z') (trans rot1))
drot2 = decr (Rotor  (cor 'Q') (cor 'Z') (trans rot2))
drot3 = decr (Rotor  (cor 'I') (cor 'Z') (trans rot3))
dref  = decr reflector
dpatch = decr patchBoard

trans :: String -> [Int]
trans = transl

transl :: String -> [Int]
transl (x:xs) = (ord x - ord 'A'):transl xs
transl [] = []


decr :: Rotor -> Rotor
decr (Rotor pos tick config) = Rotor pos tick (while config)

while = cha 0

cha :: Int -> [Int] -> [Int]
cha a ints
  | a <= 25 = find a ints : cha (a+1) ints
  | otherwise = []

find :: Int -> [Int] -> Int
find a (x:xs) = if a == x then 0 else 1 + find a xs
find _ [] = 0

maxi :: Int
maxi = maximum' patchBoard
  where maximum' (Rotor _ _ ints) = maximum ints
main = do
  str <- getLine
  (retStr,rotors) <- enc (map toUpper str) rots []
  putStrLn retStr

enc :: String -> [Rotor] -> String -> IO (String, [Rotor])
enc (x:xs) rotors acc = do
  (chr,newRot) <- encryptStr x rotors
  enc xs newRot (chr:acc)
enc [] rotors acc = return ((reverse acc), rotors)

encryptStr :: Char -> [Rotor] -> IO (Char,[Rotor])
encryptStr x rotors = do 
  return (encryptChr x rotors, incr rotors)
  where incr (rot@(Rotor pos tick config):rs) = if pos == tick then (Rotor ((pos+1) `mod` (length config)) tick config):incr rs
                                                               else (Rotor ((pos+1) `mod` (length config)) tick config :rs)

encryptChr :: Char -> [Rotor] -> Char
encryptChr x rotors
  | symbol >= 0 && symbol <= maxi = chr $ encrypt (encryptRot (encrypt (encryptRot (encrypt symbol patchBoard) rotors) reflector) (reverse (map decr rotors))) (decr patchBoard) + ord 'A'
  | otherwise = error "Invalid Symbol"
  where symbol = ord x - ord 'A'

encryptRot :: Int -> [Rotor] -> Int
encryptRot sym (x:xs) = encryptRot (encrypt sym x) xs
encryptRot sym [] = sym

decryptRot :: Int -> [Rotor] -> Int
decryptRot sym (x:xs) = decryptRot (decrypt sym x) xs
decryptRot sym [] = sym
