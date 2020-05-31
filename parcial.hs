import Text.Show.Functions()

main :: IO ()
main = return ()

type Habilidad = String
type Deseo = Chico -> Chico
type Condicion = Chico -> Bool

data Chico = CrearChico {
    nombre :: String,
    edad :: Int,
    habilidades :: [Habilidad],
    deseos :: [Deseo]
} deriving (Show)

data Chica = CrearChica {
    nombreChica :: String,
    condicion :: Condicion
}

--- Parte A

timmy :: Chico
timmy = CrearChico "Timmy" 14 ["cantar","cocinar","enamorar"] [serMayor, serGrosoEnNeedForSpeed]

jorge :: Chico
jorge = CrearChico "jorge" 14 ["ser un supermodelo noruego"] [serMayor, serGrosoEnNeedForSpeed]

cristina :: Chica
cristina = CrearChica "cristina" sabeCocinar 

aprenderHabilidades :: [Habilidad] -> Deseo
aprenderHabilidades listaHabilidades unChico = unChico { habilidades = habilidades unChico ++ listaHabilidades}

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed unChico = unChico {habilidades = habilidades unChico ++ versionesNeedForSpeed}

versionesNeedForSpeed :: [Habilidad]
versionesNeedForSpeed = map agregarString [1..]

agregarString :: Int -> String
agregarString unNumero = "jugar need for speed " ++ (show unNumero)

serMayor :: Deseo
serMayor unChico = cambiarEdad 18 unChico

cambiarEdad :: Int -> Deseo
cambiarEdad unaEdad unChico = unChico { edad = unaEdad }

wanda :: Deseo
wanda unChico = madurar.cumplirPrimerDeseo $ unChico

madurar :: Deseo
madurar unChico = cambiarEdad (edad unChico +1) unChico

cumplirPrimerDeseo :: Deseo 
cumplirPrimerDeseo unChico = (head.deseos) unChico $ unChico

cosmo :: Deseo 
cosmo unChico = desmadurar unChico

desmadurar :: Deseo
desmadurar unChico = cambiarEdad (div (edad unChico) 2) unChico

muffin :: Deseo
muffin unChico = concederTodosLosDeseos unChico

concederTodosLosDeseos :: Deseo
concederTodosLosDeseos unChico = foldl1 (.) (deseos unChico) $ unChico

--- Parte B

tieneHabilidad :: Habilidad -> Condicion
tieneHabilidad unaHabilidad unChico = elem unaHabilidad (habilidades unChico)

sabeCocinar :: Condicion
sabeCocinar unChico = tieneHabilidad "cocinar" unChico

esSuperMaduro :: Condicion
esSuperMaduro unChico = edad unChico >= 18 && tieneHabilidad "manejar" unChico

quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA _ [losPretendientes] = losPretendientes
quienConquistaA unaChica (cabeza:cola) 
    | condicion unaChica cabeza = cabeza
    | otherwise = quienConquistaA unaChica cola

{--quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA unaChica losPretendientes 
    | length losPretendientes == 1 = head losPretendientes
    | condicion unaChica (head.take 1 $ losPretendientes) = (head.take 1 $ losPretendientes)
    | otherwise = quienConquistaA unaChica (tail losPretendientes)
--}

-- Parte C

infractoresDaRules :: [Chico] -> [Chico]
infractoresDaRules listaChicos = filter tieneDeseosProhibidos listaChicos

tieneDeseosProhibidos :: Chico -> Bool
tieneDeseosProhibidos unChico = any esHabilidadProhibida (tomarCincoPrimerasHabilidades unChico)

esHabilidadProhibida :: Habilidad -> Bool
esHabilidadProhibida "matar" = True 
esHabilidadProhibida "enamorar" = True
esHabilidadProhibida "dominar el mundo" = True
esHabilidadProhibida _ = False

tomarCincoPrimerasHabilidades :: Chico -> [Habilidad]
tomarCincoPrimerasHabilidades unChico = take 5.habilidades $ unChico
