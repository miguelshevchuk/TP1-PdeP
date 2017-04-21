import Text.Show.Functions

type Nombre = String
type Vida = Int
type Reaccion = (Protagonista -> Protagonista) -> Protagonista  -> Protagonista
type SeresQueridos = [Protagonista]

data Protagonista = Protagonista {nombre::Nombre, vida::Vida, reaccion::Reaccion, seresQueridos::SeresQueridos } deriving(Show)

instance Eq Protagonista where
  protagonista1 == protagonista2 =
    nombre protagonista1 == nombre protagonista2 &&
    vida protagonista1 == vida protagonista2


nuevaVidaProtagonista vidaNueva protagonista = protagonista{vida = vidaNueva}
nuevosSeresQueridosProtagonista amigosNuevos protagonista = protagonista{seresQueridos = amigosNuevos}

--Protagonistas EJERCICIO 1
daryl = Protagonista {nombre = "Daryl", vida = 55, seresQueridos = [carol], reaccion = sacrificarse}
maggie = Protagonista {nombre = "Maggie", vida = 100, seresQueridos = [carol, daryl, krilin], reaccion = caerse}
carol = Protagonista {nombre = "Carol", vida = 200, seresQueridos = [victor], reaccion = matarZombie}
victor = Protagonista {nombre = "Victor Sueiro", vida = 1, seresQueridos = [], reaccion = sacrificarse}
krilin = Protagonista {nombre = "Krilin", vida = 1, seresQueridos = [], reaccion = sacrificarse}

--Zombies EJERCICIO 2

zombieTranqui protagonista= nuevaVidaProtagonista (vida protagonista -10) protagonista
zombieConCasco protagonista= nuevaVidaProtagonista (div (vida protagonista) 2) protagonista
zombieSinDientes = id

zombieBuenaso hambre protagonista | (length.seresQueridos) protagonista > hambre = zombieSinDientes protagonista
                                  | otherwise = zombieConCasco protagonista

zombieReSacado protagonista = nuevosSeresQueridosProtagonista ((map zombieTranqui.map zombieConCasco.seresQueridos) protagonista) (zombieTranqui protagonista)

estaMuerto protagonista = (vida protagonista) == 0 && not(any (==krilin) (seresQueridos protagonista)) && not(any (==victor) (seresQueridos protagonista))

-- EJERCICIO 3: (zombieTranqui.zombieConCasco) carol
--Resultado: Protagonista (nombre = "Carol", vida = 90)

--ACCIONES EJERCICIO 4

matarZombie zombie = id
caerse zombie = zombie.zombie
sacrificarse zombie =  nuevaVidaProtagonista 0

-- EJERCICIO 5: sacrificarse zombieTranqui carol
--Resultado: Protagonista (nombre = "Carol", vida = 0)

-- EJERCICIO 6: (caerse  zombieConCasco.matarZombie zombieSinDientes) maggie
--Resultado: Protagonista (nombre = "Maggie", vida = 25)
