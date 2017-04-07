import Text.Show.Functions

type Nombre = String
type Vida = Int
type Ataque = Protagonista -> Protagonista

data Protagonista = Protagonista {nombre::Nombre, vida::Vida } deriving(Show)
data Zombie = Zombie {ataqueZombie::Ataque} deriving(Show)

setVidaProtagonista vidaNueva protagonista = protagonista{vida = vidaNueva}

--Protagonistas EJERCICIO 1
daryl = Protagonista {nombre = "Daryl", vida = 55}
maggie = Protagonista {nombre = "Maggie", vida = 100}
carol = Protagonista {nombre = "Carol", vida = 200}

--Zombies EJERCICIO 2
zombieTranqui = Zombie {ataqueZombie = ataqueTranqui}
zombieConCasco = Zombie {ataqueZombie = ataqueZombieConCasco}
zombieSinDientes = Zombie {ataqueZombie = ataqueSinDientes}

-- Ataques Zombies
ataqueTranqui protagonista = setVidaProtagonista (vida protagonista -10) protagonista
ataqueZombieConCasco protagonista= setVidaProtagonista (div (vida protagonista) 2) protagonista
ataqueSinDientes = id

-- Funcion -> pasar zombie y protagonista
ataqueAProtagonista = ataqueZombie

-- EJERCICIO 3: ataqueAProtagnista zombieTranqui (ataqueAProtagonista zombieConCasco carol)

--ACCIONES EJERCICIO 4

matarZombie protagonista zombie = protagonista
caerse protagonista zombie = ataqueAProtagonista zombie (ataqueAProtagonista zombie protagonista)
sacrificarse protagonista zombie =  setVidaProtagonista 0 protagonista

-- Segunda opcion

-- matarZombie = id
-- caerse protagonista zombie = ataqueAProtagonista zombie (ataqueAProtagonista zombie protagonista)
-- sacrificarse =  setVidaProtagonista 0

-- EJERCICIO 5: sacrificarse carol zombieTranqui

-- EJERCICIO 6: caerse (matarZombie maggie zombieSinDientes) zombieConCasco
