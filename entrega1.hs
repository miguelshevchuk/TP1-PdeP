import Text.Show.Functions


type Nombre = String
type Vida = Int
type Ataque = Protagonista -> Protagonista

data Protagonista = Protagonista Nombre Vida deriving(Show)
data Zombie = Zombie Ataque deriving(Show)

-- GETTERS Y SETTERS PROTAGONISTA
getVidaProtagonista (Protagonista _ vida) = vida
getNombreProtagonista (Protagonista nombre _) = nombre
setNombreProtagonista (Protagonista _ vida) nombreNuevo = Protagonista nombreNuevo vida
setVidaProtagonista vidaNueva (Protagonista nombre _) = Protagonista nombre vidaNueva

-- GETTERS ZOMBIE
getAtaqueZombie (Zombie ataque) = ataque

--Protagonistas EJERCICIO 1
daryl = Protagonista "Daryl" 55
maggie = Protagonista "Maggie" 100
carol = Protagonista "Carol" 200

--Zombies EJERCICIO 2
zombieTranqui = Zombie ataqueTranqui
zombieConCasco = Zombie ataqueZombieConCasco
zombieSinDientes = Zombie ataqueSinDientes

-- Ataques Zombies
ataqueTranqui protagonista = setVidaProtagonista (getVidaProtagonista protagonista -10) protagonista
ataqueZombieConCasco protagonista= setVidaProtagonista (div (getVidaProtagonista protagonista) 2) protagonista
ataqueSinDientes = id

-- Funcion -> pasar zombie y protagonista
ataqueAProtagonista = getAtaqueZombie

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
