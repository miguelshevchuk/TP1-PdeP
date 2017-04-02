import Text.Show.Functions

type Nombre = String
type Vida = Float
type Ataque = Float -> Float

data Protagonista = Protagonista Nombre Vida deriving(Show)

getVida (Protagonista _ vida) = vida
getNombre (Protagonista nombre _) = nombre
setNombre (Protagonista _ vida) nombreNuevo = Protagonista nombreNuevo vida
setVida (Protagonista nombre _) vidaNueva = Protagonista nombre vidaNueva

data Zombie = Zombie Ataque deriving(Show)
getAtaque (Zombie ataque) = ataque

--Protagonistas EJERCICIO 1
daryl = Protagonista "Daryl" 55
maggie = Protagonista "Maggie" 100
carol = Protagonista "Carol" 200

-- Ataques Zombies
ataqueTranqui vida = vida -10
ataquePolenta vida = vida / 2
ataqueNulo = id

--Zombies EJERCICIO 2
zombieTranqui = Zombie ataqueTranqui
zombieConCasco = Zombie ataquePolenta
zombieSinDientes = Zombie ataqueNulo

ataqueAProtagonista protagonista zombie = setVida protagonista ((getAtaque zombie) (getVida protagonista))

-- EJERCICIO 3: ataqueAProtagnista (ataqueAProtagonista carol zombieConCasco) zombieTranqui


--ACCIONES EJERCICIO 4

matarZombie protagonista zombie = protagonista
caerse protagonista zombie = ataqueAProtagonista (ataqueAProtagonista protagonista zombie) zombie
sacrificarse (Protagonista nombre vida) zombie =  Protagonista nombre 0

-- EJERCICIO 5: sacrificarse carol zombieTranqui

-- EJERCICIO 6: caerse (matarZombie maggie zombieSinDientes) zombieConCasco
