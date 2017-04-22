import Text.Show.Functions

type Nombre = String
type Vida = Int
type Zombie = Protagonista -> Protagonista
type Reaccion = Zombie -> Protagonista  -> Protagonista
type AtaqueHorda = [Zombie] -> Protagonista -> Protagonista
type SeresQueridos = [Protagonista]

data Protagonista = Protagonista {nombre::Nombre, vida::Vida, reaccion::Reaccion, seresQueridos::SeresQueridos } deriving(Show)

instance Eq Protagonista where
  protagonista1 == protagonista2 =
    nombre protagonista1 == nombre protagonista2 &&
    vida protagonista1 == vida protagonista2

nuevaVidaProtagonista vidaNueva protagonista = protagonista{vida = vidaNueva}
nuevosSeresQueridosProtagonista amigosNuevos protagonista = protagonista{seresQueridos = amigosNuevos}

--Protagonistas EJERCICIO 1
daryl = Protagonista {nombre = "Daryl", vida = 55, seresQueridos = [carol], reaccion = comerSemillas 30}
maggie = Protagonista {nombre = "Maggie", vida = 100, seresQueridos = [carol, daryl, krilin], reaccion = caerse}
carol = Protagonista {nombre = "Carol", vida = 200, seresQueridos = [victor], reaccion = matarZombie}
--Protagonistas Entrega 2
victor = Protagonista {nombre = "Victor Sueiro", vida = 1, seresQueridos = [], reaccion = sacrificarse}
krilin = Protagonista {nombre = "Krilin", vida = 1, seresQueridos = [], reaccion = sacrificarse}

--Zombies EJERCICIO 2

zombieTranqui::Zombie
zombieTranqui protagonista= nuevaVidaProtagonista (vida protagonista -10) protagonista
zombieConCasco::Zombie
zombieConCasco protagonista= nuevaVidaProtagonista (div (vida protagonista) 2) protagonista
zombieSinDientes::Zombie
zombieSinDientes = id

--Zombies Entrega 2}
zombieAstuto::Int -> Zombie
zombieAstuto hambre protagonista
  | (length.seresQueridos) protagonista > hambre = zombieSinDientes protagonista
  | otherwise = zombieConCasco protagonista

zombieReSacado::Zombie
zombieReSacado protagonista = nuevosSeresQueridosProtagonista ((map zombieTranqui.map zombieConCasco.seresQueridos) protagonista) (zombieTranqui protagonista)

--ACCIONES EJERCICIO 4
matarZombie::Reaccion
matarZombie _ = id
caerse::Reaccion
caerse zombie = zombie.zombie
sacrificarse::Reaccion
sacrificarse _ =  pelearseConProtagonistas [krilin, victor].nuevaVidaProtagonista 0
--Accion Entrega 2
comerSemillas::Int -> Reaccion
comerSemillas cantidad zombie protagonista  | estaMuerto (zombie protagonista) = zombie protagonista
                                            | cantidad <= 10 = nuevaVidaProtagonista 100 protagonista
                                            | otherwise = sacrificarse zombie protagonista



-- FUNCION Para pelearse

pelearseConProtagonistas [] protagonista = protagonista
pelearseConProtagonistas (x:xs) protagonista = pelearseConProtagonistas xs ((nuevosSeresQueridosProtagonista.filter (/=x).seresQueridos) protagonista protagonista)

-- HORDAS

ataqueHorda::AtaqueHorda
ataqueHorda zombies protagonista = foldl (flip ($)) protagonista (map (reaccion protagonista) zombies)

--Ejercicio 4 Entrega 2
estaMuerto protagonista = (vida protagonista) <= 0 && not(any (==krilin) (seresQueridos protagonista)) && not(any (==victor) (seresQueridos protagonista))

-- CONSULTAS ENTREGA 1

-- EJERCICIO 3: (zombieTranqui.zombieConCasco) carol
--Resultado: Protagonista (nombre = "Carol", vida = 90)

-- EJERCICIO 5: sacrificarse zombieTranqui carol
--Resultado: Protagonista (nombre = "Carol", vida = 0)

-- EJERCICIO 6: (caerse  zombieConCasco.matarZombie zombieSinDientes) maggie
--Resultado: Protagonista (nombre = "Maggie", vida = 25)

-- CONSULTAS ENTREGA 2
{-
1) pelearseConProtagonistas [daryl, carol, victor] maggie

Resultado: Protagonista {nombre = "Maggie", vida = 100, reaccion = <function>,
seresQueridos = [Protagonista {nombre = "Krilin", vida = 1, reaccion = <function>, seresQueridos = []}]}

2)a) caerse (zombieAstuto 5) maggie

Resultado: Protagonista {nombre = "Maggie", vida = 25, reaccion = <function>, seresQueridos
 = [Protagonista {nombre = "Carol", vida = 200, reaccion = <function>, seresQuer
idos = [Protagonista {nombre = "Victor Sueiro", vida = 1, reaccion = <function>,
 seresQueridos = []}]},Protagonista {nombre = "Daryl", vida = 55, reaccion = <fu
nction>, seresQueridos = [Protagonista {nombre = "Carol", vida = 200, reaccion =
 <function>, seresQueridos = [Protagonista {nombre = "Victor Sueiro", vida = 1,
reaccion = <function>, seresQueridos = []}]}]},Protagonista {nombre = "Krilin",
vida = 1, reaccion = <function>, seresQueridos = []}]}

  b) caerse (zombieAstuto 2) maggie

Resultado:   Protagonista {nombre = "Maggie", vida = 100, reaccion = <function>, seresQuerido
  s = [Protagonista {nombre = "Carol", vida = 200, reaccion = <function>, seresQue
  ridos = [Protagonista {nombre = "Victor Sueiro", vida = 1, reaccion = <function>
  , seresQueridos = []}]},Protagonista {nombre = "Daryl", vida = 55, reaccion = <f
  unction>, seresQueridos = [Protagonista {nombre = "Carol", vida = 200, reaccion
  = <function>, seresQueridos = [Protagonista {nombre = "Victor Sueiro", vida = 1,
   reaccion = <function>, seresQueridos = []}]}]},Protagonista {nombre = "Krilin",
   vida = 1, reaccion = <function>, seresQueridos = []}]}

3)  sacrificarse zombieConCasco maggie

Resultado: Protagonista {nombre = "Maggie", vida = 0, reaccion = <function>, seresQueridos
= [Protagonista {nombre = "Carol", vida = 200, reaccion = <function>, seresQueri
dos = [Protagonista {nombre = "Victor Sueiro", vida = 1, reaccion = <function>,
seresQueridos = []}]},Protagonista {nombre = "Daryl", vida = 55, reaccion = <fun
ction>, seresQueridos = [Protagonista {nombre = "Carol", vida = 200, reaccion =
<function>, seresQueridos = [Protagonista {nombre = "Victor Sueiro", vida = 1, r
eaccion = <function>, seresQueridos = []}]}]}]}

4) ataqueHorda [zombieTranqui, zombieAstuto 20, zombieReSacado, zombieAstuto 1, zombieTranqui] maggie

Resultado: Protagonista {nombre = "Maggie", vida = -20, reaccion = <function>, seresQuerido
s = [Protagonista {nombre = "Carol", vida = 35, reaccion = <function>, seresQuer
idos = [Protagonista {nombre = "Victor Sueiro", vida = 1, reaccion = <function>,
 seresQueridos = []}]},Protagonista {nombre = "Daryl", vida = -2, reaccion = <fu
nction>, seresQueridos = [Protagonista {nombre = "Carol", vida = 200, reaccion =
 <function>, seresQueridos = [Protagonista {nombre = "Victor Sueiro", vida = 1,
reaccion = <function>, seresQueridos = []}]}]},Protagonista {nombre = "Krilin",
vida = -15, reaccion = <function>, seresQueridos = []}]}

5) (map nombre.filter estaMuerto.seresQueridos.ataqueHorda [zombieTranqui, zombieAstuto 20, zombieReSacado, zombieAstuto 1, zombieTranqui]) maggie

Resultado: ["Daryl","Krilin"]

-}

--BONUS
generarListaEterna (x:xs) repeticion = x
