type Set a = [a]
type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, Set Usuario) -- (usuario que publica, texto publicacion, likes)
type RedSocial = (Set Usuario, Set Relacion, Set Publicacion)


-- Funciones basicas

usuarios :: RedSocial -> Set Usuario
usuarios (us, _, _) = us

relaciones :: RedSocial -> Set Relacion
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> Set Publicacion
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> Set Usuario
likesDePublicacion (_, _, us) = us

--usuario1 = (1, Ksaas)
usuario2 = (2,"J")
usuario3 = (3,"FALLE")
usuario4 = (4,"A")
usuario5 = (5,"B")
usuarioJ = (6,"C")
usuarioL = (8, "NN AA")

relacion1 = (usuario4, usuario3)
relacion2 = (usuario5, usuario2)
relacion3 = (usuario2, usuarioL)
relacion4 = (usuarioJ, usuario2)
conjuntoRelaciones = [relacion4,relacion2,relacion1]
conjuntoRelaciones2 = [relacion1,relacion1]

publicacion1 = (usuario3, "JSAJ JAj jsaj ", [usuario2])
publicacion2 = (usuarioL, "ipsum loren", [usuario2,usuarioL])

stalkergrid = ([usuario2,usuario3,usuario4,usuario5, usuarioJ, usuarioL], [relacion1, relacion2, relacion3], [publicacion1, publicacion2])
stalkergrid2 = ([usuario2], [relacion1, relacion2, relacion3], [publicacion1, publicacion2])
stalkergrid3 = ([], [relacion1, relacion2, relacion3], [publicacion1, publicacion2])


listatupla1 = [(1,2), (3,4), (5,6)]
--
--nombresDeUsuarios :: [(a, b)] -> [String]
--nombresDeUsuarios ([],_,_) = error "no tenes usuarios campeon"
--nombresDeUsuarios ((a,b) : xs) = b


-- Ejercicios
--
-- Dada una red social retorna un conjunto con los nombres de todos los usuarios.
--nombresDeUsuarios :: RedSocial -> Set String
--nombresDeUsuarios ([],_,_) = error "no hay users"
--nombresDeUsuarios (xs,rs,ps) | length xs == 1 = [nombreDeUsuario (head xs)]
--                             | otherwise = (nombreDeUsuario (head xs)) : (nombresDeUsuarios (tail xs,rs,ps))

--nombresDeUsuarios :: RedSocial -> Set String
--nombresDeUsuarios ([],_,_) = error "no hay users"
--nombresDeUsuarios ((x:xs),_,_) = noHayRepetidos (usu)
--
--noHayRepetidos :: [(Integer,String)] -> String
--noHayRepetidos (x:[]) = nombreDeUsuario x
--noHayRepetidos (x:xs)
--    | nombreDeUsuario x == nombreDeUsuario (head xs) = noHayRepetidos (x:tail xs)
--    | nombreDeUsuario x /= nombreDeUsuario (head xs) = (nombreDeUsuario xs) : noHayRepetidos (x:(tail xs))

--nombresDeUsuarios ([ ], _ ,_)= [ ]
--nombresDeUsuarios ( (x:xs), _ ,_)= nohayRepetidos (nombreDeUsuario x : nombresDeUsuarios (xs, _ ,_)
--
--
--nohayRepetidos:: [Char] -> [Char]
--nohayRepetidos (x : [])= [x]
--nohayRepetidos (x:y:ys)
--    |x==y = nohayRepetidos (x:ys)
--    |x=/y = y: nohayRepetidos (x:ys)

---- Dada una red social y un usuario retorna el conjunto de amigos del mismo
amigosDe :: RedSocial -> Usuario -> Set Usuario
amigosDe (_,[],_) _ = error "no hay relaciones"
amigosDe (us,rs,ps) x = usuarioComplementario rs x

-- Me devuelve la(s) tupla(s) complementaria de un Usuario x
usuarioComplementario :: [((Integer, String), (Integer, String))] -> (Integer, String) -> [(Integer, String)]
usuarioComplementario [] _ = []
usuarioComplementario rs x
      | id == idDeUsuario (fst (head(rs))) = snd (head rs) : usuarioComplementario (tail rs) x
      | id == idDeUsuario (snd (head(rs))) = fst (head rs) : usuarioComplementario (tail rs) x
      | otherwise = usuarioComplementario (tail rs) x
    where id = idDeUsuario x

---- Dada una red social y un usuario retorna la cantidad de amigos de dicho usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (us,rs,ps) u = length (amigosDe (us,rs,ps) u )
--
---- Dada una red social retorna el usuario con mas amigos. De existir más de uno devuelve cualquiera de ellos.
--usuarioConMasAmigos :: RedSocial -> Usuario
--usuarioConMasAmigos = undefined

-- Dada una red social retorna un conjunto con id de todos los usuarios.
idsDeUsuarios :: RedSocial -> Set Integer
idsDeUsuarios ([],_,_) = error "no hay users"
idsDeUsuarios (xs,rs,ps)
    | length xs == 1 = [idDeUsuario (head xs)]
    | otherwise = (idDeUsuario (head xs)) : (idsDeUsuarios (tail xs,rs,ps))

-- Forma una tupla con (idDeUsuario, Nº amigos)
zipUsuariosNumAmigos :: RedSocial -> [(Integer,Int)]
zipUsuariosNumAmigos ([],_,_) = error "no hay users"
zipUsuariosNumAmigos (us,rs,ps)
    | length us == 1 = [((idDeUsuario(head (listaDeUsuarios))), (cantidadDeAmigos (us,rs,ps) (head (listaDeUsuarios))))]
    | otherwise = [((idDeUsuario(head (listaDeUsuarios))), (cantidadDeAmigos (us,rs,ps) (head (listaDeUsuarios))))] ++ zipUsuariosNumAmigos ((tail us),rs,ps)
    where listaDeUsuarios = usuarios (us,rs,ps)
--    | length xs == 1 = cantidadDeAmigos (xs,rs,ps) (head (idDeUsuarios (xs,rs,ps)))
--    | otherwise = cantidadDeAmigos (xs,rs,ps) (head idDeUsuarios (xs,rs,ps)) : zipUsuariosNumAmigos (xs,rs,ps) (tail x)
--
-- Se queda con tupla (Id de usuario , nº de amigos ) cuyo nº amigos sea mayor al resto
masPopular :: [(Integer,Int)] -> [(Integer,Int)]
masPopular
--
---- Dada una red social retorna True si algún usuario tiene más de un millón de amigos
--estaRobertoCarlos :: RedSocial -> Bool
--estaRobertoCarlos = undefined
--
---- Dada una red social y un usuario retorna el conjunto de publicaciones del mismo.
--publicacionesDe :: RedSocial -> Usuario -> Set Publicacion
--publicacionesDe = undefined
--
---- Dada una red social y un usuario retorna el conjunto de publicaciones a las que el usuario les dió like.
--publicacionesQueLeGustanA :: RedSocial -> Usuario -> Set Publicacion
--publicacionesQueLeGustanA = undefined
--
---- Dada una red social y dos usuarios indica si les gustan las mismas publicaciones
--lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
--lesGustanLasMismasPublicaciones = undefined
--
---- Dada una red social y un usuario u, indica si existe un usuario que le puso like a todas las publicaciones de u.
--tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
--tieneUnSeguidorFiel = undefined
--
---- Dada una red social y dos usuarios, indica si existe una secuencia de usuarios relacionados para llegar del primero al segundo.
--existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
--existeSecuenciaDeAmigos = undefined

