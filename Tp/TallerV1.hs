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

usuario1 = (1,"K")
usuario2 = (2,"J")
usuario3 = (3,"L")
usuario4 = (4,"A")
usuario5 = (5,"B")
usuarioJ = (6,"C")
usuarioL = (8, "NN AA")

relacion1 = (usuario1, usuario3)
relacion2 = (usuario3, usuario2)
relacion3 = (usuario2, usuarioL)

publicacion1 = (usuario3, "JSAJ JAj jsaj ", [usuario2])
publicacion2 = (usuarioL, "ipsum loren", [usuario1,usuarioL])

stalkergrid = ([usuario1,usuario2,usuario3,usuario4,usuario5, usuarioJ, usuarioL], [relacion1, relacion2, relacion3], [publicacion1, publicacion2])
stalkergrid2 = ([usuario1], [relacion1, relacion2, relacion3], [publicacion1, publicacion2])


listatupla1 = [(1,2), (3,4), (5,6)]
--
--soloTuplas :: [a] -> (Integer,Integer)
--soloTuplas [] = ()

-- Ejercicios
--
-- Dada una red social retorna un conjunto con los nombres de todos los usuarios.
--nombresDeUsuarios :: RedSocial -> [(Integer, String)]
--nombresDeUsuarios ([],_,_) = error "No hay usuarios"
--nombresDeUsuarios xs | length (usuarios xs) == 1 = nombreDeUsuario ( usuarios xs)
--                     | otherwise = nombresDeUsuarios xs
--
---- Dada una red social y un usuario retorna el conjunto de amigos del mismo
--amigosDe :: RedSocial -> Usuario -> Set Usuario
--amigosDe = undefined
--
---- Dada una red social y un usuario retorna la cantidad de amigos de dicho usuario
--cantidadDeAmigos :: RedSocial -> Usuario -> Int
--cantidadDeAmigos = undefined
--
---- Dada una red social retorna el usuario con mas amigos. De existir más de uno devuelve cualquiera de ellos.
--usuarioConMasAmigos :: RedSocial -> Usuario
--usuarioConMasAmigos = undefined
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

