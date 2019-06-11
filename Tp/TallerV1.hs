type Set a = [a]
type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, Set Usuario) -- (usuario que publica, texto publicacion, likes)
type RedSocial = (Set Usuario, Set Relacion, Set Publicacion)


usuario1 = (1, "Ksaas")
usuario2 = (2,"J")
usuario3 = (3,"FALLE")
usuario4 = (4,"A")
usuario5 = (5,"B")
usuarioJ = (6,"C")
usuarioL = (8, "NN AA")


conjuntoRelaciones = [relacion4,relacion2,relacion1]
conjuntoRelaciones2 = [relacion1,relacion1]

publicacion1 = (usuario3, "JSAJ JAj jsaj ", [usuario2])
publicacion2 = (usuarioL, "ipsum loren", [usuario2,usuarioL])
publicacion3 = (usuarioL,"jasjsajj ajsja ja s", [])
publicacion4 = (usuario5, "sasas", [usuarioJ,usuario3, usuario2])

stalkergrid2 = ([usuario2], [relacion1, relacion2, relacion3], [publicacion1, publicacion2])
stalkergrid3 = ([], [relacion1, relacion2, relacion3], [])

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

-- Ejercicios

-- Dada una red social retorna un conjunto con los nombres de todos los usuarios.
nombresDeUsuarios :: RedSocial -> Set String
nombresDeUsuarios ([], _, _) = []
nombresDeUsuarios ((id, nombre):us, rs, ps) = nombre:nombresDeUsuarios (us, rs, ps)

-- Dada una red social y un usuario retorna el conjunto de amigos del mismo
amigosDe :: RedSocial -> Usuario -> Set Usuario
amigosDe (_, [], _) _ = []
amigosDe (us, (a,b):rs, ps) u
    | idDeUsuario a == idDeUsuario u = b:amigosDe (us, rs, ps) u
    | idDeUsuario b == idDeUsuario u = a:amigosDe (us, rs, ps) u
    | otherwise = amigosDe (us, rs, ps) u

-- Dada una red social y un usuario retorna la cantidad de amigos de dicho usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (us,rs,ps) u = length (amigosDe (us,rs,ps) u )

-- Dada una red social retorna el usuario con mas amigos. De existir más de uno devuelve cualquiera de ellos.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (a:[], _, _) = a
usuarioConMasAmigos (a:b:us, rs, ps)
    | cantidadDeAmigos (a:b:us, rs, ps) a >= cantidadDeAmigos (a:b:us, rs, ps) b = usuarioConMasAmigos (a:us, rs, ps)
    | otherwise = usuarioConMasAmigos (b:us, rs, ps)

-- Dada una red social retorna True si algún usuario tiene más de un millón de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([], _, _) = False
estaRobertoCarlos (u:us, rs, ps)
    | cantidadDeAmigos (u:us, rs, ps) u >= 1000000 = True
    | otherwise = estaRobertoCarlos (us, rs, ps)

-- Dada una red social y un usuario retorna el conjunto de publicaciones del mismo.
publicacionesDe :: RedSocial -> Usuario -> Set Publicacion
publicacionesDe (_, _, []) _ = []
publicacionesDe (us, rs, p:ps) u
    | idDeUsuario (usuarioDePublicacion p) == idDeUsuario u = p:publicacionesDe (us, rs, ps) u
    | otherwise = publicacionesDe (us, rs, ps) u

-- Dada una red social y un usuario retorna el conjunto de publicaciones a las que el usuario les dió like.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> Set Publicacion
publicacionesQueLeGustanA (_, _, []) _ = []
publicacionesQueLeGustanA (us, rs, p:ps) a
    | pusoLike a p = p:publicacionesQueLeGustanA (us, rs, ps) a
    | otherwise = publicacionesQueLeGustanA (us, rs, ps) a

-- Dada una red social y dos usuarios indica si les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones (_, _, []) _ _ = True
lesGustanLasMismasPublicaciones (us, rs, p:ps) a b
    | (pusoLike a p && not (pusoLike b p)) || (not (pusoLike a p) && pusoLike b p) = False
    | otherwise = lesGustanLasMismasPublicaciones (us, rs, ps) a b

-- Dada una red social y un usuario u, indica si existe un usuario que le puso like a todas las publicaciones de u.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([], _, _) _ = False
tieneUnSeguidorFiel (u:us, rs, ps) a
    | likeoTodas u (publicacionesDe (u:us, rs, ps) a) = True
    | otherwise = tieneUnSeguidorFiel (us, rs, ps) a

-- Dada una red social y dos usuarios, indica si existe una secuencia de usuarios relacionados para llegar del primero al segundo.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos (us,(u1,u2):rs,ps) a b
    | idDeUsuario b `elem` idsDeRelaciones (zipUsuarioRelaciones (us,(u1,u2):rs,ps) a) = True
    | otherwise = False

-- Guardia solo id de usuarios
idsDeRelaciones :: [Usuario] -> Set Integer
idsDeRelaciones [] = []
idsDeRelaciones ((a,b):xs) = a : idsDeRelaciones xs

-- Forma una lista con relaciones de un usuario
--zipUsuarioRelaciones :: RedSocial -> Usuario -> [(Integer,Int)]
zipUsuarioRelaciones (_,[],_) _ = []
zipUsuarioRelaciones (us,(u1,u2):rs,ps) u
    | (idDeUsuario u1 == idDeUsuario u ) = (snd(head(relaciones (us,(u1,u2):rs,ps)))) : zipUsuarioRelaciones (us,rs,ps) u
    | (idDeUsuario u2 == idDeUsuario u ) = (fst(head(relaciones (us,(u1,u2):rs,ps)))) : zipUsuarioRelaciones (us,rs,ps) u
    | otherwise = zipUsuarioRelaciones (us,rs,ps) u

relacion1 = (usuario4, usuario3)
relacion2 = (usuario5, usuario2)
relacion3 = (usuario2, usuarioL)
relacion4 = (usuario4, usuario2)
relacion5 = (usuario4, usuario1)
relacion6 = (usuario5, usuarioJ)
stalkergrid = ([usuario2,usuario3,usuario4,usuario5, usuarioJ, usuarioL], [relacion1, relacion2, relacion3,relacion4,relacion5, relacion6], [publicacion1])



--Dado un usuario y un conjunto de publicaciones indica si el usuario puso like a todas las publicaciones.
likeoTodas :: Usuario -> Set Publicacion -> Bool
likeoTodas _ [] = True
likeoTodas u (p:ps)
    | pusoLike u p = likeoTodas u ps
    | otherwise = False

--Dado un usuario y una publicación indica si el usuario puso like a la publicación.
pusoLike :: Usuario -> Publicacion -> Bool
pusoLike u p = pertenece u (likesDePublicacion p)

-- Dado un usuario a y un conjunto de usuarios indica si a pertenece al conjunto.
pertenece :: Usuario -> Set Usuario -> Bool
pertenece _ [] = False
pertenece a (u:us)
    | idDeUsuario a == idDeUsuario u = True
    | otherwise = pertenece a us