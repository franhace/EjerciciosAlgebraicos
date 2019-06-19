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

-- Ejercicios

-- Dada una red social retorna un conjunto con los nombres de todos los usuarios.
nombresDeUsuarios :: RedSocial -> Set String
nombresDeUsuarios ([], _, _) = []
nombresDeUsuarios (u:us, rs, ps) = eliminarNombresRepetidos (nombreDeUsuario u:nombresDeUsuarios (us, rs, ps))

eliminarNombresRepetidos :: Set String -> Set String
eliminarNombresRepetidos [] = []
eliminarNombresRepetidos (n:ns)
    | pertenece n ns = eliminarNombresRepetidos ns
    | otherwise = n:eliminarNombresRepetidos ns

pertenece :: (Eq a) => a -> [a] -> Bool
pertenece _ [] = False
pertenece n (x:xs)
    | n == x = True
    | otherwise = pertenece n xs

-- Dada una red social y un usuario retorna el conjunto de amigos del mismo
amigosDe :: RedSocial -> Usuario -> Set Usuario
amigosDe (_, [], _) _ = []
amigosDe (us, (a,b):rs, ps) u
    | idDeUsuario a == idDeUsuario u = b:amigosDe (us, rs, ps) u
    | idDeUsuario b == idDeUsuario u = a:amigosDe (us, rs, ps) u
    | otherwise = amigosDe (us, rs, ps) u

-- Dada una red social y un usuario retorna la cantidad de amigos de dicho usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (us, rs, ps) u = length (amigosDe (us, rs, ps) u)

-- Dada una red social retorna el usuario con mas amigos. De existir más de uno devuelve cualquiera de ellos.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (a:[], _, _) = a
usuarioConMasAmigos (a:b:us, rs, ps)
    | cantidadDeAmigos (a:b:us, rs, ps) a >= cantidadDeAmigos (a:b:us, rs, ps) b = usuarioConMasAmigos (a:us, rs, ps)
    | otherwise = usuarioConMasAmigos (b:us, rs, ps)

-- Dada una red social retorna True si algún usuario tiene más de un millón de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos (us, rs, ps) = cantidadDeAmigos (us, rs, ps) (usuarioConMasAmigos (us, rs, ps)) >= 1000000

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
    | length (publicacionesDe (u:us, rs, ps) a) == 0 = False
    | likeoTodas u (publicacionesDe (u:us, rs, ps) a) = True
    | otherwise = tieneUnSeguidorFiel (us, rs, ps) a

-- Dada una red social y dos usuarios, indica si existe una secuencia de usuarios relacionados para llegar del primero al segundo.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red a b = existeSecuenciaDeAmigosAux red a b [a]

-- (Función Auxiliar) Idem existeSecuenciaDeAmigos pero pide un parámetro más del tipo Set Usuario donde se van almacenando los amigos.
existeSecuenciaDeAmigosAux :: RedSocial -> Usuario -> Usuario -> Set Usuario -> Bool
existeSecuenciaDeAmigosAux red a b vs
    | pertenece b vs = True
    | incluido (vs ++ todosLosAmigos red vs) vs = False
    | otherwise = existeSecuenciaDeAmigosAux red a b (eliminarUsuariosRepetidos (vs ++ todosLosAmigos red vs))

-- Dada una red social y un conjunto de usuarios, retorna una lista con los amigos de cada usuario del conjunto.
todosLosAmigos :: RedSocial -> Set Usuario -> [Usuario]
todosLosAmigos _ [] = []
todosLosAmigos red (u:us) = amigosDe red u ++ todosLosAmigos red us

-- Dado una lista de usuarios, retorna un conjunto con los usuarios de la lista.
eliminarUsuariosRepetidos :: [Usuario] -> Set Usuario
eliminarUsuariosRepetidos [] = []
eliminarUsuariosRepetidos (u:us)
    | perteneceUsuario u us = eliminarUsuariosRepetidos us
    | otherwise = u:eliminarUsuariosRepetidos us

--Dado un usuario y un conjunto de publicaciones indica si el usuario puso like a todas las publicaciones.
likeoTodas :: Usuario -> Set Publicacion -> Bool
likeoTodas _ [] = True
likeoTodas u (p:ps)
    | pusoLike u p = likeoTodas u ps
    | otherwise = False

--Dado un usuario y una publicación indica si el usuario puso like a la publicación.
pusoLike :: Usuario -> Publicacion -> Bool
pusoLike u p = perteneceUsuario u (likesDePublicacion p)

-- Dado un usuario a y un conjunto de usuarios indica si a perteneceUsuario al conjunto.
perteneceUsuario :: Usuario -> Set Usuario -> Bool
perteneceUsuario _ [] = False
perteneceUsuario a (u:us)
    | idDeUsuario a == idDeUsuario u = True
    | otherwise = perteneceUsuario a us

-- Dados un conjunto a de usuarios y un conjunto b de usuarios, indica si el conjunto a esta contenido en el conjunto b.
incluido :: Set Usuario -> Set Usuario -> Bool
incluido [] _ = True
incluido (a:as) bs = perteneceUsuario a bs && incluido as bs

-- Casos de test
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1)
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario4])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario4])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])

usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_1, publicacion1_2, publicacion1_3, publicacion1_4, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)