

-- data ListaEnteros = Vacia | Add ListaEnteros

--data ListaCars = Vacia | Add Char Lista Cars

data Lista a = Vacia | Add a (Lista a) deriving Show

miLength::Lista a -> Int

miLength Vacia = 0
miLength (Add x xs) = 1 + (miLength xs)



--miFilter f [] = []
--miFilter f (x:xs) = if(f x == True) then x:miFilter f xs else miFilter f xs

miFilter:: (a -> Bool) -> Lista a -> Lista a 

miFilter f Vacia = Vacia
miFilter f (Add x xs) = if(f x == True) then (Add x (miFilter f xs)) else miFilter f xs

--miConcat
--miConcat [] = []
--miConcat (xs:xss) = xs++miConcat xss

miMasMas:: Lista a -> Lista a -> Lista a
miMasMas Vacia xs = xs
miMasMas (Add y ys)xs = Add y(miMasMas ys xs)


--miConcat:: Lista(Lista a) -> Lista a
--miConcat Vacia = Vacia
--miConcat (Add xs xss) = xs ++ miConcat xss



--miTake::Int->[a]->[a]
miTake:: Int -> Lista a -> Lista a
miTake 0 (Add x Vacia) = Vacia
miTake n Vacia = Vacia
miTake n (Add x xs) = (Add x (miTake (n-1) xs))


miInit:: Lista a -> Lista a
miInit (Add x Vacia) = Vacia
miInit (Add x xs) = (Add x (miInit xs))

miLast:: Lista a -> Lista a
miLast (Add x Vacia) = Add x Vacia
miLast (Add x xs) = miLast xs


miDrop:: Int -> Lista a -> Lista a 
miDrop 0 xs = xs
miDrop n Vacia = Vacia
miDrop n (Add x xs) = miDrop (n-1) xs




--miTakeWhile f (x:xs) = if(f x == False) then [] else x:miTakeWhile f xs
miTakeWhile:: (a -> Bool) -> Lista a -> Lista a
miTakeWhile f (Add x xs) = if(f x == False) then Vacia else (Add x (miTakeWhile f xs))



--miDropWhile f (x:xs) = if(f x == True) then miDropWhile f xs else x:xs
miDropWhile::(a -> Bool) -> Lista a -> Lista a
miDropWhile f (Add x xs) = if(f x == True) then miDropWhile f xs else (Add x xs)



--miMap f [] = []
--miMap f (x:xs) = (f x):miMap f xs
miMap:: (a -> b) -> Lista a -> Lista b
miMap f Vacia = Vacia
miMap f (Add x xs) = (Add (f x) (miMap f xs))


--miZip xs [] = []
--miZip [] ys = []
--miZip (x:xs) (y:ys) = (x,y):miZip xs ys

miZip:: Lista a -> Lista b -> Lista (a,b)
miZip xs Vacia = Vacia 
miZip Vacia ys = Vacia
miZip (Add x xs) (Add y ys) = (Add (x,y) (miZip xs ys)) 


--miZipWith f xs [] = []
--miZipWith f [] ys = []
--miZipWith f (x:xs) (y:ys) = (f x y):miZipWith f xs ys

miZipWith:: (a -> b -> c) -> Lista a -> Lista b -> Lista c
miZipWith f xs Vacia = Vacia 
miZipWith f Vacia ys = Vacia
miZipWith f (Add x xs) (Add y ys) = (Add (f x y) (miZipWith f xs ys))


