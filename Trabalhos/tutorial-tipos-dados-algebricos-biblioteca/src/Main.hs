data Midia = Livro { titulo :: String, autor :: String, numPaginas :: Int }
           | Filme { titulo :: String, diretor :: String, duracao :: Int }
           | Musica { titulo :: String, artista :: String, duracao :: Int }

duracaoTotal :: [Midia] -> Int
duracaoTotal [] = 0
duracaoTotal (m:ms) = case m of
    Livro {} -> duracaoTotal ms
    Filme {duracao = d} -> d + duracaoTotal ms
    Musica {duracao = d} -> d + duracaoTotal ms

main = do
    let biblioteca = [ Livro "A Revolução dos Bichos" "George Orwell" 300
                     , Filme "Interestelar" "Christopher Nolan" 136
                     , Musica "Stairway to Heaven" "Led Zeppelin" 354 ]
    
    putStrLn $ "Duração total da biblioteca: " ++ show (duracaoTotal biblioteca) ++ " minutos"
