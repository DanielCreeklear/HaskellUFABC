data Funcionario = Gerente { nome :: String, idade :: Int, salario :: Float }
                 | Desenvolvedor { nome :: String, idade :: Int, linguagem :: String, salario :: Float }
                 | Estagiario { nome :: String, idade :: Int, curso :: String, salario :: Float }

salarioTotal :: Funcionario -> Float
salarioTotal (Gerente _ _ salario) = salario
salarioTotal (Desenvolvedor _ _ _ salario) = salario
salarioTotal (Estagiario _ _ _ salario) = salario

main = do
    let gerente = Gerente "João" 35 5000.0
    let desenvolvedor = Desenvolvedor "Maria" 28 "Python" 4000.0
    let estagiario = Estagiario "Pedro" 22 "Ciência da Computação" 1500.0

    putStrLn $ "Salário total do gerente: " ++ show (salarioTotal gerente)
    putStrLn $ "Salário total do desenvolvedor: " ++ show (salarioTotal desenvolvedor)
    putStrLn $ "Salário total do estagiário: " ++ show (salarioTotal estagiario)
