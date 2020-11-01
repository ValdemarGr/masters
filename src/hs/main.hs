main =
    let prog = (\main -> ((\add -> main (main) add) ((\a -> (\b -> a + b))))) ((\_ -> (\add -> add 1 2))) in
    putStrLn $ show prog
