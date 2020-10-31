main
  {-= let prog
          = ((\ main ->
                ((\ add -> ((main) (main)) (add)))
                  ((\ main -> (\ add -> (\ a -> (\ b -> ((a) + (b)))))))))
              ((\ main -> (\ add -> ((((add) (main)) (add)) (10)) (20))))-}
    let prog = (\add -> add 10 20) (\a -> \b -> a + b) in
    putStrLn $ show prog
