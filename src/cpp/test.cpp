main = let prog = ((\main->((\eq->((main)(main))(eq)))(
    (\main->(\eq->(\a->(\b->(((IF)(((a) == (b))))(True))(False))))))))(
    (\main->(\eq->((\b->((b)(0))(22)))(
        ((((eq)(main))(eq))(1))(2)))))in putStrLn $ show prog
