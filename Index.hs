module Index where
    idx list n = 
        if n < 0
            then error "n cant be less than 0"
            else
                error "n is greater than 0"
    --     [] !? n error "no elements left"
    -- (x:xs) !? 0 = x
    -- (x:xs) !? n = xs !? (n-1)