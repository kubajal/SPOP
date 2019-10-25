module Main where
    import Index
    main :: IO ()
    main = do
        let i = idx [1,2,3] 2
        let j = i + 1
        putStrLn (show j)