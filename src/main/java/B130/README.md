##Haskellの特徴を活かして、javaのコードを関数型プログラミングのスタイルに変更することを考えます。

まず

```haskell
readBoard :: Int -> IO Board
readBoard n = fmap (map (map (\c -> if c == '#' then 1 else 0))) (replicateM n getLine)

readBlock :: IO Block
readBlock = fmap (map (map (\c -> if c == '#' then 1 else 0))) (replicateM 3 getLine)

という記述は
