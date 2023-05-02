##Haskellの特徴を活かして、javaのコードを関数型プログラミングのスタイルに変更することを考えます。

まず以下がHaskellを使った解法の全てのコードです。

```haskell
import Control.Monad (replicateM)

type Board = [[Int]]
type Block = [[Int]]

main :: IO ()
main = do
    [n, m] <- fmap (map read . words) getLine
    board <- readBoard n
    block <- readBlock
    putStrLn $ if canInsertBlock board block then "Yes" else "No"

readBoard :: Int -> IO Board
readBoard n = fmap (map (map (\c -> if c == '#' then 1 else 0))) (replicateM n getLine)

readBlock :: IO Block
readBlock = fmap (map (map (\c -> if c == '#' then 1 else 0))) (replicateM 3 getLine)

canInsertBlock :: Board -> Block -> Bool
canInsertBlock board block = any (canPlaceBlockAt board block) positions
  where
    positions = [(x, y) | x <- [0 .. length board - 1], y <- [0 .. length (head board) - 1]]
    rotations = take 4 $ iterate rotateBlock block

    canPlaceBlockAt :: Board -> Block -> (Int, Int) -> Bool
    canPlaceBlockAt board block (x, y) = any (\b -> isValid board b (x, y)) rotations

    isValid :: Board -> Block -> (Int, Int) -> Bool
    isValid board block (x, y) = all check indices
      where
        indices = [(i, j) | i <- [0 .. 2], j <- [0 .. 2]]
        check (i, j)
          | block !! i !! j == 1 =
            x + i < length board && y + j < length (head board) && board !! (x + i) !! (y + j) == 0
          | otherwise = True

rotateBlock :: Block -> Block
rotateBlock block = [[block !! (2 - j) !! i | j <- [0 .. 2]] | i <- [0 .. 2]]


```
一つ一つ見て行きます。
まず、
```haskell
readBoard :: Int -> IO Board
readBoard n = fmap (map (map (\c -> if c == '#' then 1 else 0))) (replicateM n getLine)

readBlock :: IO Block
readBlock = fmap (map (map (\c -> if c == '#' then 1 else 0))) (replicateM 3 getLine)

```
という記述は

```Java
int[][] board = IntStream.range(0, N)
                         .mapToObj(i -> scanner.nextLine().chars().map(c -> c == '#' ? 1 : 0).toArray())
                         .toArray(int[][]::new);


int[][] block = IntStream.range(0, 3)
                         .mapToObj(i -> scanner.nextLine().chars().map(c -> c == '#' ? 1 : 0).toArray())
                         .toArray(int[][]::new);

```
にそれぞれ対応しています。

ここでいう、`IntStream`　について深掘りして行きます。

#### `IntStream` とは
##### IntStreamの後に続くよく使われるメソッドには、以下のようなものがあります。

1. `range(int startInclusive, int endExclusive)`<br>

指定された範囲の整数ストリームを生成します。startInclusiveからendExclusive-1までの整数が含まれます。

2. `of(int... values)`<br>

指定された整数配列を要素として持つIntStreamを生成します。

3. `map(IntUnaryOperator mapper)`　<br>

`IntStream` の各要素に対して、指定された関数を適用します。

    例　`IntStream.range(1, 6).map(i -> i * i).forEach(System.out::println);`
        1から5までの整数を持つIntStreamを生成し、各要素を2乗して出力するコードです。

ここで、`map()`メソッドに渡されるラムダ式は、`IntUnaryOperator`型の関数を表します。このラムダ式の引数は、`IntStream`の各要素である整数値iです。iを2乗して、新しい整数値を返す処理が記述されています。

具体的には、`i * i`という式がラムダ式内に記述されており、この式は、`IntStream`の各要素を2乗して変換する処理を行います。この処理によって、`IntStream.range(1, 6)`で生成された`IntStream`の要素は、1, 4, 9, 16, 25となります。

`filter(IntPredicate predicate)`　<br>
`IntStream`の要素のうち、指定された条件を満たす要素のみを抽出します。

sum()
IntStreamの全要素の合計を返します。

average()
IntStreamの全要素の平均値を返します。

toArray()
IntStreamをint型の配列に変換します。

boxed()
IntStreamの各要素をIntegerオブジェクトにボクシングします。

parallel()
IntStreamの並列処理を有効にします。

<span style="background-color:yellow">ハイライトしたいテキスト</span>

