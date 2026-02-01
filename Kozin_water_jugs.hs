{-
    Задача переливання (Water Jugs Problem)
    Автор: Козін Андрій
    Курс: Логічне програмування, НаУКМА
    
    Порівняння з Prolog:
    - Prolog: автоматичний backtracking, декларативний опис правил
    - Haskell: строга типізація, ефективніший Set для visited
    
    Спільне: обидві мови — декларативні, без мутабельних змінних
-}

module WaterJugs where

import qualified Data.Set as Set

-- Типи даних
type State = [Int]   -- стан: рівні води в посудинах
type Caps = [Int]    -- ємності посудин

-- Дії переливання
data Action = Fill Int           -- наповнити з джерела
            | Empty Int          -- спорожнити в злив
            | Pour Int Int Int   -- перелити (з, в, кількість)
            deriving (Show, Eq)

-- Головна функція: BFS для пошуку оптимального розв'язку
solve :: Caps -> State -> Int -> Maybe [Action]
solve caps initial goal = bfs [(initial, [])] Set.empty
  where
    bfs [] _ = Nothing  -- черга порожня — розв'язку немає
    bfs ((state, path):rest) visited
        | goal `elem` state = Just (reverse path)  -- ціль знайдена
        | state `Set.member` visited = bfs rest visited  -- пропускаємо
        | otherwise = bfs (rest ++ newStates) (Set.insert state visited)
      where
        newStates = [(s, a:path) | (a, s) <- moves caps state, 
                                   not (s `Set.member` visited)]

-- Генерація всіх можливих ходів (аналог move/4 в Prolog)
moves :: Caps -> State -> [(Action, State)]
moves caps state = fillMoves ++ emptyMoves ++ pourMoves
  where
    n = length state
    
    -- fill(I) — наповнити посудину
    fillMoves = [(Fill i, setAt i (caps !! (i-1)) state) 
                | i <- [1..n], state !! (i-1) < caps !! (i-1)]
    
    -- empty(I) — спорожнити посудину
    emptyMoves = [(Empty i, setAt i 0 state)
                 | i <- [1..n], state !! (i-1) > 0]
    
    -- pour(I,J) — перелити між посудинами
    pourMoves = [(Pour i j amt, newState)
                | i <- [1..n], j <- [1..n], i /= j,
                  let lvlI = state !! (i-1),
                  let lvlJ = state !! (j-1),
                  let capJ = caps !! (j-1),
                  lvlI > 0, lvlJ < capJ,
                  let amt = min lvlI (capJ - lvlJ),
                  let newState = setAt j (lvlJ + amt) (setAt i (lvlI - amt) state)]

-- Заміна елемента в списку (аналог set_nth/4 в Prolog)
setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take (i-1) xs ++ [x] ++ drop i xs

-- Виведення розв'язку
printSolution :: Caps -> State -> Int -> IO ()
printSolution caps initial goal = do
    putStrLn $ "Ємності: " ++ show caps ++ ", Ціль: " ++ show goal ++ " л"
    case solve caps initial goal of
        Nothing -> putStrLn "Розв'язок не знайдено!"
        Just actions -> do
            putStrLn $ "Розв'язок за " ++ show (length actions) ++ " кроків:"
            mapM_ (putStrLn . showAction) actions

-- Форматування дії
showAction :: Action -> String
showAction (Fill i) = "  Наповнити посудину " ++ show i
showAction (Empty i) = "  Спорожнити посудину " ++ show i
showAction (Pour i j amt) = "  Перелити " ++ show amt ++ "л з " ++ show i ++ " в " ++ show j

-- Демонстрація
demo :: IO ()
demo = do
    putStrLn "=== [5л, 3л] -> 4л ==="
    printSolution [5, 3] [5, 0] 4
    putStrLn "\n=== [12л, 8л, 5л] -> 6л ==="
    printSolution [12, 8, 5] [12, 0, 0] 6

main :: IO ()
main = demo