{- На этом шаге вы будете работать с монадой IO, а значит, ваша программа будет взаимодействовать с операционной системой. Чтобы тестирующая система смогла оценить вашу программу, пожалуйста, используйте только функции, работающие с файлами и директориями: getDirectoryContents, removeFile. Все эти функции уже будут находиться в области видимости, так что вам не следует их импортировать. По той же причине, главная функция вашей программы будет называться не main, а main' (со штрихом).
В этом задании ваша программа должна попросить пользователя ввести любую строку, а затем удалить все файлы в текущей директории, в именах которых содержится эта строка, выдавая при этом соответствующие сообщения.
Substring: 
Пользователь вводит любую строку:
Substring: hell
Затем программа удаляет из текущей директории файлы с введенной подстрокой в названии. К примеру, если в текущей директории находились файлы thesis.txt, kitten.jpg, hello.world, linux_in_nutshell.pdf, то вывод будет таким:
Substring: hell
Removing file: hello.world
Removing file: linux_in_nutshell.pdf
Если же пользователь ничего не ввёл (просто нажал Enter), следует ничего не удалять и сообщить об этом:
Substring: 
Canceled
Для получения списка файлов в текущей директории используйте функцию getDirectoryContents, передавая ей в качестве аргумента строку, состоящую из одной точки  ("."), что означает «текущая директория». Для удаления файлов используйте функцию removeFile (считайте, что в текущей директории нет поддиректорий — только простые файлы). В выводимых сообщениях удаленные файлы должны быть перечислены в том же порядке, в котором их возвращает функция getDirectoryContents.
Пожалуйста, строго соблюдайте приведенный в примере формат вывода. Особое внимание уделите пробелам и переводам строк! Не забудьте про пробел после Substring:, а также про перевод строки в конце (ожидается, что вы будете использовать putStrLn для вывода сообщений об удалении). -}
import System.Directory

getFileList :: IO [FilePath]
getFileList = do
    a <- getDirectoryContents "."
    return a
    
delete :: IO String
delete = undefined

main' :: IO [FilePath] 
main' = do
    putStr "Substring: "
    substringToDelete <- getLine
    if substringToDelete == "" then do 
        putStrLn "Canceled"
        return []
        else do
            a <- getDirectoryContents "."
            putStr $ concat $ map (\x -> x ++ " ") a
            return a