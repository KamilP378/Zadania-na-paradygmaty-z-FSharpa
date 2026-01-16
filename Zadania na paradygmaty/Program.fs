open System

// Wszystkie zadania z F# znajdują się tutaj. Żeby zadziałały one poprawnie, trzeba zakomentować wszystkie pozostałe lub kopiować je z tego pliku pojedyńczo razem z open System
// ===================== 
// LAB 4
// =====================


//zad.1

let rec readInt input =
    printfn "%s" input
    let line = Console.ReadLine()
    match Int32.TryParse(line) with
    | true, value -> value
    | _ ->
        printfn "Błąd: podaj poprawną liczbę typu całkowitego"
        readInt input


[<EntryPoint>]
let main _ = 
    let n = readInt "Podaj ilość liczb n = "

    if n<=0 then
        printfn "Bład: liczba n musi być większa od zera!"
    else
        let rec readNItem k acc =
            if k = 0 then List.rev acc
            else
                let x = readInt (sprintf "Podaj liczbę %d:" (n-k+1))
                readNItem (k-1) (x :: acc)

        let lista = readNItem n []

        //obliczenia
        //suma, średnia, minimum, maksimum,
        let suma = List.sum lista
        let min = List.min lista
        let max = List.max lista
        //let avg = List.average lista
        let avg = float suma / float n

        printfn "Lista elementów podanych przez użytkownika: %A" lista
        printfn "Suma %d" suma
        printfn "Min %d" min
        printfn "Max %d" max
        printfn "średnia %f" avg

    0 // koniec main


//zad.2

[<EntryPoint>]
let main argv =
    // Pobranie danych od użytkownika
    printf "Podaj a: "
    let a = Console.ReadLine() |> int
    printf "Podaj b: "
    let b = Console.ReadLine() |> int

    // Ustalenie zakresu
    let minVal, maxVal = if a > b then (b, a) else (a, b)

    // Liczniki do podsumowania
    let mutable parzyste = 0
    let mutable nieparzyste = 0
    let mutable ujemne = 0
    let mutable zera = 0
    let mutable dodatnie = 0
    let mutable podzielne3 = 0

    // Analiza liczb w zakresie
    for i in minVal .. maxVal do
        let parzystosc =
            if i % 2 = 0 then
                parzyste <- parzyste + 1
                "parzysta"
            else
                nieparzyste <- nieparzyste + 1
                "nieparzysta"

        let znak =
            if i < 0 then
                ujemne <- ujemne + 1
                "ujemna"
            elif i = 0 then
                zera <- zera + 1
                "zero"
            else
                dodatnie <- dodatnie + 1
                "dodatnia"

        let podzielnosc3 =
            if i % 3 = 0 then
                podzielne3 <- podzielne3 + 1
                "tak"
            else
                "nie"

        printfn "%d: %s, %s, podzielna przez 3? %s" i parzystosc znak podzielnosc3

    // Podsumowanie
    printfn "PODSUMOWANIE:"
    printfn "Parzyste: %d" parzyste
    printfn "Nieparzyste: %d" nieparzyste
    printfn "Ujemne: %d" ujemne
    printfn "Zera: %d" zera
    printfn "Dodatnie: %d" dodatnie
    printfn "Podzielne przez 3: %d" podzielne3

    0 // koniec 

//zad.3

[<EntryPoint>]
let main argv =
    // Wczytanie listy liczb od użytkownika (np. wpisanych po spacji)
    printfn "Podaj liczby całkowite oddzielone spacją:"
    let input = Console.ReadLine()
    let numbers =
        input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> Array.toList

    // Ile jest liczb dodatnich
    let dodatnieCount =
        numbers
        |> List.filter (fun x -> x > 0)
        |> List.length

    // Tworzenie listy kwadratów 
    let kwadraty =
        numbers
        |> List.map (fun x -> x * x)

    //  Suma wszystkich liczb
    let suma =
        numbers
        |> List.fold (fun acc x -> acc + x) 0

    // Wyniki
    printfn "Liczby: %A" numbers
    printfn "Liczb dodatnich: %d" dodatnieCount
    printfn "Kwadraty: %A" kwadraty
    printfn "Suma: %d" suma

    0 // koniec


//zad.4
type Student = { Name: string; Age: int; Grade: float }

[<EntryPoint>]
let main argv =
    let students = [
        { Name = "Anna"; Age = 20; Grade = 4.5 }
        { Name = "Bartek"; Age = 22; Grade = 3.8 }
        { Name = "Celina"; Age = 19; Grade = 4.2 }
        { Name = "Dawid"; Age = 21; Grade = 2.9 }
    ]

    // filtrowanie studentów z oceną >= 4.0
    let dobrzyStudenci =
        students
        |> List.filter (fun s -> s.Grade >= 4.0)

    // Zwiększenie każdemu uczniowi wieku o 1 rok
    let starsiStudenci =
        students
        |> List.map (fun s -> { s with Age = s.Age + 1 })

    // wyniki
    printfn "Studenci z oceną >= 4.0:"
    dobrzyStudenci |> List.iter (fun s -> printfn "%s, wiek: %d, ocena: %.1f" s.Name s.Age s.Grade)

    printfn "\nStudenci po zwiększeniu wieku:"
    starsiStudenci |> List.iter (fun s -> printfn "%s, wiek: %d, ocena: %.1f" s.Name s.Age s.Grade)

    0 // koniec



// ===================== 
// LAB 5
// =====================

//zad.1

//rek
let rec fib n = 
    if n < 0 then failwith "n musi być wartością dodatnią"
    elif n = 0 then 0
    elif n = 1 then 1
    else fib (n-1) + fib (n-2)

//rek ogonowa
let fibTail n =
    let rec loop a b i =
        if i = n then a
        else loop b (a + b) (i + 1)
    loop 0 1 0

//wywolanie

printf "fib 5 = %d" (fib 5)
printf "\nfib 5 = %d" (fibTail 5)

//zad.2

// struktura drzewa

type Tree<'T> = 
    | Empty 
    | Node of 'T * Tree<'T> * Tree<'T>

let rec searchTree value tree = 
    match tree with
    | Empty -> false
    | Node (v, left, right) ->
        if v = value then true
        else searchTree value left || searchTree value right

// iteracyjnie 

let searchTreeIter value tree = 
    let rec loop stack =
        match stack with
        | [] -> false
        | Empty :: rest -> loop rest
        | Node (v, left, right) :: rest ->
            if v = value then true
            else loop (left :: right :: rest)
    loop [tree]
        
let tree = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
printf "\nWartosc = %b" (searchTreeIter 2 tree) // true

//zad.3

let rec permutations list =
    match list with
    | [] -> [ [] ]
    | xs ->
        [ for i in xs do
            let rest = xs |> List.filter ((<>) i)
            for p in permutations rest do
                yield i :: p ]

// Lista do permutacji
let xs = [1; 2; 3]

// Wywołanie funkcji
let perms = permutations xs

printfn "Permutacje:"
perms |> List.iter (printfn "%A")

//zad.4

// rekurencyjnie

let rec hanoi n fromPeg toPeg auxPeg =
    if n = 1 then
        [ sprintf "Przenieś dysk z %s do %s" fromPeg toPeg ]
    else
        let a = hanoi (n-1) fromPeg auxPeg toPeg
        let b = [ sprintf "Przenieś dysk z %s do %s" fromPeg toPeg ]
        let c = hanoi (n-1) auxPeg toPeg fromPeg
        a @ b @ c


let movesRec = hanoi 3 "A" "C" "B"

printfn "Hanoi rekurencyjnie:"
movesRec |> List.iter (printfn "%s")


//iteracyjnie


let hanoiIterative n =
    let source = "A"
    let auxiliary = "B"
    let target = "C"

    let (aux, tgt) =
        if n % 2 = 0 then (target, auxiliary)
        else (auxiliary, target)

    let totalMoves = (1 <<< n) - 1

    [1 .. totalMoves]
    |> List.map (fun move ->
        match move % 3 with
        | 1 -> sprintf "Przenieś dysk z %s do %s" source tgt
        | 2 -> sprintf "Przenieś dysk z %s do %s" source aux
        | _ -> sprintf "Przenieś dysk z %s do %s" aux tgt
    )

let movesIter = hanoiIterative 3

printfn "\nHanoi iteracyjnie:"
movesIter |> List.iter (printfn "%s")


//zad.5

//rekurencyjnie

let rec quicksortRec list =
    match list with
    | [] -> []
    | pivot :: rest ->
        let smaller = rest |> List.filter (fun x -> x <= pivot)
        let greater = rest |> List.filter (fun x -> x > pivot)
        (quicksortRec smaller) @ [pivot] @ (quicksortRec greater)


let data = [5; 3; 8; 4; 2; 7; 1; 6]
let sortedRec = quicksortRec data

printfn "QuickSort rekurencyjny:"
printfn "%A" sortedRec

//iteracyjnie

let quicksortIter (arr: int[]) =
    let swap i j =
        let tmp = arr[i]
        arr[i] <- arr[j]
        arr[j] <- tmp

    let partition left right =
        let pivot = arr[right]
        let mutable i = left - 1

        for j in left .. right - 1 do
            if arr[j] <= pivot then
                i <- i + 1
                swap i j

        swap (i + 1) right
        i + 1

    let stack = System.Collections.Generic.Stack<int * int>()
    stack.Push(0, arr.Length - 1)

    while stack.Count > 0 do
        let (left, right) = stack.Pop()
        if left < right then
            let p = partition left right
            stack.Push(left, p - 1)
            stack.Push(p + 1, right)


let data2 = [|5; 3; 8; 4; 2; 7; 1; 6|]
quicksortIter data2

printfn "QuickSort iteracyjny:"
printfn "%A" data2

// zadania z pliku LAB8, który znajdował się razem z plikiem LAB5
// Zgodnie z treścią zadania trzeba było wykonać program z menu, które wybiera daną funkcję do wykonania.

// ==========================================
//  DEFINICJA LISTY ŁĄCZONEJ
// ==========================================

type LinkedList<'T> =
    | Empty
    | Node of 'T * LinkedList<'T>


// ==========================================
//  MODUŁ Z FUNKCJAMI
// ==========================================

module LinkedList =

    // 1. Tworzenie listy łączonej z List<'T>
    let rec fromList list =
        match list with
        | [] -> Empty
        | x :: xs -> Node(x, fromList xs)

    // Wyświetlanie elementów
    let rec print list =
        match list with
        | Empty -> ()
        | Node(x, xs) ->
            printf "%A " x
            print xs

    // 2. Sumowanie elementów
    let rec sum list =
        match list with
        | Empty -> 0
        | Node(x, xs) -> x + sum xs

    // 3. Min i Max
    let minMax list =
        let rec aux mn mx lst =
            match lst with
            | Empty -> mn, mx
            | Node(x, xs) -> aux (min mn x) (max mx x) xs
        match list with
        | Empty -> failwith "Lista jest pusta"
        | Node(x, xs) -> aux x x xs

    // 4. Odwracanie listy
    let reverse list =
        let rec aux acc lst =
            match lst with
            | Empty -> acc
            | Node(x, xs) -> aux (Node(x, acc)) xs
        aux Empty list

    // 5. Sprawdzanie obecności elementu
    let rec contains value list =
        match list with
        | Empty -> false
        | Node(x, xs) -> x = value || contains value xs

    // 6. Indeks elementu
    type IndexResult =
        | Found of int
        | NotFound

    let indexOf value list =
        let rec aux i lst =
            match lst with
            | Empty -> NotFound
            | Node(x, xs) ->
                if x = value then Found i
                else aux (i + 1) xs
        aux 0 list

    // 7. Liczenie wystąpień
    let rec count value list =
        match list with
        | Empty -> 0
        | Node(x, xs) ->
            (if x = value then 1 else 0) + count value xs

    // 8. Łączenie dwóch list
    let rec concat l1 l2 =
        match l1 with
        | Empty -> l2
        | Node(x, xs) -> Node(x, concat xs l2)

    // 9. Porównanie dwóch list
    let rec compareLists l1 l2 =
        match l1, l2 with
        | Empty, Empty -> Empty
        | Node(x, xs), Node(y, ys) ->
            Node(x > y, compareLists xs ys)
        | _ -> failwith "Listy mają różne długości"

    // 10. Filtrowanie
    let rec filter pred list =
        match list with
        | Empty -> Empty
        | Node(x, xs) ->
            if pred x then Node(x, filter pred xs)
            else filter pred xs

    // 11. Usuwanie duplikatów
    let removeDuplicates list =
        let rec aux seen lst =
            match lst with
            | Empty -> Empty
            | Node(x, xs) ->
                if Set.contains x seen then aux seen xs
                else Node(x, aux (Set.add x seen) xs)
        aux Set.empty list

    // 12. Podział listy
    let partition pred list =
        let rec aux yes no lst =
            match lst with
            | Empty -> reverse yes, reverse no
            | Node(x, xs) ->
                if pred x then aux (Node(x, yes)) no xs
                else aux yes (Node(x, no)) xs
        aux Empty Empty list



// ==========================================
//  FUNKCJE POMOCNICZE DO MENU
// ==========================================

let readIntList () =
    printf "Podaj liczby oddzielone spacją: "
    Console.ReadLine().Split(' ')
    |> Array.toList
    |> List.map int
    |> LinkedList.fromList


// ==========================================
//  MENU
// ==========================================

let rec menu currentList =
    printfn ""
    printfn "================ MENU ================"
    printfn "1. Wczytaj listę"
    printfn "2. Wyświetl listę"
    printfn "3. Suma elementów"
    printfn "4. Min i Max"
    printfn "5. Odwróć listę"
    printfn "6. Sprawdź, czy element istnieje"
    printfn "7. Znajdź indeks elementu"
    printfn "8. Policz wystąpienia elementu"
    printfn "9. Połącz z drugą listą"
    printfn "10. Porównaj dwie listy"
    printfn "11. Filtrowanie"
    printfn "12. Usuń duplikaty"
    printfn "13. Podziel listę"
    printfn "0. Wyjście"
    printf "Wybór: "

    match Console.ReadLine() with
    | "0" -> printfn "Koniec programu."

    | "1" ->
        let l = readIntList()
        menu l

    | "2" ->
        printf "Lista: "
        LinkedList.print currentList
        printfn ""
        menu currentList

    | "3" ->
        printfn "Suma: %d" (LinkedList.sum currentList)
        menu currentList

    | "4" ->
        let mn, mx = LinkedList.minMax currentList
        printfn "Min: %d, Max: %d" mn mx
        menu currentList

    | "5" ->
        let rev = LinkedList.reverse currentList
        printf "Odwrócona: "
        LinkedList.print rev
        printfn ""
        menu rev

    | "6" ->
        printf "Podaj element: "
        let v = int (Console.ReadLine())
        printfn "Zawiera? %b" (LinkedList.contains v currentList)
        menu currentList

    | "7" ->
        printf "Podaj element: "
        let v = int (Console.ReadLine())
        printfn "Indeks: %A" (LinkedList.indexOf v currentList)
        menu currentList

    | "8" ->
        printf "Podaj element: "
        let v = int (Console.ReadLine())
        printfn "Wystąpienia: %d" (LinkedList.count v currentList)
        menu currentList

    | "9" ->
        printfn "Podaj drugą listę:"
        let l2 = readIntList()
        let merged = LinkedList.concat currentList l2
        printf "Połączona: "
        LinkedList.print merged
        printfn ""
        menu merged

    | "10" ->
        printfn "Podaj drugą listę:"
        let l2 = readIntList()
        let cmp = LinkedList.compareLists currentList l2
        printf "Porównanie: "
        LinkedList.print cmp
        printfn ""
        menu currentList

    | "11" ->
        printf "Podaj wartość progową: "
        let v = int (Console.ReadLine())
        let filtered = LinkedList.filter (fun x -> x > v) currentList
        printf "Wynik: "
        LinkedList.print filtered
        printfn ""
        menu currentList

    | "12" ->
        let noDup = LinkedList.removeDuplicates currentList
        printf "Bez duplikatów: "
        LinkedList.print noDup
        printfn ""
        menu noDup

    | "13" ->
        printf "Podaj wartość progową: "
        let v = int (Console.ReadLine())
        let yes, no = LinkedList.partition (fun x -> x > v) currentList
        printf "Spełniające: "
        LinkedList.print yes
        printf "\nPozostałe: "
        LinkedList.print no
        printfn ""
        menu currentList

    | _ ->
        printfn "Niepoprawny wybór."
        menu currentList


// ==========================================
//  START PROGRAMU
// ==========================================

menu Empty





// ===================== 
// LAB 6
// =====================

//zad.1
printfn "Wprowadź tekst: "
let input = Console.ReadLine()
let wordCount = input.Split([|' '; '\n'; '\t'|], StringSplitOptions.RemoveEmptyEntries).Length

printfn "Liczba słów: %d" wordCount

let charCount = input.Replace(" ","").Length
printfn "Liczba znaków (bez spacji): %d" charCount

//zad.2

let isPalindrom(input: string) =
    let cleanedInput = input.Replace(" ","").ToLower()
    cleanedInput = String(Array.rev (cleanedInput.ToCharArray()))

printfn "Wprowadź tekst: "
let input1 = Console.ReadLine()

if isPalindrom input1 then
    printfn "Podany tekst jest palindromem"
else
    printfn "Podany tekst nie jest palindromem"

//zad.3

let main =
    printf "Podaj słowa: "
    let input = System.Console.ReadLine()

    let unique =
        input.Split ' '
        |> Array.distinct

    printfn "%A" unique


//zad.4

printfn "Podaj dane w formacie: imię, nazwisko, wiek"
printfn "Pusta linia kończy program.\n"

let mutable line = Console.ReadLine()

while not (String.IsNullOrWhiteSpace(line)) do
    let parts = line.Split(',') |> Array.map _.Trim()
    if parts.Length = 3 then
        let imie, nazwisko, wiek = parts[0], parts[1], parts[2]
        printfn "%s, %s (%s lat)" nazwisko imie wiek
    else
        printfn "Zły format!"

    line <- Console.ReadLine()

//zad.5

printf "Podaj tekst: "
let tekst = Console.ReadLine()

let slowa = tekst.Split ' '
let naj = slowa |> Array.maxBy (fun s -> s.Length)

printfn "Najdłuższe słowo: %s" naj
printfn "Długość: %d" naj.Length

//zad.6


[<EntryPoint>]
let main argv =
    // Pobranie tekstu od użytkownika
    printf "Podaj tekst: "
    let tekst = Console.ReadLine()

    // Pobranie słowa do wyszukania
    printf "Podaj słowo do wyszukania: "
    let szukane = Console.ReadLine()

    // Pobranie nowe słowo
    printf "Podaj słowo na jakie chcesz zamienić: "
    let zamiennik = Console.ReadLine()

    // Zamiana słów w tekście
    let wynik = tekst.Replace(szukane, zamiennik)

    // Wyświetlenie zmodyfikowanego tekstu
    printfn "\nZmodyfikowany tekst:"
    printfn "%s" wynik

    0

// zadania z pliku LAB7, który znajdował sie w folderze LAB6

//zad.1 - System do zarządzania biblioteką

// =======================
// Klasa Book
// =======================
type Book(title: string, author: string, pages: int) =
    member _.Title = title
    member _.Author = author
    member _.Pages = pages

    member _.GetInfo() =
        sprintf "Tytuł: %s, Autor: %s, Liczba stron: %d" title author pages


// =======================
// Klasa User
// =======================
type User(name: string) =
    let mutable borrowedBooks : Book list = []

    member _.Name = name

    member _.BorrowBook(book: Book) =
        borrowedBooks <- book :: borrowedBooks
        printfn "%s wypożyczył książkę: %s" name book.Title

    member _.ReturnBook(book: Book) =
        borrowedBooks <- borrowedBooks |> List.filter (fun b -> b <> book)
        printfn "%s zwrócił książkę: %s" name book.Title

    member _.ListBorrowedBooks() =
        printfn "Książki wypożyczone przez %s:" name
        borrowedBooks
        |> List.iter (fun b -> printfn " - %s" (b.GetInfo()))


// =======================
// Klasa Library
// =======================
type Library() =
    let mutable books : Book list = []

    member _.AddBook(book: Book) =
        books <- book :: books
        printfn "Dodano książkę: %s" book.Title

    member _.RemoveBook(book: Book) =
        books <- books |> List.filter (fun b -> b <> book)
        printfn "Usunięto książkę: %s" book.Title

    member _.ListBooks() =
        printfn "Książki dostępne w bibliotece:"
        books
        |> List.iter (fun b -> printfn " - %s" (b.GetInfo()))

    member _.HasBook(book: Book) =
        books |> List.contains book


// =======================
// Program główny
// =======================
[<EntryPoint>]
let main _ =
    let library = Library()
    let user = User("Jan Kowalski")

    let book1 = Book("Władca Pierścieni", "J.R.R. Tolkien", 1200)
    let book2 = Book("Hobbit", "J.R.R. Tolkien", 310)
    let book3 = Book("1984", "George Orwell", 328)

    library.AddBook(book1)
    library.AddBook(book2)
    library.AddBook(book3)

    printfn ""
    library.ListBooks()

    printfn ""
    if library.HasBook(book1) then
        library.RemoveBook(book1)
        user.BorrowBook(book1)

    printfn ""
    library.ListBooks()

    printfn ""
    user.ListBorrowedBooks()

    printfn ""
    user.ReturnBook(book1)
    library.AddBook(book1)

    printfn ""
    library.ListBooks()

    0

//zad.2 - System BankAccount

// =======================
// Klasa BankAccount
// =======================
type BankAccount(accountNumber: string, initialBalance: decimal) =
    let mutable balance = initialBalance

    member _.AccountNumber = accountNumber
    member _.Balance = balance

    member _.Deposit(amount: decimal) =
        if amount <= 0m then
            failwith "Kwota wpłaty musi być dodatnia"
        balance <- balance + amount
        printfn "Wpłata %.2f na konto %s. Saldo: %.2f" amount accountNumber balance

    member _.Withdraw(amount: decimal) =
        if amount <= 0m then
            failwith "Kwota wypłaty musi być dodatnia"
        if amount > balance then
            failwith "Brak wystarczających środków"
        balance <- balance - amount
        printfn "Wypłata %.2f z konta %s. Saldo: %.2f" amount accountNumber balance


// =======================
// Klasa Bank (CRUD)
// =======================
type Bank() =
    let mutable accounts : BankAccount list = []

    // CREATE
    member this.CreateAccount(accountNumber: string, initialBalance: decimal) =
        if accounts |> List.exists (fun a -> a.AccountNumber = accountNumber) then
            failwith "Konto o takim numerze już istnieje"
        let account = BankAccount(accountNumber, initialBalance)
        accounts <- account :: accounts
        printfn "Utworzono konto: %s" accountNumber
        account

    // READ
    member this.GetAccount(accountNumber: string) =
        accounts |> List.tryFind (fun a -> a.AccountNumber = accountNumber)

    // UPDATE
    member this.UpdateAccount(accountNumber: string, newBalance: decimal) =
        match this.GetAccount(accountNumber) with
        | Some account ->
            let diff = newBalance - account.Balance
            if diff > 0m then
                account.Deposit(diff)
            elif diff < 0m then
                account.Withdraw(-diff)
            printfn "Zaktualizowano saldo konta %s" accountNumber
        | None ->
            failwith "Nie znaleziono konta"

    // DELETE
    member this.DeleteAccount(accountNumber: string) =
        let before = accounts.Length
        accounts <- accounts |> List.filter (fun a -> a.AccountNumber <> accountNumber)
        if accounts.Length < before then
            printfn "Usunięto konto: %s" accountNumber
        else
            failwith "Nie znaleziono konta"

    member this.ListAccounts() =
        printfn "Lista kont w banku:"
        accounts
        |> List.iter (fun a ->
            printfn " - Konto: %s | Saldo: %.2f" a.AccountNumber a.Balance
        )


// =======================
// Program główny
// =======================
[<EntryPoint>]
let main _ =
    let bank = Bank()

    // CREATE - tworzenie kont
    let acc1 = bank.CreateAccount("123-ABC", 1000m)
    let acc2 = bank.CreateAccount("456-DEF", 500m)

    printfn ""
    bank.ListAccounts()

    // READ - odczyt konta
    printfn ""
    match bank.GetAccount("123-ABC") with
    | Some acc -> printfn "Odczyt konta %s | Saldo: %.2f" acc.AccountNumber acc.Balance
    | None -> printfn "Konto nie istnieje"

    // UPDATE - wpłata i wypłata
    printfn ""
    acc1.Deposit(200m)
    acc2.Withdraw(100m)

    // UPDATE - bezpośrednia aktualizacja salda
    printfn ""
    bank.UpdateAccount("123-ABC", 1500m)

    printfn ""
    bank.ListAccounts()

    // DELETE - usuwanie konta
    printfn ""
    bank.DeleteAccount("456-DEF")

    printfn ""
    bank.ListAccounts()

    0

