#if INTERACTIVE
#load "types.fs"
#load "tokenizer.fs"
#load "interpreter.fs"
#load "parser.fs"
#endif

open System
open System.IO
open Types
open Tokenizer
open Parser
open Interpreter
open System.Text.RegularExpressions


let env  =  Map<String, Expr> []

let rec repl env =
    printf "> "
    let source = Console.ReadLine()
    try
        printfn "Источник: %s" source
        let tokens, _ = tokenize (source |> Seq.toList)
        let expr = parse tokens
        printfn "Распаршено: %A" expr
        let evaluated, new_env = evaluate  env expr
        printfn "Обработано: %A\n" evaluated
        repl new_env
    with ex ->
        printfn "Исключение: %s" ex.Message
        repl env

let rec execute env (filename: string) =
    if (not(Regex.IsMatch(filename, "^.*[.]frn$"))) then
        printf("Для запуска кода на языке Frieren расширение файла должно быть .frn")
    else
        let lines = File.ReadAllLines(filename)
        let source = String.concat "" lines
        try
            let tokens, _ = tokenize (source |> Seq.toList)
            let expr = parse tokens
            let evaluated, new_env = evaluate  env expr
            printfn "\nУспешный успех"
        with ex ->
            printfn "Ферн недовольна: %s" ex.Message


let main =
    let args = Environment.GetCommandLineArgs()
    if args.Length <> 2 then
        printfn "Для запуска скрипта используйте: dotnet run [ИМЯ_ФАЙЛА].frn"
        1
    else
        execute env args[1]
        0
