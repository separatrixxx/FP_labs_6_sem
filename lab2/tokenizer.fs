module Tokenizer
open System
open Types


let toString = (function x -> (x |> List.toArray |> String))

let tokenize source =
    let literal_tokens = Map [
        ('.', Token.DOT);
        (';', Token.SEMICOLON);
        (',', Token.COMMA);
        ('{', Token.LEFT_CURLY);
        ('}', Token.RIGHT_CURLY);
        ('(', Token.LEFT_PARENTHESIS);
        (')', Token.RIGHT_PARENTHESIS);
    ]

    let arithmetic_tokens = Map [
        ('+', Token.OPERATOR("+"));
        ('-', Token.OPERATOR("-"));
        ('*', Token.OPERATOR("*"));
        ('/', Token.OPERATOR("/"));
        ('=', Token.OPERATOR("="));
        ('>', Token.OPERATOR(">"));
        ('<', Token.OPERATOR("<"));
        ('|', Token.OPERATOR("|"));
        ('&', Token.OPERATOR("&"));
    ]

    let rec read_string_end acc = function
    | '\\'::'"'::t -> (toString (List.rev acc)), t
    | '"'::t -> (toString (List.rev acc)), t
    | h::t -> read_string_end (h::acc) t
    | [] -> failwith "read_string_end ERROR: EOF before closing \" found"

    let rec read_comment = function
    | '~'::'/'::t -> t
    | _::t -> read_comment t
    | [] -> failwith "read_comment ERROR: EOF before closing comment"

    let rec read_linecomment = function
    | '\n'::t -> t
    | _::t -> read_linecomment t
    | [] -> []

    let rec read_id acc = function
    | h::t when Char.IsWhiteSpace(h) -> (toString (List.rev acc)), t
    | h::t when Char.IsLetter(h) || Char.IsDigit(h) || h = '_' -> read_id (h::acc) t
    | h::t when h = '(' || h = ')' || h = '{' || h = '}' -> (toString (List.rev acc)), (h::t)
    | [] -> (toString (List.rev acc)), []
    | h::_ -> failwith ("read_id ERROR: Unexpected symbol met: " + (string h))

    let rec read_number acc = function
    | h::t when Char.IsWhiteSpace(h) -> (toString (List.rev acc)), t
    | h::t when Char.IsDigit(h) -> read_number (h::acc) t
    | '.'::t -> read_number ('.'::acc) t
    | h::t when h = '(' || h = ')' -> (toString (List.rev acc)), (h::t)
    | [] -> (toString (List.rev acc)), []
    | h::_ -> failwith ("read_number ERROR: Unexpected symbol met while reading digit: " + (string h))

    let rec tokenize_impl acc = function
    | '='::'>'::t -> tokenize_impl (Token.OPERATOR("=>")::acc) t
    | ':'::t -> tokenize_impl (Token.OPERATOR(":")::acc) t
    | h::t when Char.IsWhiteSpace(h) -> tokenize_impl acc t
    | h::t when literal_tokens |> Map.containsKey h -> 
        let token = literal_tokens |> Map.find h
        tokenize_impl (token::acc) t
    | '"'::t | '\\'::'"'::t -> 
        let read_string, remaining_source = read_string_end [] t
        tokenize_impl (Token.STRING(read_string)::acc) remaining_source
    | '/'::'~'::t -> 
        let remaining_source = read_comment t
        tokenize_impl acc remaining_source
    | '/'::'/'::t -> 
        let remaining_source = read_linecomment t
        tokenize_impl acc remaining_source
    | h::'='::t when Char.IsLetter(h) -> 
        let read_id, remaining_source = read_id [] (h::t)
        tokenize_impl (Token.ID(read_id)::Token.OPERATOR("=")::acc) remaining_source
    | h1::h2::t when Char.IsDigit(h1) && (arithmetic_tokens |> Map.tryFind h2).IsSome ->
        let read_number, remaining_source = read_number [] (h1::h2::t)
        tokenize_impl (Token.NUMBER(System.Double.Parse(read_number))::acc) remaining_source
    | h::t when Char.IsLetter(h) ->
        let read_id, remaining_source = read_id [] (h::t)
        tokenize_impl (Token.ID(read_id)::acc) remaining_source
    | h::t when Char.IsDigit(h) ||
                (h = '-' && t |> List.head |> Char.IsDigit) ->
        let read_number, remaining_source = read_number [] (h::t)
        let parsed_number = System.Double.Parse(read_number, System.Globalization.CultureInfo.InvariantCulture)
        tokenize_impl (Token.NUMBER(parsed_number)::acc) remaining_source
    | [] -> List.rev acc, []
    | _ -> failwith "tokenize_impl ERROR: Unsupported symbol met or wrong structure"


    tokenize_impl [] source
    