module Parser
open Types


let parse tokens = 
    let rec parse_ids acc = function
        | Token.ID(id)::t -> parse_ids (id::acc) t
        | Token.RIGHT_CURLY::t -> List.rev acc, t
        | _ -> failwith "parse_function_parameters ERROR: not found expected id"

    let keywords = ["lt"; "mut"; "spell"; "scr"; "cast"; "if"; "else"; "for"; "while"; "readGrim"; "writeGrim"; "flowerField"]

    let rec token_parser acc = function
        | [] -> List.rev acc, []

        | Token.ID(expr)::t when (List.tryFind (fun x -> x = expr) keywords).IsSome ->
            token_parser (Expr.SIMPLE(expr)::acc) t

        | Token.NUMBER(n)::t -> token_parser (Expr.SIMPLELIST([Expr.NUMBER(n)])::acc) t
        | Token.ID("true")::t -> token_parser (Expr.SIMPLELIST([Expr.BOOL(true)])::acc) t
        | Token.ID("false")::t -> token_parser (Expr.SIMPLELIST([Expr.BOOL(false)])::acc) t
        | Token.ID("heal")::t -> token_parser (Expr.HEAL::acc) t
        | Token.ID("kill")::t -> token_parser (Expr.KILL::acc) t
        | Token.ID(id)::t -> token_parser (Expr.SIMPLELIST([Expr.ID(id)])::acc) t
        | Token.STRING(s)::t -> token_parser (Expr.SIMPLELIST([Expr.STRING(s)])::acc) t
        | Token.OPERATOR("=>")::t -> token_parser (Expr.SIMPLE("=>")::acc) t
        | Token.OPERATOR("=")::t -> token_parser (Expr.SIMPLE("=")::acc) t
        | Token.OPERATOR(":")::t -> token_parser (Expr.SIMPLE("=")::acc) t
        | Token.OPERATOR("..")::t -> token_parser (Expr.SIMPLE("..")::acc) t

        | Token.LEFT_BRACKET::t -> token_parser (Expr.SIMPLE("[")::acc) t
        | Token.RIGHT_BRACKET::t -> token_parser (Expr.SIMPLE("]")::acc) t
        | Token.RIGHT_CURLY::t -> List.rev acc, t
        | Token.LEFT_CURLY::t ->
            let read_args, remaining_part = token_parser [] t
            if List.forall (fun x -> match x with | Expr.SIMPLELIST([Expr.ID(_)]) -> true | _ -> false) read_args 
            then token_parser (Expr.SIMPLEARGLIST(read_args)::acc) remaining_part
            else failwith ("token_parser ERROR: non ids inside function args: " + (sprintf "%A" read_args))

        | Token.RIGHT_PARENTHESIS::t -> List.rev acc, t
        | Token.LEFT_PARENTHESIS::t ->
            let read_exprs, remaining_part = token_parser [] t

            match read_exprs with
            | Expr.SIMPLEOP(op)::t -> token_parser (Expr.SIMPLELIST([Expr.OPERATOR(op, t)])::acc) remaining_part
            | Expr.SIMPLE("lt")::Expr.SIMPLELIST([Expr.ID(id)])::Expr.SIMPLE("=")::Expr.SIMPLELIST(list)::[] ->
                token_parser (Expr.SIMPLELIST([Expr.VARIABLE(id, Expr.SIMPLELIST(list))])::acc) remaining_part
            | Expr.SIMPLE("mut")::Expr.SIMPLELIST([Expr.ID(id)])::Expr.SIMPLE("=")::Expr.SIMPLELIST(list)::[] ->
                token_parser (Expr.SIMPLELIST([Expr.SET(id, Expr.SIMPLELIST(list))])::acc) remaining_part

            | Expr.SIMPLE("spell")::Expr.SIMPLELIST([Expr.ID(id)])::(Expr.SIMPLEARGLIST(args_list) as args)
                ::Expr.SIMPLE("=>")::(Expr.SIMPLELIST(_) as body)::[] -> 
                token_parser (SIMPLELIST([Expr.FUNC_DEF(id, args, body, Map<string, Expr>[], List.length args_list)])::acc) remaining_part

            | Expr.SIMPLE("readGrim")::Expr.SIMPLELIST([Expr.STRING(path)])::t ->
                token_parser (SIMPLELIST([Expr.READ_FILE(path)])::acc) remaining_part
            
            | Expr.SIMPLE("writeGrim")::Expr.SIMPLELIST([Expr.STRING(path)])::Expr.SIMPLELIST([Expr.STRING(data)])::t ->
                token_parser (SIMPLELIST([Expr.WRITE_FILE(path, data)])::acc) remaining_part
            
            | Expr.SIMPLELIST([Expr.ID(id)])::t ->
                if List.forall (fun x -> match x with | Expr.SIMPLELIST(_) -> true | _ -> false) t
                then token_parser (SIMPLELIST([Expr.CALL(id, Expr.SIMPLELIST(t), List.length t)])::acc) remaining_part
                else failwith ("token_parser ERROR: wrong function syntax! Used as parameters for function call: " + (sprintf "%A" t)) 

            | Expr.SIMPLELIST(list)::t -> 
                let rec gather_simple_lists acc = function
                | (Expr.SIMPLELIST(_) as list)::t' -> gather_simple_lists (list::acc) t'
                | Expr.HEAL::t' -> gather_simple_lists (Expr.KILL::acc) t'
                | Expr.KILL::t' -> gather_simple_lists (Expr.KILL::acc) t'
                | [] -> List.rev acc, []
                | waste -> 
                    printfn "unmatched expr: %A" waste
                    failwith "gather_simple_lists ERROR: misformat expression"

                let lists, _ = gather_simple_lists [] (SIMPLELIST(list)::t)
                token_parser (Expr.SIMPLELIST(lists)::acc) remaining_part
            | Expr.SIMPLE("if")::(Expr.SIMPLELIST(_) as cond)
                ::Expr.SIMPLE("=>")::(Expr.SIMPLELIST(_) as expr1)
                ::Expr.SIMPLE("else")::Expr.SIMPLE("=>")::(Expr.SIMPLELIST(_) as expr2)::[] -> 
                    token_parser (Expr.SIMPLELIST([Expr.COND(cond, expr1, expr2)])::acc) remaining_part

            | Expr.SIMPLE("if")::(Expr.SIMPLELIST(_) as cond)
                ::Expr.SIMPLE("=>")::(Expr.SIMPLELIST(_) as expr1)::[] ->
                    token_parser (Expr.SIMPLELIST([Expr.COND(cond, expr1, SIMPLE(""))])::acc) remaining_part

            | Expr.SIMPLE("for")::Expr.SIMPLELIST([Expr.ID(var)])
                ::Expr.SIMPLE("[")::(Expr.SIMPLELIST(_) as start)
                ::Expr.SIMPLE("..")::(Expr.SIMPLELIST(_) as endValue)
                ::Expr.SIMPLE("]")::Expr.SIMPLE("=>")::(Expr.SIMPLELIST(_) as body)::[] ->
                    token_parser (Expr.SIMPLELIST([Expr.FOR_LOOP(var, start, endValue, body)])::acc) remaining_part

            | Expr.SIMPLE("while")::(Expr.SIMPLELIST(_) as cond)::
                Expr.SIMPLE("=>")::(Expr.SIMPLELIST(_) as body)::[] ->
                    token_parser (Expr.SIMPLELIST([Expr.WHILE(cond, body)])::acc) remaining_part

            | Expr.SIMPLE("cast")::(Expr.SIMPLELIST(_) as body)::[] ->
                token_parser (Expr.SIMPLELIST([Expr.PRINT(body)])::acc) remaining_part

            | Expr.SIMPLE("flowerField")::[] ->
                token_parser (Expr.SIMPLELIST([Expr.SIMPLE("flowerField")])::acc) remaining_part


            | (Expr.NUMBER(_) as num_expr)::[] -> token_parser (SIMPLELIST([num_expr])::acc) remaining_part
            | (Expr.STRING(_) as str_expr)::[] -> token_parser (SIMPLELIST([str_expr])::acc) remaining_part
            | waste -> failwith ("token_parser ERROR: wrong parenthesis structure: " + (sprintf "%A" waste))

        | Token.OPERATOR(op)::t -> token_parser (Expr.SIMPLEOP(op)::acc) t
        | waste -> 
            printfn "unexpected token: %A" waste
            failwith "token_parser ERROR: unexpected token"

    let parsed_expr, remaining_part = token_parser [] tokens
    if remaining_part <> [] then 
        printfn "unparsed part: %A" remaining_part
        failwith "token_parser ERROR: misformat expression"
    Expr.SIMPLELIST(parsed_expr)
