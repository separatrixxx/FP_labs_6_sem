let my_func x : float = (1. / (2. * x - 5.))

let a = 0.
let b = 2.
let n = 10

let my_abs (x: float) =
    if x > 0. then
        x 
    else
        -x

let rec dumb_taylor' (x: float) n (acc: float) =
    let eps = 0.000001

    if my_abs (my_func x - acc) < eps then
        acc, n - 1
    else
        let newAcc = acc - (((2. ** (float n - 1.)) * (x ** (float n - 1.))) / (5. ** float n))
        dumb_taylor' x (n + 1) newAcc

let dumb_taylor (x: float) = dumb_taylor' x 1 0.


let rec smart_taylor' (x: float) n (prev: float) (acc: float) =
    let eps = 0.000001

    if my_abs (my_func x - acc) < eps then
        acc, n
    else
        let newAcc = acc + ((prev * 2. * x) / 5.)
        let newPrev = prev * 2. * x / 5.
        smart_taylor' x (n + 1) newPrev newAcc

let smart_taylor (x: float) = smart_taylor' x 1 (-1. / 5.) (-1. / 5.)

let main =
    printfn " x     Builtin    Smart Taylor  # terms  Dumb Taylor  # terms"
    for i = 0 to n do
        let x = a + (float i) / (float n) * (b - a)
        let x1, n1 = smart_taylor x
        let x2, n2 = dumb_taylor x
        printfn "%5.2f %10.6f %10.6f     %d       %10.6f    %d" x (my_func x) x1 n1 x2 n2

main