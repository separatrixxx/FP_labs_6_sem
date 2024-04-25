let my_abs (x: float) =
    if x > 0. then
        x 
    else
        -x

let rec dichotomy f (a: float) (b: float) =
    let eps = 0.000001
    let xn = (a + b) / 2.
    let fa = f a
    let fb = f b

    if my_abs (f xn) < eps then
        xn
    else if fa < fb then
        if (f xn) < 0. then
            dichotomy f xn b
        else
            dichotomy f a xn
    else 
        if (f xn) > 0. then
            dichotomy f xn b
        else
            dichotomy f a xn

let rec iterations phi x0 =
    let eps = 0.000001

    if my_abs (x0 - (phi x0)) < eps then
        x0
    else
        let next = phi x0
        iterations phi next

let newthon f f' x0 =
    let phi x : float = x - (f x) / (f' x)
    iterations phi x0

let f1 x : float = 0.6 * (3. ** x) - 2.3 * x - 3.
let f1' x : float = (System.Math.Log 3. * (3. ** (x + 1.))) / 5. - 2.3
let phi1 x : float = System.Math.Log ((5. + (23. * x) / 6.), 3.)

let f2 x : float = x ** 2. - System.Math.Log (1. + x) - 3.
let f2' x : float = 2. * x - 1. / (1. + x)
let phi2 x : float = sqrt (System.Math.Log (1. + x) + 3.)

let f3 x : float = 2. * x * System.Math.Sin x - System.Math.Cos x
let f3' x : float = 3. * System.Math.Sin x + 2. * x * System.Math.Cos x
let phi3 x : float = (System.Math.Cos x / System.Math.Sin x) * (1. / 2.)

let main =
    printfn "#     Dichotomy   Iterations  Newton"
    printfn "%d  %10.5f  %10.5f  %10.5f" 8 (dichotomy f1 2. 3.) (iterations phi1 2.5) (newthon f1 f1' 2.5)
    printfn "%d  %10.5f  %10.5f  %10.5f" 9 (dichotomy f2 2. 3.) (iterations phi2 2.5) (newthon f2 f2' 2.5)
    printfn "%d %10.5f  %10.5f  %10.5f" 10 (dichotomy f3 0.4 1.) (iterations phi3 0.6) (newthon f3 f3' 0.7)

main