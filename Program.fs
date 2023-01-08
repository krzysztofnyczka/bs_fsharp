// Krzysztof Nyczka

open System
open MathNet.Numerics.Distributions
open System.IO

let normalDistribution: Normal = new Normal(0,1)

type inputFile = {
    S: double; // price
    K: double; // strike price
    T: double; // maturity
    r: double; // risk free intrest rate
    sigma: double; // sigma
}

type outputFile = {
    price: double;
    delta: double;
    // d1: double;
    // d2: double;
}

let d1 (S:double,K:double,T:double,r:double,sigma:double) : double = 
    (Math.Log(S/K) + (r + sigma ** 2/2.) * T) / (sigma * Math.Sqrt(T))

let d2 (S:double,K:double,T:double,r:double,sigma:double) : double = 
    d1(S,K,T,r,sigma) - sigma * Math.Sqrt(T)

let blackScholesCall(S:double,K:double,T:double,r:double,sigma:double) : double =
    (S * normalDistribution.CumulativeDistribution(d1(S,K,T,r,sigma)) - K * Math.Exp(-r * T) * normalDistribution.CumulativeDistribution(d2(S,K,T,r,sigma)))

let blackScholesPut(S:double,K:double,T:double,r:double,sigma:double) : double =
    K * Math.Exp(-r * T) - S * blackScholesCall(S,K,T,r,sigma)

type OptionType = 
    | CALL
    | PUT


let blackScholes (f: inputFile, optionType: OptionType) : outputFile =
    
    match optionType with
    | CALL -> {price = blackScholesCall(f.S,f.K,f.T,f.r,f.sigma);
        delta = normalDistribution.CumulativeDistribution(d1(f.S,f.K,f.T,f.r,f.sigma));
        // d1 = d1(f.S,f.K,f.T,f.r,f.sigma);
        // d2 = d2(f.S,f.K,f.T,f.r,f.sigma) 
        }
    | PUT -> {price = blackScholesPut(f.S,f.K,f.T,f.r,f.sigma);
        delta = -normalDistribution.CumulativeDistribution(-d1(f.S,f.K,f.T,f.r,f.sigma));
        // d1 = d1(f.S,f.K,f.T,f.r,f.sigma);
        // d2 = d2(f.S,f.K,f.T,f.r,f.sigma) 
        }



[<EntryPoint>]
let main(args: string[]) =  
    match args.Length with
    | 0 ->
        let fileReader: string = (new StreamReader("/Users/krzysztof/Documents/data_science/f_sharp/BS/wallet.json")).ReadToEnd()
        let json: inputFile = System.Text.Json.JsonSerializer.Deserialize<inputFile>(fileReader)
        let callResult: outputFile = blackScholes (json, CALL)
        let putResult: outputFile = blackScholes (json, PUT)
        printfn "\n"
        printfn "Initial wallet: %A\n" json
        printfn "Call option result: %A\n" callResult
        printfn "Put option result: %A\n" putResult

    | 1 ->
        let fileReader: string = (new StreamReader("/Users/krzysztof/Documents/data_science/f_sharp/BS/wallet.json")).ReadToEnd()
        let json: inputFile = System.Text.Json.JsonSerializer.Deserialize<inputFile>(fileReader)
        
        if args[0].ToUpper() = "CALL" then
            let callResult: outputFile = blackScholes (json, CALL)
            printfn "\n"
            printfn "Initial wallet: %A\n" json
            printfn "Call option result: %A\n" callResult
        elif args[0].ToUpper() = "PUT" then
            let putResult: outputFile = blackScholes (json, PUT)
            printfn "\n"
            printfn "Initial wallet: %A\n" json
            printfn "Put option result: %A\n" putResult
        else
            printfn "Run program with PUT or CALL argument\n"
    | _ ->
        let fileReader: string = (new StreamReader(args[1])).ReadToEnd()
        let json: inputFile = System.Text.Json.JsonSerializer.Deserialize<inputFile>(fileReader)
        if args[0].ToUpper() = "CALL" then
            let callResult: outputFile = blackScholes (json, CALL)
            printfn "\n"
            printfn "Initial wallet: %A\n" json
            printfn "Call option result: %A\n" callResult
        elif args[0].ToUpper() = "PUT" then
            let putResult: outputFile = blackScholes (json, PUT)
            printfn "\n"
            printfn "Initial wallet: %A\n" json
            printfn "Put option result: %A\n" putResult
        else
            printfn "Run program with PUT or CALL argument\n"
    0

