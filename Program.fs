// Copyright (c) 2019 Qiao Wang
//
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

open System
open System.IO
open Microsoft.FSharp.Control
open FSharp.Core
open FSharp.Data
open FSharp.Json

type Code = string

[<JsonUnion(Mode=UnionMode.CaseKeyAsFieldValue, CaseKeyField="type", CaseValueField="code")>]
type StockCode =
    | SH of Code
    | SZ of Code
    | HK of Code
    | NSDQ of Code

type Stock = {
    [<JsonField("name")>]
    Name: string
    [<JsonField("code")>]
    Code: StockCode
}

type StockData = {
    Price: float
    Open: float
    High: float
    Low: float
    Updown: float
    UpdownRate: float
    ExchangeRatio: float
}

type StockResult = {
    Stock: Stock
    Data: StockData option
}

module HexunStock =
    let internal baseHost = "hermes.hexun.com"
    let internal column = "Price,Open,High,Low,UpDown,UpDownRate,PE2,ExchangeRatio"
    let internal callback = "c"

    let getCodeString = function
        | SH code -> "sse" + code
        | SZ code -> "szse" + code
        | HK code -> "HKEX" + code
        | NSDQ code -> "NASDAQ" + code

    let getRegion = function
        | SZ _ -> "a"
        | SH _ -> "a"
        | HK _ -> "hk"
        | NSDQ _ -> "usa"

    let getHostPrefix = function
        | SZ _ -> "webstock.quote"
        | SH _ -> "webstock.quote"
        | HK _ -> "webhkstock.quote"
        | NSDQ _ -> "webusstock"

    let parseData code (text: string) =
        try
            let valuePart = text.Split(":").[1]
            let unit = match code with
                        | HK _ -> 1000.0
                        | _ -> 100.0
            valuePart.Substring(3, valuePart.Length - 10).Split(",")
            |> Seq.map (float >> (fun x -> x / unit))
            |> Seq.toArray
            |> fun s ->
                Some {
                    Price = s.[0]
                    Open = s.[1]
                    High = s.[2]
                    Low = s.[3]
                    Updown = s.[4]
                    UpdownRate = s.[5]
                    ExchangeRatio = s.[6]
                }
        with
            | _ -> None

    let getStockData stock =
        async {
            let code = stock.Code
            let baseURL = sprintf "http://%s.%s/%s/quotelist" (getHostPrefix code) baseHost (getRegion code)
            let url = sprintf "%s?code=%s&column=%s&callback=%s" baseURL (getCodeString code) column callback
            let! resp = Http.AsyncRequest url
            if resp.StatusCode > 300 then
                return { Stock = stock; Data = None }
            else
                let stockData = resp.Body.ToString() |> parseData code
                return { Stock = stock; Data = stockData }
        }

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printfn "Usage: TStock [filename]"
        exit 1

    let filename = argv.[0]

    if not(File.Exists(filename)) then
        printfn "File %s do not exist, please check your input!" filename
        exit 1

    let content = File.ReadAllText(filename)

    let follows = Json.deserialize<Stock[]>(content)

    printfn "|%-20s|%10s|%10s|%10s|%10s|%10s|%10s|" "Name" "Price" "Open" "Low" "High" "UpDown" "UpDownRate"

    follows
    |> Seq.map HexunStock.getStockData
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.iter
        (fun result ->
            match result with
            | result when result.Data = None -> printfn "|%-20s|%10s|%10s|%10s|%10s|%10s|%10s|" result.Stock.Name "_" "_" "_" "_" "_" "_"
            | { StockResult.Stock = stock; StockResult.Data = Some data } ->
                printfn "|%-20s|%10.2f|%10.2f|%10.2f|%10.2f|%10.2f|%10.2f%%|" stock.Name data.Price data.Open data.Low data.High data.Updown data.UpdownRate
            | _ -> ()
        )
    0
