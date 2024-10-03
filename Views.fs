module Views

open Giraffe.ViewEngine
open Common

let classes = String.concat " "

let tabButton airport airportClass =
    let baseCssCls = "tablink"

    let airportCssCls =
        match airportClass with
        | Bravo -> "airport-bravo"
        | Charlie -> "airport-charlie"
        | Delta -> "airport-delta"
        | Echo -> "airport-echo"

    let cls = classes [ baseCssCls; airportCssCls ]
    let onclick = $"openpage('{airport}', this)"

    button [ _class cls; _onclick onclick ] [ str airport ]

let echoToggleButton () =
    let onclick = "toggleEchoRow()"
    button [ _class "tablink"; _onclick onclick ] [ str "E" ]

let echoAirportRow (airports: Airport seq) =
    let echoAirports = airports |> Seq.filter (fun a -> a.Class = Echo) |> Seq.map (fun a -> tabButton a.Id a.Class) |> Seq.toList
    div [ _id "echoRow"; _class "hidden bg-gray-900 p-1 flex flex-row flex-wrap justify-start items-start" ] echoAirports

let navBar (airports: Airport seq) =
    let mainButtons = 
        airports
        |> Seq.filter (fun a -> a.Class <> Echo) // Exclude Echo airports
        |> Seq.map (fun a -> tabButton a.Id a.Class)
        |> Seq.toList
    let echoButton = echoToggleButton ()
    nav [ _class "nav flex items-center" ] (mainButtons @ [echoButton])

let chartButton (chart: Chart) =
    let baseCssCls = "chartbutton"

    let chartCssCls =
        match chart.Type with
        | APD -> "chartbutton-apd"
        | MIN -> "chartbutton-min"
        | LAH -> "chartbutton-lah"
        | HOT -> "chartbutton-hot"
        | STAR -> "chartbutton-star"
        | IAP -> "chartbutton-iap"
        | DP -> "chartbutton-dp"
        | DAU -> "chartbutton-dau"

    let cls = classes [ baseCssCls; chartCssCls ]
    let chartId = $"{chart.Airport} {chart.Name}"
    let onclick = $"openpdf('{chart.PdfPath}', this)"

    div [ _id chartId; _class cls; _data "url" chart.PdfPath; _onclick onclick ] [ str chart.Name ]

let tabContent airportId (charts: Chart seq) =
    let cls = "tabcontent flex flex-row flex-wrap justify-start items-start hidden"

    div [ _id airportId; _class cls ] (Seq.map chartButton charts |> Seq.toList)

let pdfViewer =
    let baseCssCls = "hidden w-full h-full"
    let __data = attr "data"

    object
        [ _id "pdfViewer"
          _class baseCssCls
          _type "application/pdf"
          _width "100%"
          _height "100%"
          __data "" ]
        []

let renderPage (pageTitle: string) (viewModel: (Airport * Chart list) seq) =

    let htmlStyle = classes []

    let navContentSection = viewModel |> Seq.map fst |> navBar
    let echoAirportSection = viewModel |> Seq.map fst |> echoAirportRow // Add echo airport row

    let tabContentSection =
        viewModel
        |> Seq.map (fun (airport, charts) -> tabContent airport.Id charts)
        |> Seq.toList
    html
        [ _class "html h-full" ]
        [ head
              []
              [ title [] [ str pageTitle ]
                link [ _rel "stylesheet"; _href "style.css" ]
                link [ _rel "icon"; _type "image/x-icon"; _href "favicon.ico" ]
                script [ _src "script.js" ] [] ]
          body
              [ _class "body h-full" ]
              [ div [] [ navContentSection ]
                div [] [ echoAirportSection ] // Include echo row section
                div
                    [ _class "flex flex-row h-full" ]
                    [ div [ _class "w-1/3" ] tabContentSection
                      div [ _class "w-2/3" ] [ pdfViewer ] ] ]
          footer [] [] ]
