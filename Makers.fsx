#r "nuget: Deedle"
#r "nuget: NodaTime"

open System
open System.IO
open System.Globalization
open Deedle
open NodaTime
open NodaTime.Text

type Report =
    { Start: LocalDateTime
      End: LocalDateTime
      TotalReviews: int
      General: Map<string, int>
      Tdd: Map<string, int>
      RequirementsGathering: Map<string, int>
      Debugging: Map<string, int>
      Weeks: Map<string, int>
      Surprises: List<string>
      Flags: List<string> }

module Format =

    let culture = CultureInfo "en-GB"

    let date = "d/M/yyyy"

    let dateTime = $"{date} h:mmtt"

    let longDate = "d MMM yyyy"

module Column =

    let date = "Date"

    let uuid = "Review"

    let general = "General aspects about the review"

    let surprises = "New trend or surprising behaviour"

    let tdd = "TDD process"

    let requirements = "Requirements-gathering process"

    let debugging = "Debugging process"

    let week = "Week (from Review)"

    let trends column = $"Trends - {(column: string)}"

module Header =

    let frequency = "Trends frequency"

    let general = "General"

    let tdd = Column.tdd

    let debugging = Column.debugging

    let requirements = Column.requirements

    let week = "Review weeks"

    let surprises = "Surprising behaviour"

    let flags = "Devs flagged for attention (with at least 4 negative trends and no notable improvement)"

module Trend =

    let cancellations = "Cancellations"

    let positive = "Notable improvement between sessions"

    let reviewTotal = "Total reviews during this period"

    let exclusions = [ "No-show"; "No UUID provided"; positive; "UUID error" ]

    let containers = [ Column.tdd; Column.requirements; Column.debugging; Column.general ]

module Read =

    module private Message =

        let start = "Start date: "

        let end' = "End date: "

        let csvPath = "Reviews CSV path: "

        let cancellations = "Number of cancellations in the period: "

        let acceptFlags = "Y/n to accept or reject these flags:"

        let reportPath = "Target report path: "

    let private input message =
        printf "%s" message
        Console.ReadLine()

    let private parseDate schema message =
        let pattern =
            LocalDateTimePattern.Create(schema, Format.culture)
        let result = pattern.Parse message
        if result.Success then
            Some result.Value
        else
            None

    let rec private userDate message () =
        match message |> input |> parseDate Format.date with
        | Some date -> date
        | None -> userDate message ()

    let private startDate = userDate Message.start

    let private endDate = userDate Message.end'

    let private csvDate value = parseDate Format.dateTime value

    let rec private archive () =
        try
            input Message.csvPath
            |> Frame.ReadCsv
            |> Frame.rows
        with :? FileNotFoundException -> archive ()

    let rec private reviews archive startDate endDate =
        (archive: Series<int, ObjectSeries<string>>)
        |> Series.filterValues
            (fun r ->
                let date = r.GetAs<string> Column.date |> csvDate
                date >= startDate && date < endDate)

    let rec cancellations () =
        let userInput = input Message.cancellations
        try
            Int32.Parse userInput
        with :? FormatException -> cancellations ()

    let acceptFlags names =
        printfn "%s" Message.acceptFlags
        let rec predicate name =
            match input $"{name}: " |> fun s -> s.ToLower() with
            | "y"
            | "yes" -> true
            | "n"
            | "no" -> false
            | _ -> predicate name
        List.filter predicate names

    let target () = input Message.reportPath

    let report () =
        let start = startDate ()
        let end' = endDate ()
        let archive = archive ()
        {| StartDate = start
           EndDate = end'
           Reviews = reviews archive (Some start) (Some end')
           Archive = archive |}

module Evaluate =

    let private reviewCount rows = Series.countValues rows

    let private trend column label rows =
        let column = Column.trends column
        let filter (value: string) = value.Contains(label: string)
        Series.filterValues (fun (r: ObjectSeries<string>) -> r.GetAs<string> column |> filter) rows
        |> Series.countValues
        |> fun count -> label, count

    let private category label trends rows =
        List.map (fun t -> trend label t rows) trends

    let private surprisingTrends rows =
        let column = Column.trends Column.surprises
        Series.filterValues
            (fun (r: ObjectSeries<string>) ->
                r.GetAs<string> column
                |> String.IsNullOrWhiteSpace
                |> not)
            rows
        |> Series.mapValues (fun (r: ObjectSeries<string>) -> r.GetAs<string> column)
        |> Series.values
        |> List.ofSeq

    let private countFolder counts trend =
        let count =
            match Map.tryFind trend counts with
            | None -> 1
            | Some count -> count + 1
        Map.add trend count counts

    let private countTrend column rows =
        (rows: Series<'a, ObjectSeries<string>>)
        |> Series.mapValues (fun r -> r.GetAs<string> <| Column.trends column)
        |> Series.values
        |> Seq.collect
            (fun s ->
                s.Split ","
                |> Array.filter (String.IsNullOrEmpty >> not))
        |> Seq.fold countFolder Map.empty

    let private countNegativeTrend category rows =
        let trends = countTrend category rows
        List.fold (fun trends excludedTrend -> Map.remove excludedTrend trends) trends Trend.exclusions

    let private getName (row: ObjectSeries<string>) = row.GetAs<string> Column.uuid

    let private lastImproved name rows =
        rows
        |> Series.filterValues (fun (r: ObjectSeries<string>) -> getName r = name)
        |> Series.lastValue
        |> (fun r ->
            Column.trends Column.general
            |> r.GetAs<string>)
        |> (fun s -> s.Contains Trend.positive)

    let private hadSingleReview archive name =
        archive
        |> Series.filterValues (fun (r: ObjectSeries<string>) -> getName r = name)
        |> Series.countValues
        |> (=) 1

    let private hasAtLeast4NegativeTrends (row: ObjectSeries<string>) =
        Trend.containers
        |> List.map (fun category -> countNegativeTrend category ([ row ] |> Series.ofValues))
        |> List.fold (fun count value -> Map.count value + count) 0
        |> (fun count -> count >= 4)

    let private flag archive acceptFlags rows =
        rows
        |> Series.filterValues hasAtLeast4NegativeTrends
        |> Series.filterValues
            (fun (r: ObjectSeries<string>) ->
                getName r
                |> fun name -> not <| lastImproved name rows)
        |> Series.mapValues getName
        |> Series.values
        |> Seq.distinct
        |> Seq.filter (fun name -> hadSingleReview archive name |> not)
        |> Seq.toList
        |> (acceptFlags: List<string> -> List<string>)

    let private weekCount rows =
        rows
        |> Series.mapValues (fun (r: ObjectSeries<string>) -> r.GetAs<string> Column.week)
        |> Series.foldValues countFolder Map.empty

    let private tdd rows = countTrend Column.tdd rows

    let private general cancellations rows =
        let counts =
            countTrend Column.general rows
        Map.add Trend.cancellations (cancellations ()) counts

    let private requirementsGathering rows =
        countTrend Column.requirements rows

    let private debugging rows = countTrend Column.debugging rows

    let report archive start end' cancellations acceptFlags reviews =
        { Start = start
          End = end'
          TotalReviews = reviewCount reviews
          General = general cancellations reviews
          Tdd = tdd reviews
          RequirementsGathering = requirementsGathering reviews
          Debugging = debugging reviews
          Weeks = weekCount reviews
          Surprises = surprisingTrends reviews
          Flags = flag archive acceptFlags reviews }

module Print =

    let private title startDate endDate =
        let pattern =
            LocalDateTimePattern.Create(Format.longDate, Format.culture)
        $"Trend report for period: {pattern.Format startDate} - {pattern.Format endDate}\n"

    let private frequency label count = $"{label}: {count}\n"

    let private reviewTotal total = frequency Trend.reviewTotal total

    let private category folder label values = List.fold folder $"{label}:\n" values

    let private table label (frequencies: Map<string, int>) =
        let folder state (key, value) = state + frequency key value
        category folder label <| Map.toList frequencies

    let private listing label entries =
        category (fun state e -> $"{state}{e}\n") label entries

    let private (.+.) report update = report + "\n" + update

    let report target value =
        title value.Start value.End
        .+. reviewTotal value.TotalReviews
        .+. $"{Header.frequency}:\n"
        .+. table Header.general value.General
        .+. table Header.tdd value.Tdd
        .+. table Header.requirements value.RequirementsGathering
        .+. table Header.debugging value.Debugging
        .+. table Header.week value.Weeks
        .+. listing Header.surprises value.Surprises
        .+. listing Header.flags value.Flags
        |> (fun s ->
            use writer = (target () |> File.CreateText)
            fprintfn writer "%s" s)

let generateReport () =
    let data = Read.report ()
    let value =
        Evaluate.report
            data.Archive
            data.StartDate
            data.EndDate
            Read.cancellations
            Read.acceptFlags
            data.Reviews
    Print.report Read.target value

generateReport () // Entry point
