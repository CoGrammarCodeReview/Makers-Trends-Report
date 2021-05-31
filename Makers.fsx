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

    let general = "General aspects about the review"

    let surprises = "New trend or surprising behaviour"

    let tdd = "TDD process"

    let requirements = "Requirements-gathering process"

    let debugging = "Debugging process"

    let week = "Week (from Review)"

    let trends column = $"Trends - {(column: string)}"

module Read =

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

    let private startDate = userDate "Start date: "

    let private endDate = userDate "End date: "

    let private csvDate value = parseDate Format.dateTime value

    let rec private archive () =
        try
            input "CSV path: "
            |> Frame.ReadCsv
            |> Frame.rows
        with :? FileNotFoundException -> archive ()

    let rec private reviews archive startDate endDate =
        (archive: Series<int, ObjectSeries<string>>)
        |> Series.filterValues
            (fun r ->
                let date = r.GetAs<string> "Date" |> csvDate
                date >= startDate && date < endDate)

    let rec cancellations () =
        let userInput = input "Cancellations: "
        try
            Int32.Parse userInput
        with :? FormatException -> cancellations ()

    let acceptFlags names =
        printfn "Y/n to accept or reject these flags:"
        let rec predicate name =
            match input $"{name}: " |> fun s -> s.ToLower() with
            | "y"
            | "yes" -> true
            | "n"
            | "no" -> false
            | _ -> predicate name
        List.filter predicate names

    let target () = input "Target path: "

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

    let private trendCategories =
        [ Column.tdd
          Column.requirements
          Column.debugging
          Column.general ]

    let private positiveTrend = "Notable improvement between sessions"

    let private excludedTrends =
        [ "No-show"
          "No UUID provided"
          positiveTrend
          "UUID error" ]

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
        List.fold (fun trends excludedTrend -> Map.remove excludedTrend trends) trends excludedTrends

    let private getName (row: ObjectSeries<string>) = row.GetAs<string> "Review"

    let private lastImproved name rows =
        rows
        |> Series.filterValues (fun (r: ObjectSeries<string>) -> getName r = name)
        |> Series.lastValue
        |> (fun r ->
            Column.trends Column.general
            |> r.GetAs<string>)
        |> (fun s -> s.Contains positiveTrend)

    let private hadSingleReview archive name =
        archive
        |> Series.filterValues (fun (r: ObjectSeries<string>) -> getName r = name)
        |> Series.countValues
        |> (=) 1

    let private hasAtLeast4NegativeTrends (row: ObjectSeries<string>) =
        trendCategories
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
        Map.add "Cancellations" (cancellations ()) counts

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

    let private reviewTotal total =
        frequency "Total reviews during this period" total

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
        .+. "Trends frequency:\n"
        .+. table "General" value.General
        .+. table "TDD process" value.Tdd
        .+. table "Requirements-gathering process" value.RequirementsGathering
        .+. table "Debugging process" value.Debugging
        .+. table "Review week frequencies" value.Weeks
        .+. listing "Surprising behaviour" value.Surprises
        .+. listing
                "Devs flagged for attention (with at least 4 negative trends and no notable improvement)"
                value.Flags
        |> (fun s ->
            use writer = (target () |> File.CreateText)
            fprintfn writer "%s" s)

let report () =
    let report = Read.report ()
    let report =
        Evaluate.report
            report.Archive
            report.StartDate
            report.EndDate
            Read.cancellations
            Read.acceptFlags
            report.Reviews
    Print.report Read.target report

report () // Entry point
