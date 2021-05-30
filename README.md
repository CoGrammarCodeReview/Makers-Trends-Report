# Makers_trend_report

Script to generate trend reports for Makers Academy based on
[Katherine James' version](https://github.com/CoGrammarCodeReview/Makers_trend_report).

This script was written (in F#) as a rewrite of the original to improve
maintainability and to automate some of the original process. It should
be easier to add trends to the reports.

Trend reports are provided by us to Makers every two weeks on a Monday.

## Dependencies

All dependencies are handled by `dotnet fsi` via the first lines in the
script.

## Process

* Go to https://airtable.com/shrwOi72LcgFIdRR5/tblWkpfb3H87FreC3
* Select download CSV from the … tab.
* Run Makers.fsx using `dotnet fsi Makers.fsx`
* Enter the dates to calculate the report for:
    * The first date should be the Monday of the last two-week cycle
    * The second date should be the Monday of the next two-week cycle (which is excluded)
* You will have to manually filter the students flagged for attention.
  *  When the script prompts for a Y/n for flagging, check if the dev
    has been flagged manually on Slack
* Search your email for the cancellations filtered by date when prompted.
* Create a post in the Slack channel.
* Paste the report and format it nicely.