module Gigasecond

open System

let gigasecond : TimeSpan = TimeSpan.FromSeconds 1e9

let add (beginDate : DateTime) : DateTime =
    beginDate.Add gigasecond
