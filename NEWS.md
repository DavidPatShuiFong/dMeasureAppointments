---
title: "NEWS"
author: "David Fong"
date: "09/10/2021"
output: html_document
---

# 1.2.0
16th October 2021

## New

* send SMS reminders for appointments via 'Join'
* `initialize_configuration_db` (exported function) and `$read_configuration_db` (method)
  + integration with `dMeasure` configuration database
  + `initialize_join_config` and `initialize_sms_config` return lists to assist setting up configuration databases
  + methods to write, delete and modify the 'Join' configuration database
+ user interface (UI and server) for configuring 'Join' and 'SMStext' with DailyMeasure/GPstat

# 1.1.0
15th October 2021

## New

* appointment table with telephone numbers
  + `$appointments_telephone` (exported) appends telephone numbers to `dMeasure$appointments_filtered_time`
* `$sms_join` (export) send SMS with Join
  + Join is by joaoapps https://joaoapps.com/join/

# 1.0.1
12th October 2021

## Changes

* Modularization of user interface

# dMeasureAppointments 1.0.0
10th October 2021

* working release

## New

* add `sidebarmenuPriority` to `fct_integration.R`
  - allows 'high' priority in sidebar menu display
  
## Changes

* removed 'Print view' - which was not being used

# dMeasureAppointments 0.0.0.9000
9th October 2021

* Added a `NEWS.md` file to track changes to the package.
