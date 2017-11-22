Changes in Version 1.2.1 (2017-11-22)
--------------------------------------------------------

BUG FIXES

* `PrepareData` now correctly handles the case where mulitple variables
  are given as a list in a single element of the list supplied to the
  "input.data.raw" argument and a distribution `chart.type`is
  requested.  Relevant in Displayr when a user supplies variables
  without a grouping variable and selects a distribution chart type. (DS-1659)


Changes in Version 1.0.0 (2017-10-20)
--------------------------------------------------------

NEW FEATURES

* New functions `PrepareData`, `PrepareNumbers`, and `PrepareColors`
for preparing data, number formats, and colors for use with charts.
Moved from their original location in flipChartBasics (DS-1471)
