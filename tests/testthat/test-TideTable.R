context("Equal return TideTable, SynTT")
library(TideTables)

tt_tt <- TideTable(dataInput=observation, asdate=observation$observation_date[1], 
          astime=observation$observation_time[1], 
          aedate="1991/05/01", aetime="21:00:00", ssdate="1995/01/01", 
          sstime="00:00:00", sedate="1995/01/31", setime="21:00:00")[["c.table"]]

tt_syn <- SynTT(tmodel = BuildTT(dataInput = observation, asdate = observation$observation_date[1],
                                 astime = observation$observation_time[1],
                                 aedate = "1991/05/01", aetime = "21:00:00"),
                ssdate = "1995/01/01", sstime = "00:00:00", sedate = "1995/01/31", setime = "21:00:00")

test_that("equal return values of TideTable vs BuildTT+SynTT", {
  expect_equal(tt_tt, tt_syn)
})
