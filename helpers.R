parseFoundDate <- function(wpt) {
  log = 1

  while(1) {
    type = xmlValue(wpt[[ "cache" ]][[ "logs" ]][[log]][["type"]])
    
    if(type != "Found it" && type != "Webcam Photo Taken" && type != "Attended") {
      log = log + 1
    } else {
      break
    }
  }

  d = xmlValue(wpt[[ "cache" ]][[ "logs" ]][[log]][["date"]])
  n = nchar(d)

  if(grepl("+", d, fixed=T)) {
    # groundspeak(?) appears to screw up adding timezone, just delete it...
    p = str_locate(d, "\\+")[1]
    d = substr(d, 1, p-1)
  }

  # PST/PDT timezone adjustment...
  # https://www.timeanddate.com/time/zone/usa/los-angeles
  o = 8
  if(d > "2000-04-02 09:00:00 UTC" && d < "2000-10-29 09:00:00 UTC") o = 7
  if(d > "2001-04-01 09:00:00 UTC" && d < "2001-10-28 09:00:00 UTC") o = 7
  if(d > "2002-04-07 09:00:00 UTC" && d < "2002-10-27 09:00:00 UTC") o = 7
  if(d > "2003-04-06 09:00:00 UTC" && d < "2003-10-26 09:00:00 UTC") o = 7
  if(d > "2004-04-04 09:00:00 UTC" && d < "2004-10-31 09:00:00 UTC") o = 7
  if(d > "2005-04-03 09:00:00 UTC" && d < "2005-10-30 09:00:00 UTC") o = 7
  if(d > "2006-04-02 09:00:00 UTC" && d < "2006-10-29 09:00:00 UTC") o = 7
  if(d > "2007-03-11 09:00:00 UTC" && d < "2007-11-04 09:00:00 UTC") o = 7
  if(d > "2008-03-09 09:00:00 UTC" && d < "2008-11-02 09:00:00 UTC") o = 7
  if(d > "2009-03-08 09:00:00 UTC" && d < "2009-11-01 09:00:00 UTC") o = 7
  if(d > "2010-03-14 09:00:00 UTC" && d < "2010-11-07 09:00:00 UTC") o = 7
  if(d > "2011-03-13 09:00:00 UTC" && d < "2011-11-06 09:00:00 UTC") o = 7
  if(d > "2012-03-11 09:00:00 UTC" && d < "2012-11-04 09:00:00 UTC") o = 7
  if(d > "2013-03-10 09:00:00 UTC" && d < "2013-11-03 09:00:00 UTC") o = 7
  if(d > "2014-03-09 09:00:00 UTC" && d < "2014-11-02 09:00:00 UTC") o = 7
  if(d > "2015-03-08 09:00:00 UTC" && d < "2015-11-01 09:00:00 UTC") o = 7
  if(d > "2016-03-13 09:00:00 UTC" && d < "2016-11-06 09:00:00 UTC") o = 7
  if(d > "2017-03-12 09:00:00 UTC" && d < "2017-11-05 10:00:00 UTC") o = 7
  if(d > "2018-03-11 09:00:00 UTC" && d < "2018-11-04 10:00:00 UTC") o = 7
  if(d > "2019-03-10 09:00:00 UTC" && d < "2019-11-03 10:00:00 UTC") o = 7
  if(d > "2020-03-08 09:00:00 UTC" && d < "2020-11-01 10:00:00 UTC") o = 7
  if(d > "2021-03-14 09:00:00 UTC" && d < "2021-11-07 10:00:00 UTC") o = 7
  if(d > "2022-03-13 09:00:00 UTC" && d < "2022-11-06 10:00:00 UTC") o = 7
  if(d > "2023-03-12 09:00:00 UTC" && d < "2023-11-05 10:00:00 UTC") o = 7
  if(d > "2024-03-10 09:00:00 UTC" && d < "2024-11-03 10:00:00 UTC") o = 7
  if(d > "2025-03-09 09:00:00 UTC" && d < "2025-11-02 10:00:00 UTC") o = 7

  if(as.numeric(substr(d, 12, 13)) >= o) {
    d = substr(d,1,10)
  } else {
    d = as.character(as.Date(substr(d,1,10))-1)
  }

  return(d)
}

target_needed <- function(target, average, count, level) {
  if((target > average & level > target) | (target < average & level < target )) {
    return(ceiling((target - average) * count / (level - target)))
  }

  return("-")
}


getBackgroundColour <- function(tt, d, t, m) {
  if(tt[d,t] == 0) 
    return("#E0D3C5")
  
  if(tt[d,t] == m)
    return("#c83200; font-weight: bold")
  
  
  col = floor(log(tt[d,t])/log(m) * 180 + 20)
  
  return(paste("#", as.hexmode(col), "c800", sep=""))
}

getTextColour <- function(s, v, m) {
  if(s[v] == m)
    return("#c83200")

  return("inherit")
}
