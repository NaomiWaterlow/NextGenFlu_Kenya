# epidemics_list.R 

epidemics_list <- list(
  epidemic1 = list(start_date = as.Date(c("2010-03-12")), 
                   end_date = as.Date(c("2010-12-17")), 
                   year = "2010", 
                   flutype = "AH3N2", 
                   previous_epi = NA
                   ),
  epidemic2 = list(start_date = as.Date(c("2010-12-17")), 
                   end_date = as.Date(c("2011-08-05")), 
                   year = "2011", 
                   flutype = "B", 
                   previous_epi = NA
                   ),
  epidemic3 = list(start_date = as.Date(c("2011-08-12")), 
                   end_date = as.Date(c("2012-03-16")), 
                   year = "2011", 
                   flutype = "B", 
                   previous_epi = 2
  ),
  epidemic4 = list(start_date = as.Date(c("2011-12-23")), 
                   end_date = as.Date(c("2012-05-11")), 
                   year = "2012", 
                   flutype = "AH3N2", 
                   previous_epi = 1
  ),
  epidemic5 = list(start_date = as.Date(c("2013-05-03")), 
                   end_date = as.Date(c("2013-12-13")), 
                   year = "2013", 
                   flutype = "B", 
                   previous_epi = 3
  ),
  epidemic6 = list(start_date = as.Date(c("2013-12-20")), 
                   end_date = as.Date(c("2014-09-05")), 
                   year = "2014", 
                   flutype = "AH1N1", 
                   previous_epi = NA
  ),
  epidemic7 = list(start_date = as.Date(c("2015-11-27")), 
                   end_date = as.Date(c("2016-05-20")), 
                   year = "2016", 
                   flutype = "B", 
                   previous_epi = 5
  ),
  epidemic8 = list(start_date = as.Date(c("2016-03-25")), 
                   end_date = as.Date(c("2016-11-25")), 
                   year = "2016", 
                   flutype = "AH3N2", 
                   previous_epi = 4
  ),
  epidemic9 = list(start_date = as.Date(c("2017-09-01")), 
                   end_date = as.Date(c("2018-06-22")), 
                   year = "2018", 
                   flutype = "B", 
                   previous_epi = 7
  ),
  epidemic10 = list(start_date = as.Date(c("2018-01-19")), 
                   end_date = as.Date(c("2018-10-12")), 
                   year = "2018", 
                   flutype = "AH1N1", 
                   previous_epi = 6
  ),
  epidemic11 = list(start_date = as.Date(c("2018-06-15")), 
                       end_date = as.Date(c("2018-12-14")), 
                       year = "2018", 
                       flutype = "AH3N2",
                    previous_epi = 8
  )
  
  )
  
 

 
