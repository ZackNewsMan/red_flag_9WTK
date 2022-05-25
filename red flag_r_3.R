# After additional reporting and consultation of more sources, 
  # we decided to put temporary red flag (TRPO, 14 day) and year long (ERPO's) into one pool of cases. 
  # I will use the distinct function to ID unique cases. 
  # Similar code from the first attempt at this analysis will work well. 
# Vaughan and I built out the red flag data with ID's. So I am going to work with that data. 

library(tidyverse)

red_ids <- red_flag_w_ids_010120_111121_5 

red_ids %>% 
  distinct(case_number, .keep_all = TRUE)

# Shows 245 rows but there is a blank appearing in the data as a unique row for some reason. Going to delete and re-import
  # Real number of cases is 244 bc of that blank

red_ids <- red_flag_w_ids_010120_111121_6

# Going to bring in again. 
  # Repeat under D0682021CV030005. Data entry error. Should be filed by DPD but respondent was accidentally entered as the file under his name, Alexander Braden. 
  # I searched in the court records and confirmed that it was DPD that filed it. 
    # These are the SQL queries that I used to check:
  
      # CREATE TABLE uniq_leo_app AS
      # SELECT DISTINCT(case_number), law_enforcement, county_name
      # FROM red_ids
  
      # SELECT law_enforcement, count(law_enforcement)
      # FROM uniq_leo_app
      # GROUP BY law_enforcement
      
      # SELECT case_number, count(case_number)
      # FROM uniq_leo_app
      # GROUP BY case_number
      # ORDER BY count(case_number) DESC

red_ids <- red_flag_w_ids_010120_111121_7

  # Problem after that was I changed the charged/filing by party to DPD, but the formula wasn't active so I needed to manually change the LEO column to be "Yes."
    # Otherwise, it would show up as Yes and No in the column falsely. 

red_ids <- red_flag_w_ids_010120_111121_8

red_ids <- red_flag_w_ids_010120_111121_9

library(tidyverse)

red_ids %>% 
  distinct(case_number, .keep_all = TRUE)

# Now shows 244 unique cases.
  # Verified by SQL:
    # SELECT DISTINCT(case_number)
    # FROM red_ids

  unique_app <- red_ids %>% 
  distinct(case_number, .keep_all = TRUE)

  # How many apps with law enforcement involvement?
  
  red_ids %>% 
    distinct(case_number, .keep_all = TRUE) %>% 
  group_by(law_enforcement) %>% 
    summarize(count = n())

  # law_enforcement count
    # No                137
    # Yes               107
  
    # SQL verified now that I fixed the problem 
  
      # CREATE TABLE uniq_leo_app AS
      # SELECT DISTINCT(case_number), law_enforcement, county_name
      # FROM red_ids_8
      
      # SELECT law_enforcement, count(law_enforcement)
      # FROM uniq_leo_app
      # GROUP BY law_enforcement
      
      # To check for repeats:
        # SELECT case_number, count(case_number)
        # FROM uniq_leo_app
        # GROUP BY case_number
        # ORDER BY count(case_number) DESC
    
  unique_app %>% 
    group_by(law_enforcement) %>% 
    summarize(count = n())
  
  
# Going to do cases with just ERPO, cases with just TRPO. 
  # Then full join both of them into one data frame
  # Then do a distinct filter to see how many unique cases were approved for either a 14-day or year-long red flag. 

# TRPO (14-day red flag)

red_ids %>% 
  filter(event_code == "TRPO") %>% 
  distinct(case_number, .keep_all = TRUE) %>% 
  View()

  # Worked. 140 cases 
    # SQL matched but added duplicates for some reason if I had more than these columns in the command line. 
    # And made table:
      # CREATE TABLE temp_approved AS
      # SELECT DISTINCT(case_number), district_nbr, county_name, respondent, dob, filed_by, notes, law_enforcement, case_file_date, case_file_year, event_code, event_code_description
      # FROM red_ids
      # WHERE event_code = "TRPO"
    
    temp_approved <- red_ids %>% 
      filter(event_code == "TRPO") %>% 
      distinct(case_number, .keep_all = TRUE) 
    
# And year-long (ERPO)

    red_ids %>% 
      filter(event_code == "ERPO") %>% 
      distinct(case_number, .keep_all = TRUE) %>% 
      View()
  
    # 99 cases with an approved year-long protection order
      # Vetted by SQL:
        # SELECT DISTINCT(case_number), district_nbr, county_name, respondent, dob, filed_by, notes, law_enforcement, case_file_date, case_file_year, event_code, event_code_description
        # FROM red_ids
        # WHERE event_code = "ERPO"
    
    year_approved <- red_ids %>% 
      filter(event_code == "ERPO") %>% 
      distinct(case_number, .keep_all = TRUE)

# Now let's put them together
    
    temp_approved %>% full_join(year_approved) %>% 
      View()
    
    # Worked! 239 columns (140 temp + 99 year = 239) and a peek under the hood shows both ERPO and TRPO cases in that data frame. 
      # Apparently there's not a full join equivalent in SQLite. There's a convaluted workaround but I think we're good because of the checking done in line 71.
        # https://searchdatacenter.techtarget.com/answer/MySQL-full-outer-join-workaround
    
    # Will manually put them together and do DISTINCT in SQL to verify
    
      year_approved %>% write_csv("year_approved.csv", na = "")
    
      temp_approved %>% write_csv("temp_approved.csv", na = "")
        
    all_approved <- temp_approved %>% full_join(year_approved)
    
    all_approved %>% write_csv("all_approved.csv", na = "")
    
    # And only pull distinct cases
    
    all_approved %>% 
      distinct(case_number, .keep_all = TRUE) %>% 
      View()
    
    # So - 146 unique cases where there was either a temp or year-long approval
      # 146 cases verified in SQL:
        # SELECT DISTINCT(case_number)
        # FROM all_approved
    
    uniq_all_approved <- all_approved %>% 
      distinct(case_number, .keep_all = TRUE)
    
    # Thus - 146 cases were approved out of 244 cases. 
      # See lines 14 and 22.
    
    (146/244)*100
    
    # 60% of red flag cases got some sort of protection order approved. 
      # Aka 3 in 5 approved
      # 2 in 5 denied. 
    
# More likely to get approved if law enforcement?    

    uniq_all_approved %>% 
      group_by(law_enforcement) %>% 
      summarize(count = n()) 

    #  law_enforcement count
      # No                 44
      # Yes               102
    
    # SQL verified:
    
      # Because SQL can't do a full join, I exported each part and then stitched it together in a new CSV
    
        # Will manually put them together and do DISTINCT in SQL to verify
        
        year_approved %>% write_csv("year_approved.csv", na = "")
        
        temp_approved %>% write_csv("temp_approved.csv", na = "")
        
      # CREATE TABLE uniq_leo_app AS
      # SELECT DISTINCT(case_number), law_enforcement, county_name
      # FROM all_approved
      
      # SELECT law_enforcement, count(law_enforcement)
      # FROM uniq_leo_app
      # GROUP BY law_enforcement
      
    # Out of 146 unique cases with approval:
      # 101 approved were law enforcement
    
        (102/146)*100
        
          # 69% of approved cases were law enforcement
    
      # 44 were not 
    
        (44/146)*100
    
          # 30% of approved cases were NOT law enforcement. 
            #  So - you are more likely to get your petition approved if it comes from a LEO. 
      # The CO AG published a report on the first year of the red flag order. They had similar percentages but not the exact same. I have most of 2021 in this data (until mid-November 2021).
       # See page 11 for law enforcement section: https://coag.gov/app/uploads/2021/08/First-year-implementation-of-Colorado-violence-prevention-act.pdf  
  
      # Want to see if there are counties were this gap is larger, but need to do other work to pull out other county info. See below.

      # Shown above but worth emphasizing again:
    
     unique_app %>% 
      group_by(law_enforcement) %>% 
      summarize(count = n())
    
         # law_enforcement count
        # No                137
        # Yes               107
     
      # So - law enforcement applied for a red flag protection 107 times -- and were approved 101 times. That's 94%
      # Citizens applied 137 times and were approved 44 times
    
              ((44/137)*100)
     
        # That's 32%
     
# Let's do all the denials to make sure I am not missing anything crazy
    
    # TRPD (14-day red flag DENIED)
    
    red_ids %>% 
      filter(event_code == "TRPD") %>% 
      distinct(case_number, .keep_all = TRUE) %>% 
      View()
    
      # 61 denied for a temp order
    
        temp_denied <- red_ids %>% 
          filter(event_code == "TRPD") %>% 
          distinct(case_number, .keep_all = TRUE)
    
    # And year-long DENIALS (ERPD)
    
    red_ids %>% 
      filter(event_code == "ERPD") %>% 
      distinct(case_number, .keep_all = TRUE) %>% 
      View()
    
    # 42 year-long denials
    
    yr_denied <- red_ids %>% 
      filter(event_code == "ERPD") %>% 
      distinct(case_number, .keep_all = TRUE)
    
   # Let's put it together:
    
    all_denied <- temp_denied %>% full_join(yr_denied) 
    
    # And filter out for only unique case numbers
    
      uniq_all_denied <- all_denied %>% 
        distinct(case_number, .keep_all = TRUE)
    
    # So there are 102 cases that have had some sort of denial along the way in the process.  
               
# Approvals per county
        
      uniq_all_approved %>% 
        group_by(county_name) %>% 
        summarize(count_approved = n()) %>% 
        arrange(desc(count_approved)) 
      
     # Denver has the most number of approved cases
     # For export:
      
      uniq_county_approved <- uniq_all_approved %>% 
        group_by(county_name) %>% 
        summarize(count_approved = n()) %>% 
        arrange(desc(count_approved))
      
      uniq_county_approved  %>% write_csv("uniq_county_approved .csv", na = "")
      
      
      
# Cases per county 
      
      red_ids %>% 
        distinct(case_number, .keep_all = TRUE) %>% 
        group_by(county_name) %>% 
        summarize(count_approved = n()) %>% 
        arrange(desc(count_approved)) 
     
      # Denver leads with 63 cases. 
      # SQL verifies with two-part query: 
        # CREATE TABLE uniq_case_county2 AS
        # SELECT DISTINCT(case_number), county_name
        # FROM red_ids
        
        # SELECT county_name, count(case_number)
        # FROM uniq_case_county2
        # GROUP BY county_name
        # ORDER BY count(case_number) DESC
      
     # Going to export and then merge together (will lose some counties if I do a full join because there are some counties that may have had a case but not an approval).
      
       uniq_county_cases <- red_ids %>% 
        distinct(case_number, .keep_all = TRUE) %>% 
        group_by(county_name) %>% 
        summarize(count_approved = n()) %>% 
        arrange(desc(count_approved)) 
       
       uniq_county_cases %>% write_csv("uniq_county_cases.csv", na = "")
    
    # Merged it together and now let's bring it back in 
       
       library(readr)
       uniq_county_approved_cases <- read_csv("uniq_county_approved_cases.csv")
       View(uniq_county_approved_cases)
       
      # El Paso still ranks as one of the lowest approval rates. Denver as one of the highest
       
    # Will need to take a look in a similar way but for law enforcement by county. Group by county and a yes/no for law enforcement   
  
       uniq_all_approved %>% 
         group_by(county_name, law_enforcement) %>% 
         summarize(count_approved = n()) %>% 
         arrange(desc(count_approved)) %>% 
         View()
            
      # Had both "yes" and "no" in that query. Will need to restrict to just "yes"
       
       uniq_all_approved %>% 
         filter(law_enforcement == "Yes") %>% 
         group_by(county_name, law_enforcement) %>% 
         summarize(count_approved = n()) %>% 
         arrange(desc(count_approved)) %>% 
         View()
      
       # This worked well. 46 cases with an approval happened with a law enforcement filing. Most of the county
          # Verified with this two-part SQL query. Exported data used in SQL came from line 76. SQL can't do a full join. 
           # CREATE TABLE uniq_all_approved2 AS
           # SELECT DISTINCT(case_number), county_name, respondent, filed_by, law_enforcement
           # FROM all_approved
       
          # SELECT county_name, count(county_name)
          # FROM uniq_all_approved2
          # WHERE law_enforcement = "Yes"
          # GROUP BY county_name
          # ORDER BY count(county_name) DESC
       
       # Going to order alphabetically to make it easier to build into other county category
       
       leo_uniq_all_approved <- uniq_all_approved %>% 
         filter(law_enforcement == "Yes") %>% 
         group_by(county_name, law_enforcement) %>% 
         summarize(leo_count_approved = n())
       
       leo_uniq_all_approved %>% write_csv("leo_uniq_all_approved.csv", na = "")
       
        # Now citizens:
       
       cit_uniq_all_approved <- uniq_all_approved %>% 
         filter(law_enforcement == "No") %>% 
         group_by(county_name, law_enforcement) %>% 
         summarize(leo_count_approved = n())
       
       cit_uniq_all_approved %>% write_csv("cit_uniq_all_approved.csv", na = "")
  
 ############################################ next question on how many expired red flag law applied renewal ##############
       
       # Motions for Renewal Filed (MRNW)		
       # Granted (ORNG)		
       # Denied (ORND)		
       
      # renewal apps
       
       red_ids %>% 
         filter(event_code == "MRNW") %>% 
         distinct(case_number, .keep_all = TRUE) 
       
          # SQL verified, 13 rows.
            # SELECT case_number, district_nbr, county_name, respondent, dob, filed_by, notes, law_enforcement, event_file_date, event_file_year, event_code, event_code_description
            # FROM red_ids
            # WHERE event_code = "MRNW"
            # GROUP BY case_number
       
       renew_app <- red_ids %>% 
         filter(event_code == "MRNW") %>% 
         distinct(case_number, .keep_all = TRUE) 
       
        # how many of the renew apps were LEO?
       
      renew_app %>% 
         group_by(law_enforcement) %>% 
         summarize(count = n())
      
        # 12 out of 13 renewals were LEO
          # 92%
      
      # And renewal approved
       
       red_ids %>% 
         filter(event_code == "ORNG") %>% 
         distinct(case_number, .keep_all = TRUE) %>% 
         View()
       
       renew_approved <- red_ids %>% 
         filter(event_code == "ORNG") %>% 
         distinct(case_number, .keep_all = TRUE)
       
        # 8 cases with approved extensions, SQL verified:
           # CREATE TABLE "extension_approved" AS	
           # SELECT case_number, district_nbr, county_name, respondent, dob, filed_by, notes, law_enforcement, event_file_date, event_file_year, event_code, event_code_description
           # FROM red_ids
           # WHERE event_code = "ORNG"
           # GROUP BY case_number
       
       # LEO?
       
       renew_approved %>% 
         group_by(law_enforcement) %>% 
         summarize(count = n())
       
        # All 8 renewed were from LEO
       
    # denied?
       
       red_ids %>% 
         filter(event_code == "ORND") %>% 
         distinct(case_number, .keep_all = TRUE)
      
        # 1 renewal denied
        
      # see how many approved got renewed
       
       all_approved %>% left_join(
             
      # Not quite what Next is asking. They want to know: How many red flag orders that were granted have expired? If it's expired that means they could get a gun
         
        # Temp approved 
         
         temp_approved <- red_ids %>% 
           filter(event_code == "TRPO") %>% 
           distinct(case_number, .keep_all = TRUE) 
         
            # SQL verified:
             # CREATE TABLE "temp_approved" AS	
             # SELECT case_number, district_nbr, county_name, respondent, dob, filed_by, notes, law_enforcement, event_file_date, event_file_year, event_code, event_code_description
             # FROM red_ids
             # WHERE event_code = "TRPO"
             # GROUP BY case_number
         
         temp_approved  %>% write_csv("temp_approved.csv", na = "")
         
         
         # And year-long (ERPO)
         
         red_ids %>% 
           filter(event_code == "ERPO") %>% 
           distinct(case_number, .keep_all = TRUE) %>% 
           View()
         
         # 99 cases with an approved year-long protection order
         # Vetted by SQL:
         # SELECT DISTINCT(case_number), district_nbr, county_name, respondent, dob, filed_by, notes, law_enforcement, case_file_date, case_file_year, event_code, event_code_description
         # FROM red_ids
         # WHERE event_code = "ERPO"
         
         year_approved <- red_ids %>% 
           filter(event_code == "ERPO") %>% 
           distinct(case_number, .keep_all = TRUE)
         
            # Made slight adjustment for the sake of this fact-check:
               # CREATE TABLE "year_approved" AS	
               # SELECT case_number, district_nbr, county_name, respondent, dob, filed_by, notes, law_enforcement, event_file_date, event_file_year, event_code, event_code_description
               # FROM red_ids
               # WHERE event_code = "ERPO"
               # GROUP BY case_number
         
         year_approved %>% write_csv("year_approved.csv", na = "")
         
        # Now let's put them together
         
           temp_approved %>% full_join(year_approved) %>% 
            View()
         
        # So using that -- let's add time to the event_file_date 
          # https://statisticsglobe.com/add-subtract-months-years-from-date-in-r 
           
          library(lubridate) 
           
          temp_approved %>% 
            as.Date("event_file_date")
           
          temp_approved %>% 
            mutate(exp_date = event_file_date + days(14))
          
            # I think this would wrok but I need to change event_file_date into a date
              # https://www.tidyverse.org/blog/2021/03/clock-0-1-0/ 
              # https://community.rstudio.com/t/cast-character-column-to-date-column-mutate-and-as-date/943/3 
            
         temp_approved_2 <- temp_approved %>%
            mutate(event_file_date = dmy(event_file_date))
          
         temp_approved %>%
           mutate(event_file_date = lubridate::dmy(event_file_date)) %>% 
           which(is.na(dates$datetime))
         
           temp_approved %>% 
             select(case_number, )
             days(14)
           
           # none of that shit worked so I exported and set that column as a date when I reimported. Worked. 
             
             temp_approved_2 <- temp_approved
             
             temp_approved_2 <- temp_approved_2 %>% 
               mutate(exp_date = event_file_date + days(14)) %>% 
               View()
           
           # That worked swimmingly. Will do the same for year-long cases too. 
           
              year_approved_2 <- year_approved %>% 
                mutate(exp_date = event_file_date + days(364)) %>% 
                View()
           
           # stupendous!
              # now let's merge em
              
              year_approved_2 %>% full_join(temp_approved_2)
              
              all_approved_2 <- year_approved_2 %>% full_join(temp_approved_2)
              
              # weird join error was because I didn't update the temp dataframe, good 2 go now
          
          # check to see if any these cases were renewed
              
              all_approved_2  %>%  inner_join(renew_approved)
              
              inner_join(all_approved_2, renew_approved, by = "case_number") %>% 
                View()
              
              # worked but need to figure out what line of data to concentrate on. Some repeats for someone who had a temp and yr-long that may have gotten it extended?
                # first step is likely figuring out the latest temp or ERPO to see when it was going to expire. latest one is what we care about the most. 
              
              
              extended_all_approved_2 <- inner_join(all_approved_2, renew_approved, by = "case_number")
              
              extended_all_approved_3 = select(extended_all_approved_2, -district_nbr.y:-case_file_year.y)
                
                # Dropped superfluous columns based off of this:
                  # https://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html 
              
              # Based off of a read of when the expiration dates, it looks like TRPO's were superseded by expiration dates set by ERPO's.
                # All TRPO's ended in 2020 while ERPO's took orders into 2021.     
              
              extended_all_approved_3 %>% 
                as.Date("exp_date", format = "%m/%d/%Y")
              
              extended_all_approved_3 %>% 
                as.Date(exp_date, '%m/%d/%Y') %>% 
                filter(event_code.x == "ERPO") %>%
                ("exp_date" + days(364)) %>% 
                View()
              
              # weird error refusing to convert to dates because fudge me I guess. Going to filter to ERPO and export. Then do the added days. 
                # Then merge and see how many are current
              
              extended_all_approved_3 %>%
              filter(event_code.x == "ERPO") %>%
              View()
              
              
            ERPO_extended_all_approved_3 <- extended_all_approved_3 %>%
                filter(event_code.x == "ERPO")
          
            ERPO_extended_all_approved_3 %>% write_csv("ERPO_extended_all_approved_3.csv", na = "")
             
            ERPO_extended_all_approved_3 %>%    
              mutate(exp_date = event_file_date + days(364)) %>% 
              View()
            
            # was still being weird so I reformatted it in excel and re-imported it
            
            ERPO_extended_all_approved_3_2 %>%    
              mutate(exp_date + days(364)) %>% 
              View()
            
            # worked but weird formatting. maybe I can update old column?
            
            ERPO_extended_all_approved_3_2 %>%    
              as.Date(exp_date) + days(364) %>% 
              View()
            
            ERPO_extended_all_approved_3_2 %>%    
              mutate(exp_date = exp_date + days(364)) %>% 
              View()
            
            # THIS MOTHERFUCKING WORKED!!!!!!!
              # using the same name made it happen. It acted as a paste over what USED to be in the exp_date column. 
            
            ERPO_extended_all_approved_3_2 %>%    
              mutate(exp_date = event_file_date.y + days(364)) %>% 
              View()
            
            # Need to do it off of event_file_date.y because that is the when the renewal dates occured, per the renew_approved dataframe. Will redo
            
            # so now - need to full join now and sort by most recent
            
            
            extended_all_approved_4 <- ERPO_extended_all_approved_3_2 %>%    
              mutate(exp_date = event_file_date.y + days(364))
            
                      
                            # maybe rename if necessary
                            
                            extended_all_approved_4 %>% 
                              rename(district_nbr = cdistrict_nbr.x,
                                    county_name = county_name.x,
                                    respondent = respondent.x,
                                    dob = dob.x,
                                    filed_by = filed_by.x,
                                    notes = notes.x,
                                    supp_position_start_date = position_start_date.y,
                                    supp_position_end_date = position_end_date.y)
                            
                            ## lots of columns but ultimately unnecessary 
                      
            extended_all_approved_5 <- all_approved_2 %>% full_join(extended_all_approved_4) %>% 
              View()
            
                 # Worked great 
            
            extended_all_approved_5 %>% 
              arrange(desc(exp_date)) %>% 
              View()
            
                # roughly 30 cases, need to do a distinct to make sure there aren't any repeat cases 
            
              extended_all_approved_5 %>% 
              distinct(case_number, .keep_all = TRUE) %>% 
              arrange(desc(exp_date)) %>% 
              View()  
              
              distinct_approved_expiration_date <- extended_all_approved_5 %>% 
                distinct(case_number, .keep_all = TRUE) %>% 
                arrange(desc(exp_date))
            
              
              distinct_approved_expiration_date %>% write_csv("distinct_approved_expiration_date.csv", na = "")
              
              # Alt R way to double check:
              
                  extended_all_approved_5 %>% 
                    separate(exp_date, c("exp_year","exp_month", "exp_date"), sep="-") %>% 
                    View()
                  
                    # Inspo: 
                      # http://statseducation.com/Introduction-to-R/modules/tidy%20data/separate/ 
                      # table3 %>%
                        # separate(rate, c("cases", "population"), sep="/")
                 
                sep_extended_all_approved_5 <- extended_all_approved_5 %>% 
                    separate(exp_date, c("exp_year","exp_month", "exp_date"), sep="-")
                 
                sep_extended_all_approved_5 %>% 
                  filter(exp_year == "2022") %>% 
                  View()
                
                ext_2022_all_approved <- sep_extended_all_approved_5 %>% 
                  filter(exp_year == "2022")
                
                ext_2022_all_approved %>% write_csv("ext_2022_all_approved.csv", na = "")
                
                  # Actually -- there are cases along the bottom. Need to do an actual filter, eyeballing it isn't going to cut it
                    # Check for repeats first:
                
                    sep_extended_all_approved_5 %>% 
                      filter(exp_year == "2022") %>% 
                      group_by(case_number) %>% 
                      summarize(count = n()) %>% 
                      arrange(desc(count)) %>% 
                      View()
                    
                  # No repeat case num in 2022. Says 62 rows in 2022. Same as in Excel/SQL merge
                    
                    ext_2022_all_approved %>% 
                      filter((grepl('6', exp_month) %>% 
                      View()
                      
                      ext_2022_all_approved %>% 
                        filter(grepl('06|07|08|09|10|11', exp_month)) %>% 
                        View()
                      
                      # Worked! 30 rows. 
                        # Inspo: https://stackoverflow.com/questions/22850026/filter-rows-which-contain-a-certain-string 
                      
                      ext_2022_all_approved %>% 
                        filter(grepl('06|07|08|09|10|11|12', exp_month)) %>% 
                        View()
                      
                      # Still 30 rows!
                        #  # After the air date (05/25), people are still actively prevented from owning a gun in 30 cases.
                          # So of the 146 cases with an approval, 21% of cases are prevented from gun ownership
                      
                      ext_2022_all_approved %>% 
                        filter(grepl('06|07|08|09|10|11|12', exp_month)) %>% 
                        distinct(case_number, .keep_all = TRUE) %>% 
                        View()
                      
                        # 30 distinct cases. Still good
   
               
               
           
           
           
           
           
           
       