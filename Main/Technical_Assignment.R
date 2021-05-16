#################################################################################
#################################### Goals ######################################
# Date: 2021-May-15
# Author: Arunabha Sarkar
# Goals: Technical Assignment
# File Name: Technical_Assignment
#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################
# Dynamically installing packages if required, then loading all the packages as well
Packages_Required <- c("dplyr","purrr","stringr","mgsub")
if(length(Packages_Required) > 0)
{for(i_Packages_Required in 1:length(Packages_Required))
  {Packages_Required[i_Packages_Required] -> temp_package
  if(!(temp_package %in% installed.packages()))
    {install.packages(temp_package)
    do.call("library", list(temp_package))
    }else{do.call("library", list(temp_package))
    } # End of 'if(!(temp_package %in% installed.packages()))'
  } # End of 'for(i_Packages_Required in 1:length(Packages_Required))'
} # End of 'if(length(Packages_Required) > 0)'
#################################################################################
#################################################################################

#################################################################################
############################## Hyper-parameters #################################
"C:/Users/Administrator/Desktop/Technical Q" -> Project_Folder
"Output" -> Output_Foldername
Output_Foldername %>% paste0(Project_Folder,"/",.,sep="") -> Output_Folderpath
"Main" -> Main_Foldername
Main_Foldername %>% paste0(Project_Folder,"/",.,sep="") -> Main_Folderpath
"Resources" -> Resources_Foldername
Resources_Foldername %>% paste0(Project_Folder,"/",.,sep="") -> Resources_Folderpath
"interviewAssignment.csv" -> Input_Filename
Input_Filename %>% paste0(Resources_Folderpath,"/",.,sep="") -> Input_Folderpath
c("10yrhigh",1,"1m",3,"3m",6,"6m",12,"12m","ytd") -> acceptable_timeframes
Hist_width <- 360
Hist_height <- 640
#################################################################################
#################################################################################

#################################################################################
######################## Establishing Output Database ###########################
tryCatch(expr = 
           {if(dir.exists(Output_Folderpath))
             {Output_Folderpath %>% paste0("Output Database found: ",.,sep="") %>% print
             }else{dir.create(Output_Folderpath)
               Project_Database %>% paste0("Making new Output Database: ",.,sep="") %>% print
               } # End of 'if(dir.exists(Output_Folderpath))'
             },
         error = function(e){
           e %>% paste0("Error in detecting/making Output Database: ",.,sep="") %>% print
           "#000001_Error: " %>% paste0(.,e,". ",sep="") %>% 
             paste0(.,"Couldn't find Project Database: ",Project_Database,
                    "; thus quitting.",sep="") -> temp_error
           temp_error %>% print
           quit()
           },
         warning = function(w){
           w %>% paste0("Warning in detecting Project Database: ",.,sep="") %>% print
           "#000001_Warning: " %>% paste0(.,w,". ",sep="") %>% 
             paste0(.,"Couldn't find Project Database: ",Project_Database,
                    sep="") -> temp_error
           temp_error %>% print
         }) # End of 'tryCatch('
#################################################################################
#################################################################################

#################################################################################
################################ Loading Data ###################################
Input_Folderpath %>% read.csv -> Input_Raw_Data
#################################################################################
#################################################################################

#################################################################################
############################# Pre-processing Data ###############################
Input_Raw_Data %>% mutate_all(~na_if(., 'NULL')) -> XTS_Data
colnames(XTS_Data)[1] <- 'ticker_exchange'
xts(XTS_Data[,-c(2)],order.by = as.Date(XTS_Data[,2],format = "%m/%d/%Y"))->XTS_Data
XTS_Data %>% as.tibble %>% mutate_if(purrr::compose(all,is.numeric),as.numeric)->Tibble_Data
#################################################################################
#################################################################################

#################################################################################
########################### Hist Fut Price Function #############################
# metric, value, and timeframe
Hist_Fut_Price <- function(data=Tibble_Data,
                           temp_metric = 'price/epsntm',
                           temp_value = 0,
                           temp_timeframe=1)
{
  tryCatch(expr = {
    data %>% colnames %>% tolower -> data_colnames
    temp_parameter_colnum <- c()
    Complex_Metric_Bool <- FALSE
    Metric_FOUND_BOOL <- FALSE
    
    if(!temp_metric %in% data_colnames)
    {
      print(paste0("Constructing new metric: '",temp_metric,"'",sep=""))
      
      temp_metric %>% strsplit(split="") %>% .[[1]] -> temp_metric_chars
      temp_operand_index <- c()
      which(!temp_metric_chars %in% c(letters[0:26],LETTERS[0:26])) -> temp_operand_index
      
      if(length(temp_operand_index) > 0){
        Complex_Metric_Bool <- TRUE
        temp_operand <- temp_metric_chars[temp_operand_index]
        temp_first_variable <- paste(temp_metric_chars[0:(temp_operand_index-1)],collapse = "")
        temp_second_variable <- paste(temp_metric_chars[(temp_operand_index+1):length(temp_metric_chars)],collapse = "")
        
        temp_first_variable_FOUND_BOOL <- FALSE
        temp_second_variable_FOUND_BOOL <- FALSE
        
        which(data_colnames == temp_first_variable) -> temp_first_variable_colnum
        if(length(temp_first_variable_colnum)>0){temp_first_variable_FOUND_BOOL<-TRUE}
        
        which(data_colnames == temp_second_variable) -> temp_second_variable_colnum
        if(length(temp_second_variable_colnum)>0){temp_second_variable_FOUND_BOOL<-TRUE}
        
        Both_Variable_FOUND_BOOL <- temp_first_variable_FOUND_BOOL & temp_second_variable_FOUND_BOOL
        if(Both_Variable_FOUND_BOOL)
        {
          data[,temp_first_variable_colnum] %>% unlist %>% as.double -> temp_Data_first
          data[,temp_second_variable_colnum] %>% unlist %>% as.double -> temp_Data_second
          
          length(temp_Data_first) -> temp_Data_first_length
          
          temp_output <- rep(NA,temp_Data_first_length) # Dummy values
          for(i_1 in 1:temp_Data_first_length)
          {
            temp_calculation_text <- paste0(temp_Data_first[i_1],temp_operand,temp_Data_second[i_1],sep="")
            temp_value <- eval(parse(text = temp_calculation_text))
            # Real values
            if(!temp_value %in% c(NA,-Inf,Inf))
            {
              temp_value -> temp_output[i_1]
            } # End of 'if(!temp_value %in% c(NA,-Inf,Inf))'
          } # End of 'for(i_1 in 1:length(temp_Data_first))'
          
          temp_output -> data$New_Parameter
          "New_Parameter" %>% {which(. == colnames(data))} -> temp_parameter_colnum
          
          Metric_FOUND_BOOL <- TRUE
        }else{
          if(!temp_first_variable_FOUND_BOOL)
          {
            "Couldn't find data column for paratemer: '" %>% 
              paste0(.,temp_first_variable,"'",sep="") %>% print
            Metric_FOUND_BOOL <- FALSE
            temp_parameter_colnum <- c()
          } # End of 'if(!temp_first_variable_FOUND_BOOL)'
          if(!temp_second_variable_FOUND_BOOL)
          {
            "Couldn't find data column for paratemer: '" %>% 
              paste0(.,temp_second_variable,"'",sep="") %>% print
            Metric_FOUND_BOOL <- FALSE
            temp_parameter_colnum <- c()
          } # End of 'if(!temp_second_variable_FOUND_BOOL)'
        } # End of 'if(Both_Variable_FOUND_BOOL)'
      }else{
        print(paste0("Can't find/make metric: '",temp_metric,"'",sep=""))
        Metric_FOUND_BOOL <- FALSE
      } # End of 'if(length(temp_operand) > 0){'
    }else{
      print(paste0("Found in data the metric: '",temp_metric,"'",sep=""))
      temp_metric %>% {which(. == data_colnames)} -> temp_parameter_colnum
      Metric_FOUND_BOOL <- FALSE
    } # End of 'if(temp_metric %in% data_colnames)'
    
    if(Metric_FOUND_BOOL)
    {
      data[,temp_parameter_colnum] %>% unlist %>% as.double %>% na.omit -> temp_data_points
      
      # Subset for temp_value
      if(length(temp_data_points))
      {
        temp_data_points %>% {which(. >= temp_value)} %>% 
          temp_data_points[.] -> temp_data_points_value
        
        if(length(temp_data_points_value) > 0)
        {
          temp_timeframe %>% tolower -> temp_timeframe_lower
          # Subset for temp_timeframe
          if(!temp_timeframe %in% acceptable_timeframes)
          {
            "Couldn't understand timeframe: '" %>% 
              paste0(.,temp_timeframe,"', valid options are: ",
                     paste(acceptable_timeframes,collapse = ", "), sep="") %>% print
          }else{
            temp_timeframe_colnum <- c()
            if(temp_timeframe %in% c("10yrhigh"))
            {
              data_colnames %>% {which(. %in% c("px10yrhigh"))} -> temp_timeframe_colnum
            } # End of 'if(temp_timeframe %in% c("10yrhigh"))'
            
            if(temp_timeframe %in% c("1m",1))
            {
              data_colnames %>% {which(. %in% c("pricereturn1m"))} -> temp_timeframe_colnum
            } # End of 'if(temp_timeframe %in% c("1m"))'
            
            if(temp_timeframe %in% c("3m",3))
            {
              data_colnames %>% {which(. %in% c("pricereturn3m"))} -> temp_timeframe_colnum
            } # End of 'if(temp_timeframe %in% c("3m"))'
            
            if(temp_timeframe %in% c("6m",6))
            {
              data_colnames %>% {which(.  %in% c("pricereturn6m"))} -> temp_timeframe_colnum
            } # End of 'if(temp_timeframe %in% c("6m"))'
            
            if(temp_timeframe %in% c("12m",12))
            {
              data_colnames %>% {which(. %in% c("pricereturn12m"))} -> temp_timeframe_colnum
            } # End of 'if(temp_timeframe %in% c("12m"))'
            
            if(temp_timeframe %in% c("ytd"))
            {
              data_colnames %>% {which(. %in% c("pricereturnytd"))} -> temp_timeframe_colnum
            } # End of 'if(temp_timeframe %in% c("ytd"))'
            
            if(length(temp_timeframe_colnum) > 0)
            {
              # Histogram plot output
              Sys.time() -> temp_timestamp
              temp_timestamp %>% as.character %>% mgsub(.,c("-"," ",":"),c("","","")) %>% 
                paste0(.,"_Hist_Plot_metric_",str_replace_all(temp_metric, "[[:punct:]]", ""),
                       "_value_",temp_value,
                       "_timeframe_",temp_timeframe,".png",sep="") -> temp_plotname
              temp_plotname %>% paste0(Output_Folderpath,"/",.,sep="") -> temp_plotpath
              if(!file.exists(temp_plotpath))
              {
                try(dev.off(),silent=TRUE)
                png(temp_plotpath, width = Hist_width, height = Hist_height)
                
                data[data[,temp_parameter_colnum] >= temp_value,] -> value_temp_data
                
                value_temp_data[,temp_timeframe_colnum] %>% unlist %>% as.double %>% 
                  na.omit %>% as.double -> value_temp_data_temp_timeframe
                
                paste0("Returns_Hist","_metric_",temp_metric,"_value_",temp_value,
                       "_timeframe_",temp_timeframe,".png",sep="") -> temp_title
                
                hist(value_temp_data_temp_timeframe,main=temp_title,freq=TRUE) -> plot_data

                dev.off()
                
                # Histogram dataframe output
                temp_metric
                
                temp_timestamp %>% as.character %>% mgsub(.,c("-"," ",":"),c("","","")) %>% 
                  paste0(.,"_Hist_Bins_metric_",str_replace_all(temp_metric, "[[:punct:]]", ""),
                         "_value_",temp_value,
                         "_timeframe_",temp_timeframe,".csv",sep="") -> temp_plot_bin_data
                temp_plot_bin_data %>% paste0(Output_Folderpath,"/",.,sep="") -> temp_plot_bin_data_path
                if(!file.exists(temp_plot_bin_data_path))
                {
                  plot_data %>% .[c(2,3,4)] %>% unlist %>% 
                    matrix(.,nrow=length(plot_data[[2]]),byrow = TRUE) %>% 
                    data.frame(stringsAsFactors = F) %>% round(.,2) %>% 
                    `row.names<-`(paste("Bin_",1:length(plot_data[[2]]),sep="")) %>% 
                    `colnames<-`(c("counts","density","mids")) %>% 
                    write.csv(temp_plot_bin_data_path)
                } # End of 'if(!file.exists(temp_plot_bin_data))'
              }else{
                "Plot already exists, at: " %>% paste0(.,temp_plotpath,sep="") %>% print
              } # End of 'if(!file.exists(temp_plotpath))'
              
            } # End of 'if(length(temp_timeframe_colnum) > 0)'
          } # End of 'if(!temp_timeframe %in% c("10yrhigh","1m","3m","6m","12m"))'
        }else{
          "No data points after metric: '" %>% 
            paste0(., temp_metric, "'", sep = "") %>% 
            paste0(.," and filter for value: '", temp_value,"'",sep="") %>% 
            print
        } # End of 'if(length(temp_data_points_value) > 0)'
        #temp_data_points %>% data.frame %>% `colnames<-`(temp_metric) %>% 
        #ggplot(., aes(x=temp_metric))
        
        #temp_data_points %>% hist(main=temp_metric)
      }else{
        "No data points and thus no hist plot for metric: '" %>% 
          paste0(., temp_metric, "'", sep = "") %>% print
      } # End of 'if(length(temp_data_points))'
    }else{
      "Error in finding/calculating metric: " %>% 
        paste0(.,temp_metric,sep="") %>% print
    } # End of 'if(Metric_FOUND_BOOL)'
  },
  error = function(e){
    e %>% paste0("Error in Hist_Fut_Price function: ",.,sep="") %>% print
    "#000002_Error: " %>% paste0(.,e,". ",sep="") %>% 
      paste0(.,"Couldn't evaluate: ",sep="") -> temp_error
    temp_error %>% print
  },
  warning = function(w){
    w %>% paste0("Warning in Hist_Fut_Price function: ",.,sep="") %>% print
    "#000002_Warning: " %>% paste0(.,w,". ",sep="") %>% 
      paste0(.,"Couldn't evaluate",sep="") -> temp_error
    temp_error %>% print
  }) # End of 'tryCatch('
} # End of Function
#################################################################################
#################################################################################

#################################################################################
################################ Usage Example ##################################

Hist_Fut_Price(data=Tibble_Data,temp_metric='price/epsntm',temp_value=30,temp_timeframe=6)

#################################################################################
#################################################################################

# Made by Arunabha Sarkar