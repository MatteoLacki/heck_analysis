setwd('~/pride_csv/csv_files/')
n <- 156
all_files<- list.files()

parsed_file <- all_files[1]

library(readr)

data <- read_csv(parsed_file)
data_c <- na.omit(data_frame(seq= data$pep_seq,intensity= data$c_intt))
data_z <- na.omit(data_frame(seq = data$pep_seq,intensity=  data$z_intt))

data_c$fragment_name <- paste("c",nchar(data_c$seq), sep="")
data_c$file_name <- parsed_file


data_z$fragment_name <- paste("z",n-nchar(data_z$seq), sep="")
data_z$file_name <- parsed_file

data_z<-data_z[,-1]
data_c<-data_c[,-1]

full_data<-rbind(data_c,data_z)


for(parsed_file in all_files[-1]){
  if (!(parsed_file %in% c("20141202_AMB_Bora_10x_40MeOH_1FA_OT_120k_10uscans_920_EThcD_8ms_15CE_19precZ_DC_L2_XlinkX.csv"))){
    
  
  data <- read_csv(parsed_file)
  data_c <- na.omit(data_frame(seq= data$pep_seq,intensity= data$c_intt))
  data_z <- na.omit(data_frame(seq = data$pep_seq,intensity=  data$z_intt))
  
  if(nrow(data_c)>0){ 
    data_c$fragment_name <- paste("c",nchar(data_c$seq), sep="")
    data_c$file_name <- parsed_file
    data_c<-data_c[,-1]
    
    full_data<-rbind(full_data, data_c)
  }
  
  if(nrow(data_z)>0){
    
    data_z$fragment_name <- paste("z",n-nchar(data_z$seq), sep="")
    data_z$file_name <- parsed_file
  
    data_z<-data_z[,-1]
    full_data<-rbind(full_data, data_z)
  }
  
  
  }  
}

save(full_data, file = "../parsed_csv.Rdata")  
