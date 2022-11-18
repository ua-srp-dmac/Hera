library("RPostgres")
library("readxl")
library("biogeo")
library("gridExtra")
library("stringr")

findIDsite <- function(name){

query <- dbReadTable(con, "artemis_site")

query <- as.data.frame(query)

return(query$id[query$name == as.character(name)])

}

findIDtreat <- function(label){

query <- dbReadTable(con, "artemis_treatment")

query <- as.data.frame(query)

return(query$id[query$label == as.numeric(label)])

}

findIDplot <- function(site,label){

query <- dbReadTable(con, "artemis_plot")

query <- as.data.frame(query)

return(query$id[which(query$label == as.numeric(label) & query$site == as.numeric(findIDsite(site)))])

}

findIDRep <- function(label, site, plabel){

query <- dbReadTable(con, "artemis_replicate")

query <- as.data.frame(query)

return(query$id[which(query$label == as.numeric(label) & query$plot == as.numeric(findIDplot(site,plabel)))])

}


check_plot <- function(site,label_T, label_P, time) {

out <- "New"
siteid <- findIDsite(site)
treatid <- findIDtreat(label_T)
qtime <- 1

query <- dbReadTable(con, "artemis_plot")
query <- as.data.frame(query)
	
x <- query$treatment_id[which(query$site_id == siteid & query$label == label_P)]

if(qtime == time){

if(!identical(as.numeric(x), numeric(0))){

if(x == treatid){
out <- "Duplicate - Plot data already present "
} else{
out <- "Duplicate - Plot data already present but with different treatment "
}

}

} else {

if(!identical(as.numeric(x), numeric(0))){

if(x != treatid){
out <- "Treatment change across time detected "
}

}

}

return(out)

}

check_rep <- function(site, label_P, label_R, time) {

out <- "New"

plotid <- findIDplot(site, label_P)


qtime <- 1

query <- dbReadTable(con, "artemis_replicate")
query <- as.data.frame(query)

x <- query$id[which(query$plot_id == plotid & query$label == label_R)]	

if(qtime == time){

if(!identical(as.numeric(x), numeric(0))){

out <- "Duplicate - Replicate data already present "

}
} 


return(out)

}

check_min <- function(site) {

out <- "New"

siteid <- findIDsite(site)

query <- dbReadTable(con, "artemis_mineralogy")
query <- as.data.frame(query)
x <- query$id[query$site_id == siteid]	


if(!identical(as.numeric(x), numeric(0))){

out <- "Duplicate - Mineralogy data already present."

}



return(out)

}


check_geo <- function(site,label_R, label_P, time) {

out <- "New"
siteid <- findIDsite(site)
repid <- findIDRep(label_R, site, label_P)
if(is.na(label_P)){
repid <- NA
}
qtime <- 1

query <- dbReadTable(con, "artemis_geochemistry")
query <- as.data.frame(query)
	
x <- query$id[which(query$site_id == siteid & query$replicate_id == repid & query$time_label == time)]

if(!identical(as.numeric(x), numeric(0))){

out <- "Duplicate - Geochemistry data already present"

}


return(out)

}


check_extract <- function(site, time) {

out <- "New"
siteid <- findIDsite(site)


query <- dbReadTable(con, "artemis_extraction")
query <- as.data.frame(query)
	
x <- query$id[which(query$site_id == siteid & query$time_label == time)]

if(!identical(as.numeric(x), numeric(0))){

out <- "Duplicate - Extraction data already present"

}


return(out)

}


homedata <- "/var/www/data"

setContentType("text/html")

cat('
<!DOCTYPE html>
<html>
<head>
<title>University of Arizona SRP-DMAC Olympus server</title>
<meta http-equiv="expires"; content="0"; charset=UTF-8">
<link rel="stylesheet" type="text/css" href="/AresC/semantic.min.css">
<style>

div#banner2 { 
z-index: 6;

  overflow: hidden;
  position: fixed;
  bottom: 50px;
  left: 0;	
	background-color: #000000;
	width: 100%; 
	height: 3px;
}


div#footnotebanner { 
z-index: 6;

  overflow: hidden;
  position: fixed;
  bottom: 0;
  left: 0;	
	background-color: #FFFFFF;
	width: 100%; 
	height: 50px;
}

#SRPlogo{ 

  overflow: hidden;
  position: fixed;
bottom: 5px;

margin-left:45%;
height: 40px;

}

#NIEHSlogo{

  overflow: hidden;
  position: fixed;
bottom: 5px;

margin-left:5.4%;
height: 40px;


}


div#footnote3 { 
 position: fixed;
       width: auto;
margin-left:78%;   
       float left;
}



</style>

</head>


<body>

<div id="root">
<div class="App">
	<header class="App-header">
	<div class="ui inverted top fixed menu" style:"width:90%">
		<div class="ui container">
		<a class="header item" href="http://dmac.pharmacy.arizona.edu/Hera/Develop">
		<h1 >Hera</h1>
		</a>
		<a class="item" href="http://dmac.pharmacy.arizona.edu">
		<h4 > Home</h4>
		</a>
		<a href="https://dmac.pharmacy.arizona.edu/Ares/redirect_uri?logout=https://dmac.pharmacy.arizona.edu" class="item">
		<h4>Logout</h4>
		</a>		
			<div class="ui float right dropdown link item">
   		 		<span class="text">Menu</span>
    					<div class="menu">    
      						<a class="item" href="http://dmac.pharmacy.arizona.edu"> Home </a>
      						<a class="item" href="http://dmac.pharmacy.arizona.edu/Hera/Develop"> Geo Up </a>
						      							
					</div>
			</div>
		</div>

	</div>
	</header>

<div class="ui container appBody" style = "padding-top: 140px;">

')

headconv <- read.csv("/var/www/Rfiles/headconversion.csv")

usremail <- SERVER$headers_in$OIDC_CLAIM_email

username <- headconv$Folder[headconv$Email==usremail]

project <- headconv$Project[headconv$Email==usremail]

Aperm <- headconv$Hera[headconv$Email==usremail]

flaguser = 0

if(length(username) != 0){
if(Aperm == 1){
datadirectories <- list.dirs(path = homedata, full.names = FALSE, recursive = FALSE)

for(datadirs in datadirectories){

if(username == datadirs){
flaguser = 1
}

}
}
}


cat('<div id="loader1" class="ui disabled inverted dimmer">
    <div class="ui text loader">Loading</div>
  </div>

')


if(flaguser == 1){

if(is.null(FILES$FirstFile$name)){

cat('
	<div class="ui container" >
	<div class="ui fluid segment">
		<p class="p-t-15" style = "font-size: large;">
		Upload Your Excel Sheet Here</p>
	<form enctype="multipart/form-data" method="POST" action="https://dmac.pharmacy.arizona.edu/Hera/Develop">
	<input type="file" name="FirstFile">
	<input type="submit" name="Upload" value = "Upload">
	</div>
')


}
con <- dbConnect()

flag = 1

if(flag == 1){

dbSendQuery(con, statement = "DELETE FROM artemis_treatment")
dbSendQuery(con, statement = "DELETE FROM artemis_plot")
dbSendQuery(con, statement = "DELETE FROM artemis_replicate")
dbSendQuery(con, statement = "DELETE FROM artemis_mineralogy")
dbSendQuery(con, statement = "DELETE FROM artemis_site")
dbSendQuery(con, statement = "DELETE FROM artemis_geochemistry")
dbSendQuery(con, statement = "DELETE FROM artemis_extraction")

}
		
query1 <- dbReadTable(con, "artemis_treatment")

#print(query1)

query1 <- dbReadTable(con, "artemis_site")

#print(query1)

query1 <- dbReadTable(con, "artemis_plot")

#print(query1)


query1 <- dbReadTable(con, "artemis_replicate")

#print(query1)

query1 <- dbReadTable(con, "artemis_mineralogy")

#print(query1)

query1 <- dbReadTable(con, "artemis_geochemistry")

#print(query1)

query1 <- dbReadTable(con, "artemis_extraction")

#print(query1)

#print(check_plot("Iron King", 8, 30, 3))
#print(check_rep("Iron King", 1, 2, 2))
#print(check_geo("Iron King", NA, NA, 0))
#print(check_extract("Iron King",0))

if(!is.null(FILES$FirstFile$name)){

	randbase = paste0("session",as.integer(rnorm(1,100000,100)),as.integer(rnorm(1,100000,100)),as.integer(rnorm(1,100000,100)))

	analysisdir = paste0("/geoupsession/", randbase)

	dir.create(analysisdir)


	destination <- file.path(analysisdir,FILES$FirstFile$name)
	destination2 <- file.path("/GeoUp/Session",randbase,FILES$FirstFile$name)
	file.copy(FILES$FirstFile$tmp_name,destination,overwrite=TRUE)

	if(file.exists(destination))
	{

		s1_warn = NULL
		s2_warn = NULL
		s3_warn = NULL
		s4_warn = NULL
		s5_warn = NULL
		
		s0_errors = NULL
		s1_errors = NULL
		s2_errors = NULL
		s3_errors = NULL
		s4_errors = NULL
		s5_errors = NULL
	
		first_site <- FALSE

		read_error_coor = 0
		unknown_error_coor = 0
		
		unknown_error_name = 0

		res <- try({
		plotdata <- read_excel(destination, sheet = 4, col_names = FALSE)
		}
		,silent = TRUE)

		if(class(res) == "try-error")
		{
			read_error_coor = 1
		}


		res <- try({
		temp <- as.character(plotdata[1,1])
		temp <- str_split(temp," ")
		n <- length(temp[[1]])

		fsite <- temp[[1]][-c(n,n-1,n-2)]
		
		df_site <- NULL
		df_site$name <-  paste0(fsite,collapse = " ")
		
		df_site <- as.data.frame(df_site)

		if(identical(as.numeric(findIDsite(df_site$name)),numeric(0))){
		
		first_site <- TRUE

		}

		})

		if(class(res) == "try-error")
		{
			unknown_error_name = 1		
		}


		if(unknown_error_name == 1){
		s0_errors  <- rbind(s0_errors ,'Error reading site name.')
		}
				
		########################################################

		read_error_treat = 0
		unknown_error_treat = 0

		res <- try({
		phavedata <- read_excel(destination, sheet = 5, col_names = FALSE)}
		,silent = TRUE)

		if(class(res) == "try-error")
		{
			read_error_treat = 1
		}

		res <- try({
		i = 4
		j=1
		
		df_treat <- NULL

		if(is.na(as.character(phavedata[2,1]))){
		   s5_warn  <- rbind(s5_warn ,'Warning could not find plot label')
		} else if(as.character(phavedata[2,1]) != "plot"){
		   s5_warn  <- rbind(s5_warn ,'Warning wrong plot label')
		}

		if(is.na(as.character(phavedata[2,3]))){
		   s5_warn  <- rbind(s5_warn ,'Warning could not find treatment label')
		} else if(as.character(phavedata[2,3]) != "treatment"){
		   s5_warn  <- rbind(s5_warn ,'Warning wrong treatment label')
		}
		if(is.na(as.character(phavedata[2,4]))){
		   s5_warn  <- rbind(s5_warn ,'Warning could not find pH label')
		} else if(as.character(phavedata[2,4]) != "pH"){
		   s5_warn  <- rbind(s5_warn ,'Warning wrong pH label')
		}

		if(is.na(as.character(phavedata[2,6]))){
		   s5_warn  <- rbind(s5_warn ,'Warning could not find pH Ca label')
		} else if(as.character(phavedata[2,6]) != "pH Ca"){
		   s5_warn  <- rbind(s5_warn ,'Warning wrong pH Ca label')
		}

		if(is.na(as.character(phavedata[2,8]))){
		   s5_warn  <- rbind(s5_warn ,'Warning could not find EC label')
		} else if(as.character(phavedata[2,8]) != "EC"){
		   s5_warn  <- rbind(s5_warn ,'Warning wrong EC label')
		}



		while(!is.na(as.numeric(phavedata[i,1]))){
		
		
		temp <-NULL
		temp$label <- as.numeric(phavedata[i,2])
		temp$description <- as.character(phavedata[i,3])
		
		temp <- as.data.frame(temp)
			
		if(!(temp$label %in% df_treat$label))
		{
			df_treat <- rbind(df_treat,temp)
			j= j+1

		}

		i = i + 1
		
		}
		
		}
		,silent = TRUE)

		if(class(res) == "try-error")
		{
			unknown_error_treat = 1
		}


		#################################################
			
		res <- try({

		i = 4
		
		df_plot <- NULL
			
		while(!is.na(as.numeric(phavedata[i,1]))){
		
		errortemp <- NULL

		temp <- NULL
    		temp$label = as.numeric(phavedata[i,1])
		temp$treatment = as.numeric(phavedata[i,2])

		temp <- as.data.frame(temp)

		errortemp <- check_plot(df_site$name, as.numeric(phavedata[i,2]), as.numeric(phavedata[i,1]), 1)

		df_plot <- rbind(df_plot,temp)
		
		if(errortemp != "New"){
		errortemp <- paste0(errortemp, "for plot ", phavedata[i,1], " at site ", df_site$name, " at time ", 1)
		s5_errors  <- rbind(s5_errors ,errortemp)
		}

		i = i + 1
		
		}

		}
		,silent = TRUE)


		if(read_error_treat == 1){
		s5_errors  <- rbind(s5_errors ,'Error reading "pH EC avg" sheet.')
		} else 	{

		if(unknown_error_treat == 1){
		s5_errors  <- rbind(s5_errors ,'Unknown error parsing treatment data from "pH EC avg" sheet.')
		} 
	        if(class(res) == "try-error")
		{
			s5_errors  <- rbind(s5_errors,'Unknown error parsing treatment data from "pH EC avg" sheet.')
		}

		}
		#############################################################

		res <- try({


		df_site$latitude <- mean(as.numeric(plotdata[3,10]),as.numeric(plotdata[4,10]),as.numeric(plotdata[3,10]),as.numeric(plotdata[6,10]))

		df_site$longitude <- mean(as.numeric(plotdata[3,14]),as.numeric(plotdata[4,14]),as.numeric(plotdata[3,14]),as.numeric(plotdata[6,14]))

		}
		,silent = TRUE)

		if(class(res) == "try-error")
		{
			unknown_error_coor = 1
		}


		##########################################################
		

		res <- try({
		i = 7
		df_rep <- NULL
		
		while(!is.na(as.numeric(plotdata[i,1]))){
		
		errortemp <- NULL
		temp <- NULL
		temp$label <- as.numeric(plotdata[i,3])
		 
		temp$latitude = as.numeric(plotdata[i,10])
   		temp$longitude = as.numeric(plotdata[i,14])

		
		temp <- as.data.frame(temp)

		df_rep <- rbind(df_rep,temp)
	
		errortemp <- check_rep(df_site$name, as.numeric(plotdata[i,1]), as.numeric(plotdata[i,3]), 1)
		
		if(errortemp != "New"){
		errortemp <- paste0(errortemp, "for replicate ", plotdata[i,3] ," at plot ", plotdata[i,1], " at site ", df_site$name, " at time ", 1)
		s4_errors <- rbind(s4_errors,errortemp)
		}


    		i = i + 1
		}

		}
		,silent = TRUE)




		if(read_error_coor == 1){
			s4_errors  <- rbind(s4_errors ,'Error reading "pH EC GPS" sheet.')
			} 

		else {
			if(unknown_error_coor == 1){
			s4_errors  <- rbind(s4_errors ,'Unknown error parsing coordinate data from "pH EC GPS" sheet.')
			} 

			if(class(res) == "try-error")
			{
			s4_errors  <- rbind(s4_errors,'Unknown error parsing replicate data from "pH EC GPS" sheet.')
			}

		    }


		############################################################################
		
		
		read_error_min = 0

		res <- try({
		mineroldata <- read_excel(destination, sheet = 3, col_names = FALSE)
		}
		,silent = TRUE)

		if(class(res) == "try-error")
		{
		read_error_min = 1
		}


		res <- try({
		
		df_min <- NULL
		
		for(i in 2:dim(mineroldata)[2]){

		temp <- NULL

    		temp$collection_date = NA
    		temp$time_label = 0

    		temp$min_depth = as.integer(strsplit(as.character(mineroldata[3,i]),"-")[[1]][1])
   		temp$max_depth = as.integer(strsplit(as.character(mineroldata[3,i]),"-")[[1]][2])

    
    		temp$quartz = as.numeric(mineroldata[4,i])
    		temp$plagioclase = as.numeric(mineroldata[5,i])
   		temp$illite = as.numeric(mineroldata[6,i])
    		temp$chlorite = as.numeric(mineroldata[7,i])
    		temp$kaolinite = as.numeric(mineroldata[8,i])
    		temp$pyrite = as.numeric(mineroldata[9,i])
    		temp$gypsum = as.numeric(mineroldata[10,i])
    		temp$jarosite = as.numeric(mineroldata[11,i])
    		temp$melanternite = as.numeric(mineroldata[12,i])
    		temp$ankerite = as.numeric(mineroldata[13,i])
    		temp$siderite = as.numeric(mineroldata[14,i])
    		temp$amorphous = as.numeric(mineroldata[15,i])

		temp <- as.data.frame(temp)
		
		df_min <- rbind(df_min,temp)

		i=i+1

		}

		errortemp <- check_min(df_site$name)
		
		if(errortemp != "New"){	
			s3_errors <- errortemp
		}


		}
		,silent = TRUE)

		if(read_error_min == 1){
			s3_errors  <- NULL		
			s3_errors  <- rbind(s3_errors ,'Error reading "mineralogy" sheet.')
			} else if(class(res) == "try-error")
		{
		s3_errors  <- NULL	
		s3_errors  <- rbind(s3_errors,'Unknown error parsing mineralogy data from "mineralogy" sheet.')
		}

		#############################################################
	
		read_error_geo = 0
		unknown_error_geo = 0

		res <- try({
		geochem <- read_excel(destination, sheet = 1, col_names = FALSE)
		}
		,silent = TRUE)

		if(class(res) == "try-error")
		{
		read_error_geo = 1
		}

		res <- try({
		df_geo <- NULL
	

		for(i in 1:7){	

		temp <- NULL

		temp$collection_date <- NA
		temp$time_label <- 0

		temp$min_depth <- as.integer(strsplit(as.character(geochem[3,1+i]),"-")[[1]][1])
		temp$max_depth <- as.integer(strsplit(as.character(geochem[3,1+i]),"-")[[1]][2])
		temp$pH <- as.numeric(geochem[5,1+i])
		temp$EC <- as.numeric(geochem[6,1+i])
		temp$color <- as.character(geochem[7,1+i])

		temp$Ag = as.numeric(geochem[9,1+i])
		temp$Al = as.numeric(geochem[10,1+i])
		temp$As = as.numeric(geochem[11,1+i])
		temp$Au = as.numeric(geochem[12,1+i])
		temp$Ba = as.numeric(geochem[13,1+i])
		temp$Be = as.numeric(geochem[14,1+i])
		temp$Bi = as.numeric(geochem[15,1+i])
		temp$Br = as.numeric(geochem[16,1+i])
		temp$Ca = as.numeric(geochem[17,1+i])
		temp$Cd = as.numeric(geochem[18,1+i])
		temp$Ce = as.numeric(geochem[19,1+i])
		temp$Co = as.numeric(geochem[20,1+i])
		temp$Cr = as.numeric(geochem[21,1+i])
		temp$Cs = as.numeric(geochem[22,1+i])
		temp$Cu = as.numeric(geochem[23,1+i])
		temp$Dy = as.numeric(geochem[24,1+i])
		temp$Er = as.numeric(geochem[25,1+i])
		temp$Eu = as.numeric(geochem[26,1+i])
		temp$Fe = as.numeric(geochem[27,1+i])
		temp$Ga = as.numeric(geochem[28,1+i])
		temp$Gd = as.numeric(geochem[29,1+i])
		temp$Ge = as.numeric(geochem[30,1+i])
		temp$Hf = as.numeric(geochem[31,1+i])
		temp$Ho = as.numeric(geochem[32,1+i])
		temp$In = as.numeric(geochem[33,1+i])
		temp$Ir = as.numeric(geochem[34,1+i])
		temp$K = as.numeric(geochem[35,1+i])
		temp$La = as.numeric(geochem[36,1+i])
		temp$Lu = as.numeric(geochem[37,1+i])
		temp$Mg = as.numeric(geochem[38,1+i])
		temp$Mn = as.numeric(geochem[39,1+i])
		temp$Mo = as.numeric(geochem[40,1+i])
		temp$Na = as.numeric(geochem[41,1+i])
		temp$Nb = as.numeric(geochem[42,1+i])
		temp$Nd = as.numeric(geochem[43,1+i])
		temp$Ni = as.numeric(geochem[44,1+i])
		temp$P = as.numeric(geochem[45,1+i])
		temp$Pb = as.numeric(geochem[46,1+i])
		temp$Pr = as.numeric(geochem[47,1+i])
		temp$Rb = as.numeric(geochem[48,1+i])
		temp$S = as.numeric(geochem[49,1+i])
		temp$Sb = as.numeric(geochem[50,1+i])
		temp$Sc = as.numeric(geochem[51,1+i])
		temp$Se = as.numeric(geochem[52,1+i])
		temp$Si = as.numeric(geochem[53,1+i])
		temp$Sm = as.numeric(geochem[54,1+i])
		temp$Sn = as.numeric(geochem[55,1+i])
		temp$Sr = as.numeric(geochem[56,1+i])
		temp$Ta = as.numeric(geochem[57,1+i])
		temp$Tb = as.numeric(geochem[58,1+i])
		temp$Th = as.numeric(geochem[59,1+i])
		temp$Ti = as.numeric(geochem[60,1+i])
		temp$Tl = as.numeric(geochem[61,1+i])
		temp$Tm = as.numeric(geochem[62,1+i])
		temp$U = as.numeric(geochem[63,1+i])
		temp$V = as.numeric(geochem[64,1+i])
		temp$W = as.numeric(geochem[65,1+i])
		temp$Y = as.numeric(geochem[66,1+i])
		temp$Yb = as.numeric(geochem[67,1+i])
		temp$Zn = as.numeric(geochem[68,1+i])
		temp$Zr = as.numeric(geochem[69,1+i])

	   	temp <- as.data.frame(temp)

		df_geo <- rbind(df_geo,temp)

		}

		errortemp <- check_geo(df_site$name, NA, NA, 0)
		if(errortemp != "New"){
			s1_errors <- errortemp
		}


		#####
	
		j=1
   		while(as.character(geochem[2,4+6*j]) == "Field treated"){
		
		errortemp <- NULL

		errortemp <- check_geo(df_site$name, as.integer(strsplit(as.character(geochem[3,4+6*j]),"-")[[1]][3]), as.integer(strsplit(as.character(geochem[3,4+6*j]),"-")[[1]][1]), 1)
		
		if(errortemp != "New"){
			errortemp <- paste0(errortemp, " for plot ", as.integer(strsplit(as.character(geochem[3,4+6*j]),"-")[[1]][1]), " at replicate ", as.integer(strsplit(as.character(geochem[3,4+6*j]),"-")[[1]][3]) ) 
			s1_errors <- rbind(s1_errors,errortemp)
		}
		

		for(i in 1:4){

		temp <- NULL

		temp$collection_date <- NA
		temp$time_label <- 1

		temp$min_depth <- as.integer(strsplit(as.character(geochem[3,4+6*j+i]),"-")[[1]][1])
		temp$max_depth <- as.integer(strsplit(as.character(geochem[3,4+6*j+i]),"-")[[1]][2])
		#temp$replicate_id <- repid;
		temp$pH <- as.numeric(geochem[5,4+6*j+i])
		temp$EC <- as.numeric(geochem[6,4+6*j+i])
		temp$color <- as.character(geochem[7,4+6*j+i])

		temp$Ag = as.numeric(geochem[9,4+6*j+i])
		temp$Al = as.numeric(geochem[10,4+6*j+i])
		temp$As = as.numeric(geochem[11,4+6*j+i])
		temp$Au = as.numeric(geochem[12,4+6*j+i])
		temp$Ba = as.numeric(geochem[13,4+6*j+i])
		temp$Be = as.numeric(geochem[14,4+6*j+i])
		temp$Bi = as.numeric(geochem[15,4+6*j+i])
		temp$Br = as.numeric(geochem[16,4+6*j+i])
		temp$Ca = as.numeric(geochem[17,4+6*j+i])
		temp$Cd = as.numeric(geochem[18,4+6*j+i])
		temp$Ce = as.numeric(geochem[19,4+6*j+i])
		temp$Co = as.numeric(geochem[20,4+6*j+i])
		temp$Cr = as.numeric(geochem[21,4+6*j+i])
		temp$Cs = as.numeric(geochem[22,4+6*j+i])
		temp$Cu = as.numeric(geochem[23,4+6*j+i])
		temp$Dy = as.numeric(geochem[24,4+6*j+i])
		temp$Er = as.numeric(geochem[25,4+6*j+i])
		temp$Eu = as.numeric(geochem[26,4+6*j+i])
		temp$Fe = as.numeric(geochem[27,4+6*j+i])
		temp$Ga = as.numeric(geochem[28,4+6*j+i])
		temp$Gd = as.numeric(geochem[29,4+6*j+i])
		temp$Ge = as.numeric(geochem[30,4+6*j+i])
		temp$Hf = as.numeric(geochem[31,4+6*j+i])
		temp$Ho = as.numeric(geochem[32,4+6*j+i])
		temp$In = as.numeric(geochem[33,4+6*j+i])
		temp$Ir = as.numeric(geochem[34,4+6*j+i])
		temp$K = as.numeric(geochem[35,4+6*j+i])
		temp$La = as.numeric(geochem[36,4+6*j+i])
		temp$Lu = as.numeric(geochem[37,4+6*j+i])
		temp$Mg = as.numeric(geochem[38,4+6*j+i])
		temp$Mn = as.numeric(geochem[39,4+6*j+i])
		temp$Mo = as.numeric(geochem[40,4+6*j+i])
		temp$Na = as.numeric(geochem[41,4+6*j+i])
		temp$Nb = as.numeric(geochem[42,4+6*j+i])
		temp$Nd = as.numeric(geochem[43,4+6*j+i])
		temp$Ni = as.numeric(geochem[44,4+6*j+i])
		temp$P = as.numeric(geochem[45,4+6*j+i])
		temp$Pb = as.numeric(geochem[46,4+6*j+i])
		temp$Pr = as.numeric(geochem[47,4+6*j+i])
		temp$Rb = as.numeric(geochem[48,4+6*j+i])
		temp$S = as.numeric(geochem[49,4+6*j+i])
		temp$Sb = as.numeric(geochem[50,4+6*j+i])
		temp$Sc = as.numeric(geochem[51,4+6*j+i])
		temp$Se = as.numeric(geochem[52,4+6*j+i])
		temp$Si = as.numeric(geochem[53,4+6*j+i])
		temp$Sm = as.numeric(geochem[54,4+6*j+i])
		temp$Sn = as.numeric(geochem[55,4+6*j+i])
		temp$Sr = as.numeric(geochem[56,4+6*j+i])
		temp$Ta = as.numeric(geochem[57,4+6*j+i])
		temp$Tb = as.numeric(geochem[58,4+6*j+i])
		temp$Th = as.numeric(geochem[59,4+6*j+i])
		temp$Ti = as.numeric(geochem[60,4+6*j+i])
		temp$Tl = as.numeric(geochem[61,4+6*j+i])
		temp$Tm = as.numeric(geochem[62,4+6*j+i])
		temp$U = as.numeric(geochem[63,4+6*j+i])
		temp$V = as.numeric(geochem[64,4+6*j+i])
		temp$W = as.numeric(geochem[65,4+6*j+i])
		temp$Y = as.numeric(geochem[66,4+6*j+i])
		temp$Yb = as.numeric(geochem[67,4+6*j+i])
		temp$Zn = as.numeric(geochem[68,4+6*j+i])
		temp$Zr = as.numeric(geochem[69,4+6*j+i])

	   	temp <- as.data.frame(temp)
		df_geo <- rbind(df_geo,temp)

		}

		j=j+1

		}
	
		df_geo <- as.data.frame(df_geo)
		},silent = TRUE)

		geo_replicates = j-1
		
		if(read_error_geo == 1){
			s1_errors  <- rbind(s1_errors ,'Error reading "geochemistry" sheet.')
			} else if(class(res) == "try-error")
		{
			s1_errors  <- rbind(s1_errors,'Unknown error parsing geochemistry data from "geochemistry" sheet.')
		}


		################################################################################
		
		unknown_error_extract = 0
		read_error_extract = 0

		res <- try({
		extract_data <- read_excel(destination, sheet = 2, col_names = FALSE)
		}
		,silent = TRUE)

		if(class(res) == "try-error")
		{
		read_error_extract = 1
		}

		res <- try({
		df_extract <- NULL
		
		for(j in 1:11){

		ele <- extract_data[13*j - 10,1]

		for( i in 1:(dim(extract_data)[2]-1)){

		temp <- NULL
		
    		temp$collection_date = NA
    		temp$time_label = 0

    		temp$element <- as.character(ele)

    		temp$min_depth = as.integer(strsplit(as.character(extract_data[3,i +1]),"-")[[1]][1])
    		temp$max_depth = as.integer(strsplit(as.character(extract_data[3,i +1]),"-")[[1]][2])
 
    		temp$H20 = as.numeric(extract_data[13*j - 9,i +1])
    		temp$H20_sd = as.numeric(extract_data[13*j - 8,i +1])
    		temp$AmNO3 = as.numeric(extract_data[13*j - 7,i +1])
    		temp$AmNO3_sd = as.numeric(extract_data[13*j - 6,i +1])
    		temp$AAc = as.numeric(extract_data[13*j - 5,i +1])
    		temp$AAc_sd = as.numeric(extract_data[13*j - 4,i +1])
   		temp$PO4 = as.numeric(extract_data[13*j - 3,i +1])
    		temp$PO4_sd = as.numeric(extract_data[13*j-2 ,i +1])
    		temp$AAO = as.numeric(extract_data[13*j -1,i +1])
    		temp$AAO_sd = as.numeric(extract_data[13*j ,i +1])
    		temp$CDB = as.numeric(extract_data[13*j +1,i +1])
    		temp$CDB_sd = as.numeric(extract_data[13*j +2,i +1])

		temp <- as.data.frame(temp)
		df_extract <- rbind(df_extract,temp)

		}
		}

		errortemp <- check_extract(df_site$name,0)
		
		if(errortemp != "New"){
		
		s2_errors <- errortemp

		}


		}
		,silent = TRUE)
		
		if(read_error_extract == 1){
			s2_errors  <- rbind(s2_errors ,'Error reading "extractions" sheet.')
			} else if(class(res) == "try-error")
		{
			s2_errors  <- rbind(s2_errors,'Unknown error parsing extractions data from "extractions" sheet.')
		}


		##############################################


		Errors <- sum(length(s5_errors), length(s4_errors), length(s3_errors), length(s2_errors), length(s1_errors), length(s0_errors))
		Warnings <- sum(length(s5_warn), length(s4_warn), length(s3_warn), length(s2_warn), length(s1_warn))

		cat('

		<div class="ui top attached tabular menu">
  		<a class="item active" data-tab="first">Summary')
		
		if(length(s0_errors) > 0){
		
	 	cat('<i class="exclamation triangle icon"></i>')
		}

	
		cat('</a>
  		<a class="item" data-tab="second">Geochemistry ')
		
		 if(length(s1_errors) > 0){
		
	 	cat('<i class="exclamation triangle icon"></i>')
		}
		
		cat('</a>
 		 <a class="item" data-tab="third">Extractions')
		
		 if(length(s2_errors) > 0){
		
	 	cat('<i class="exclamation triangle icon"></i>')
		}

		cat('</a>
		<a class="item" data-tab="fourth">Mineralogy')

		if(length(s3_errors) > 0){
		cat('<i class="exclamation triangle icon"></i>')
		}		

		cat('</a>
		<a class="item" data-tab="fifth">pH EC GPS ')

		if(length(s4_errors) > 0){
		
	 	cat('<i class="exclamation triangle icon"></i>')
		}

		
		cat('</a>
		<a class="item" data-tab="sixth">pH EC avg')
		
		if(length(s5_warn) > 0){
		cat('<i class="exclamation icon"></i>')
		}
		

		if(length(s5_errors) > 0){
		
	 	cat('<i class="exclamation triangle icon"></i>')
		}
		
		cat('
		</a>

		</div>



		<div class="ui bottom attached tab segment active" data-tab="first">
  		
		<p class="p-t-15" style = "font-size: large;">
		Upload Summary</p> 
			
		<p class="p-t-15" style = "font-size: medium;">
		Filename: <a href="')
		cat(destination2)
		cat('" download> ')

		cat(FILES$FirstFile$name)
		cat('
		</a>
		</p>
 		

		<p class="p-t-15" style = "font-size: medium;">
		Site: ')

		if(length(s0_errors) > 0){ 

cat('<i class="exclamation triangle icon"></i> Error Reading Site Name')
		
		} else {

	cat(df_site$name)

		if(first_site == TRUE){

		cat(' (New Site!)')

		}

		}

		cat('</p>
			
		<p class="p-t-15" style = "font-size: medium;">
		Number of Warnings: ')

		cat(Warnings)
		
		cat('</p>

		<p class="p-t-15" style = "font-size: medium;">
		Number of Errors: ')

		cat(Errors)
		
		cat('</p>')

		if(Errors == 0)
		{		
		cat('
		<form enctype="multipart/form-data" method="POST" action="https://dmac.pharmacy.arizona.edu/Hera/Develop">
		<input type="submit" name="UploadFinal" value="Final Submit" onclick="loading1()">
		<input type = "hidden" name ="')
		cat(destination)
		cat('" >')
		} else{
		
		cat(' 
		<div class="ui divider"></div>
		<p class="p-t-15" style = "font-size: medium;">
		Fix Errors To Submit </p>')		

		}
		
		cat('
		</div>
		<div class="ui bottom attached tab segment" data-tab="second">
  		
		<p class="p-t-15" style = "font-size: Large;">
		Geochemistry </p>

		<p class="p-t-15" style = "font-size: medium;">

		Number of Replicates: ')
		
		
		cat(geo_replicates)
		

		cat('</p>

		<p class="p-t-15" style = "font-size: medium;">

		Depths: ')

		res <- try({
				
		geo_dep <- as.data.frame(unique(df_geo[,c("min_depth","max_depth")]))

		for(dep in 1:dim(geo_dep)[1]){

		cat(geo_dep[dep,1])
		cat("-")
		cat(geo_dep[dep,2])
		if(dep != dim(geo_dep)[1]){
		cat(", ")
		}
		}
		}
		,silent = TRUE)

		if(class(res) == "try-error")
		{
		cat('<i class="exclamation triangle icon"></i>')
		}

		cat('</p>

		<div class="ui divider"></div>

		<p class="p-t-15" style = "font-size: medium;">

		Warnings: None</p>
		
		<p class="p-t-15" style = "font-size: medium;">

		Errors: 	
		</p>')

		for( warn in s1_errors){

		cat('<p class="p-t-15" style = "font-size: medium;">
		<i class="exclamation triangle icon"></i>')

		cat(warn)

		cat('</p>')

		}

		cat('

		</div>



		<div class="ui bottom attached tab segment" data-tab="third">
  				<p class="p-t-15" style = "font-size: Large;">
		Extractions </p>


		<p class="p-t-15" style = "font-size: medium;">

		Number of Elements: ')
		
		res <- try({
		cat(length(unique(df_extract$element)))
		}
		,silent = TRUE)

		if(class(res) == "try-error")
		{
		cat('<i class="exclamation triangle icon"></i>')
		}

		
		cat('</p>

		
		<p class="p-t-15" style = "font-size: medium;">

		Depths: ')

		res <- try({
		extr_dep <- as.data.frame(unique(df_extract[,c("min_depth","max_depth")]))

		for(dep in 1:dim(extr_dep)[1]){

		cat(extr_dep[dep,1])
		cat("-")
		cat(extr_dep[dep,2])
		if(dep != dim(extr_dep)[1]){
		cat(", ")
		}
		}
		}
		,silent = TRUE)

		if(class(res) == "try-error")
		{
		cat('<i class="exclamation triangle icon"></i>')
		}


		cat('</p>

		<div class="ui divider"></div>

		<p class="p-t-15" style = "font-size: medium;">

		Warnings: 	
			')
		cat("None")

		cat('</p>
		
		<p class="p-t-15" style = "font-size: medium;">

		Errors: 	
		</p>')

		for( warn in s2_errors){

		cat('<p class="p-t-15" style = "font-size: medium;">
		<i class="exclamation triangle icon"></i>')

		cat(warn)

		cat('</p>')

		}

		cat('

		</div>

		<div class="ui bottom attached tab segment" data-tab="fourth">
  						<p class="p-t-15" style = "font-size: Large;">
		Mineralogy </p>


		<p class="p-t-15" style = "font-size: medium;">

		Number of Minerals: ')
		cat(12)

		cat('</p>

		<p class="p-t-15" style = "font-size: medium;">

		Depths: ')
		
		
		res <- try({
				
		min_dep <- as.data.frame(unique(df_min[,c("min_depth","max_depth")]))

		for(dep in 1:dim(min_dep)[1]){

		cat(min_dep[dep,1])
		cat("-")
		cat(min_dep[dep,2])
		if(dep != dim(min_dep)[1]){
		cat(", ")
		}
		}
		}
		,silent = TRUE)

		if(class(res) == "try-error")
		{
		cat('<i class="exclamation triangle icon"></i>')
		}


		cat('</p>

		<div class="ui divider"></div>

		<p class="p-t-15" style = "font-size: medium;">

		Warnings: 	
			')
		cat("None")

		cat('</p>
		
		<p class="p-t-15" style = "font-size: medium;">

		Errors: 	
		</p>')

		for( warn in s3_errors){

		cat('<p class="p-t-15" style = "font-size: medium;">
		<i class="exclamation triangle icon"></i>')

		cat(warn)

		cat('</p>')

		}

		cat('

		</div>


		<div class="ui bottom attached tab segment" data-tab="fifth">
  		
						<p class="p-t-15" style = "font-size: Large;">
		pH EC GPS </p>


		<p class="p-t-15" style = "font-size: medium;">

		Number of Plots: ')
		cat(24)

		cat('</p>

		<p class="p-t-15" style = "font-size: medium;">

		Number of Treatments: ')
		cat(6)

		cat('</p>

		<p class="p-t-15" style = "font-size: medium;">

		Number of Replicates: ')
		cat(length(unique(df_rep$label)))

		cat('</p>

		<div class="ui divider"></div>
		
		<p class="p-t-15" style = "font-size: medium;">

		Warnings: 	
			')
		cat("None")

		cat('</p>
		
		<p class="p-t-15" style = "font-size: medium;">

		Errors: 	
		</p>')

		for( warn in s4_errors){

		cat('<p class="p-t-15" style = "font-size: medium;">
		<i class="exclamation triangle icon"></i>')

		cat(warn)

		cat('</p>')

		}

		cat('
		</div>





		<div class="ui bottom attached tab segment" data-tab="sixth">
  		<p class="p-t-15" style = "font-size: Large;">
		pH EC avg </p>


		<p class="p-t-15" style = "font-size: medium;">

		Number of Plots: ')
		cat(length(unique(df_plot$label)))

		cat('</p>

		<p class="p-t-15" style = "font-size: medium;">

		Number of Treatments: ')
		cat(length(unique(df_plot$treatment)))

		cat('</p>

		<div class="ui divider"></div>
		
		<p class="p-t-15" style = "font-size: medium;">

		Warnings:
		</p>')

		for( warn in s5_warn){

		cat('<p class="p-t-15" style = "font-size: medium;">
		<i class="exclamation icon"></i>')

		cat(warn)

		cat('</p>')


		}

		cat('
	
		<p class="p-t-15" style = "font-size: medium;">

		Errors: 	
		</p>')

		for( err in s5_errors){

		cat('<p class="p-t-15" style = "font-size: medium;">
		<i class="exclamation triangle icon"></i>')

		cat(err)

		cat('</p>')


		}

		cat('
		</div>


		')
	

	}
		
	
}

spaceflag = 0

if(!is.null(POST[1])){
if(names(POST[1]) == "UploadFinal"){

spaceflag = 1

destination <- names(POST[2])

fname <- strsplit(destination,"/")[[1]][4]

file.copy(destination,paste0("/var/www/data/", username, "/Hera/Data/",fname),overwrite=TRUE)

destcy <- paste0("/iplant/home/shared/srp_dmac/",project, "/", username, "/Hera")

syscom <- paste0("R < /var/www/Rfiles/GeoUp/GeoUpScripts/Upcyverse.R ", destcy, " ", destination, " --no-save")

system(syscom)

############################################################
###########################################################
##############################################################
###########################################################
##############################################################
###########################################################
##############################################################

		plotdata <- read_excel(destination, sheet = 4, col_names = FALSE)
		
	
		temp <- as.character(plotdata[1,1])
		temp <- str_split(temp," ")
		n <- length(temp[[1]])

		fsite <- temp[[1]][-c(n,n-1,n-2)]
		
		df_site <- NULL
		df_site$name <-  paste0(fsite,collapse = " ")

		df_site$latitude <- mean(as.numeric(plotdata[3,10]),as.numeric(plotdata[4,10]),as.numeric(plotdata[3,10]),as.numeric(plotdata[6,10]))
		df_site$longitude <- mean(as.numeric(plotdata[3,14]),as.numeric(plotdata[4,14]),as.numeric(plotdata[3,14]),as.numeric(plotdata[6,14]))

		df_site <- as.data.frame(df_site)

		dbWriteTable(con, "artemis_site", value = df_site, append = TRUE, row.names = FALSE)
		
		########################################################

		phavedata <- read_excel(destination, sheet = 5, col_names = FALSE)

		i = 4
		j=1
		
		df_treat <- NULL

		while(!is.na(as.numeric(phavedata[i,1]))){
		
		
		temp <-NULL
		temp$label <- as.numeric(phavedata[i,2])
		temp$description <- as.character(phavedata[i,3])
		
		temp <- as.data.frame(temp)
			
		if(!(temp$label %in% df_treat$label))
		{
			df_treat <- rbind(df_treat,temp)
			j= j+1

		}

		i = i + 1
		
		}

		dbWriteTable(con, "artemis_treatment", value = df_treat, append = TRUE, row.names = FALSE)
		

		#################################################
		
		i = 4
		
		df_plot <- NULL
		
		while(!is.na(as.numeric(phavedata[i,1]))){

		temp <- NULL
    		temp$site_id = as.numeric(findIDsite(df_site$name))
    		temp$label = as.numeric(phavedata[i,1])
    		temp$treatment_id = as.numeric(findIDtreat(phavedata[i,2]))
		
		temp <- as.data.frame(temp)

		df_plot <- rbind(df_plot,temp)
		
		i = i + 1
		
		}

		
		dbWriteTable(con, "artemis_plot", value = df_plot, append = TRUE, row.names = FALSE)
	

		#############################################################


		i = 7
		df_rep <- NULL

		while(!is.na(as.numeric(plotdata[i,1]))){

		temp <- NULL
		temp$plot_id <- as.numeric(findIDplot(df_site$name,plotdata[i,1]))
		temp$label <- as.numeric(plotdata[i,3])		

		temp$latitude = as.numeric(plotdata[i,10])
   		temp$longitude = as.numeric(plotdata[i,14])

		temp <- as.data.frame(temp)

		df_rep <- rbind(df_rep,temp)

		i = i + 1
		
		}

		dbWriteTable(con, "artemis_replicate", value = df_rep, append = TRUE, row.names = FALSE)


		############################################################################
		
		mineroldata <- read_excel(destination, sheet = 3, col_names = FALSE)
			

		i = 2
		df_min <- NULL

		for(i in 2:dim(mineroldata)[2]){

		temp <- NULL

		temp$site_id = as.numeric(findIDsite(df_site$name))
    		temp$collection_date = NA
    		temp$time_label = 0

    		temp$min_depth = as.integer(strsplit(as.character(mineroldata[3,i]),"-")[[1]][1])
   		temp$max_depth = as.integer(strsplit(as.character(mineroldata[3,i]),"-")[[1]][2])

    
    		temp$quartz = as.numeric(mineroldata[4,i])
    		temp$plagioclase = as.numeric(mineroldata[5,i])
   		temp$illite = as.numeric(mineroldata[6,i])
    		temp$chlorite = as.numeric(mineroldata[7,i])
    		temp$kaolinite = as.numeric(mineroldata[8,i])
    		temp$pyrite = as.numeric(mineroldata[9,i])
    		temp$gypsum = as.numeric(mineroldata[10,i])
    		temp$jarosite = as.numeric(mineroldata[11,i])
    		temp$melanternite = as.numeric(mineroldata[12,i])
    		temp$ankerite = as.numeric(mineroldata[13,i])
    		temp$siderite = as.numeric(mineroldata[14,i])
    		temp$amorphous = as.numeric(mineroldata[15,i])

		temp <- as.data.frame(temp)
		
		df_min <- rbind(df_min,temp)

		i=i+1

		}

		dbWriteTable(con, "artemis_mineralogy", value = df_min, append = TRUE, row.names = FALSE)

		#############################################################
	
		geochem <- read_excel(destination, sheet = 1, col_names = FALSE)
		
		df_geo <- NULL
		
		for(i in 1:7){	

		temp <- NULL
		temp$site_id <- as.numeric(findIDsite(df_site$name))
		temp$replicate_id <- NA
		temp$collection_date <- NA
		temp$time_label <- 0

		temp$min_depth <- as.integer(strsplit(as.character(geochem[3,1+i]),"-")[[1]][1])
		temp$max_depth <- as.integer(strsplit(as.character(geochem[3,1+i]),"-")[[1]][2])
		temp$pH <- as.numeric(geochem[5,1+i])
		temp$EC <- as.numeric(geochem[6,1+i])
		temp$color <- as.character(geochem[7,1+i])

		temp$Ag = as.numeric(geochem[9,1+i])
		temp$Al = as.numeric(geochem[10,1+i])
		temp$As = as.numeric(geochem[11,1+i])
		temp$Au = as.numeric(geochem[12,1+i])
		temp$Ba = as.numeric(geochem[13,1+i])
		temp$Be = as.numeric(geochem[14,1+i])
		temp$Bi = as.numeric(geochem[15,1+i])
		temp$Br = as.numeric(geochem[16,1+i])
		temp$Ca = as.numeric(geochem[17,1+i])
		temp$Cd = as.numeric(geochem[18,1+i])
		temp$Ce = as.numeric(geochem[19,1+i])
		temp$Co = as.numeric(geochem[20,1+i])
		temp$Cr = as.numeric(geochem[21,1+i])
		temp$Cs = as.numeric(geochem[22,1+i])
		temp$Cu = as.numeric(geochem[23,1+i])
		temp$Dy = as.numeric(geochem[24,1+i])
		temp$Er = as.numeric(geochem[25,1+i])
		temp$Eu = as.numeric(geochem[26,1+i])
		temp$Fe = as.numeric(geochem[27,1+i])
		temp$Ga = as.numeric(geochem[28,1+i])
		temp$Gd = as.numeric(geochem[29,1+i])
		temp$Ge = as.numeric(geochem[30,1+i])
		temp$Hf = as.numeric(geochem[31,1+i])
		temp$Ho = as.numeric(geochem[32,1+i])
		temp$In = as.numeric(geochem[33,1+i])
		temp$Ir = as.numeric(geochem[34,1+i])
		temp$K = as.numeric(geochem[35,1+i])
		temp$La = as.numeric(geochem[36,1+i])
		temp$Lu = as.numeric(geochem[37,1+i])
		temp$Mg = as.numeric(geochem[38,1+i])
		temp$Mn = as.numeric(geochem[39,1+i])
		temp$Mo = as.numeric(geochem[40,1+i])
		temp$Na = as.numeric(geochem[41,1+i])
		temp$Nb = as.numeric(geochem[42,1+i])
		temp$Nd = as.numeric(geochem[43,1+i])
		temp$Ni = as.numeric(geochem[44,1+i])
		temp$P = as.numeric(geochem[45,1+i])
		temp$Pb = as.numeric(geochem[46,1+i])
		temp$Pr = as.numeric(geochem[47,1+i])
		temp$Rb = as.numeric(geochem[48,1+i])
		temp$S = as.numeric(geochem[49,1+i])
		temp$Sb = as.numeric(geochem[50,1+i])
		temp$Sc = as.numeric(geochem[51,1+i])
		temp$Se = as.numeric(geochem[52,1+i])
		temp$Si = as.numeric(geochem[53,1+i])
		temp$Sm = as.numeric(geochem[54,1+i])
		temp$Sn = as.numeric(geochem[55,1+i])
		temp$Sr = as.numeric(geochem[56,1+i])
		temp$Ta = as.numeric(geochem[57,1+i])
		temp$Tb = as.numeric(geochem[58,1+i])
		temp$Th = as.numeric(geochem[59,1+i])
		temp$Ti = as.numeric(geochem[60,1+i])
		temp$Tl = as.numeric(geochem[61,1+i])
		temp$Tm = as.numeric(geochem[62,1+i])
		temp$U = as.numeric(geochem[63,1+i])
		temp$V = as.numeric(geochem[64,1+i])
		temp$W = as.numeric(geochem[65,1+i])
		temp$Y = as.numeric(geochem[66,1+i])
		temp$Yb = as.numeric(geochem[67,1+i])
		temp$Zn = as.numeric(geochem[68,1+i])
		temp$Zr = as.numeric(geochem[69,1+i])

	   	temp <- as.data.frame(temp)

		df_geo <- rbind(df_geo,temp)

		}

		#####
	

   		for(j in 1:4){
		
		repid  <- as.numeric(findIDRep(as.integer(strsplit(as.character(geochem[3,4+6*j]),"-")[[1]][3]),df_site$name, as.integer(strsplit(as.character(geochem[3,4+6*j]),"-")[[1]][1])))

		for(i in 1:4){

		temp <- NULL

		temp$site_id <- as.numeric(findIDsite(df_site$name))
		temp$replicate_id <- repid
		temp$collection_date <- NA
		temp$time_label <- 1

		temp$min_depth <- as.integer(strsplit(as.character(geochem[3,4+6*j+i]),"-")[[1]][1])
		temp$max_depth <- as.integer(strsplit(as.character(geochem[3,4+6*j+i]),"-")[[1]][2])
		temp$pH <- as.numeric(geochem[5,4+6*j+i])
		temp$EC <- as.numeric(geochem[6,4+6*j+i])
		temp$color <- as.character(geochem[7,4+6*j+i])

		temp$Ag = as.numeric(geochem[9,4+6*j+i])
		temp$Al = as.numeric(geochem[10,4+6*j+i])
		temp$As = as.numeric(geochem[11,4+6*j+i])
		temp$Au = as.numeric(geochem[12,4+6*j+i])
		temp$Ba = as.numeric(geochem[13,4+6*j+i])
		temp$Be = as.numeric(geochem[14,4+6*j+i])
		temp$Bi = as.numeric(geochem[15,4+6*j+i])
		temp$Br = as.numeric(geochem[16,4+6*j+i])
		temp$Ca = as.numeric(geochem[17,4+6*j+i])
		temp$Cd = as.numeric(geochem[18,4+6*j+i])
		temp$Ce = as.numeric(geochem[19,4+6*j+i])
		temp$Co = as.numeric(geochem[20,4+6*j+i])
		temp$Cr = as.numeric(geochem[21,4+6*j+i])
		temp$Cs = as.numeric(geochem[22,4+6*j+i])
		temp$Cu = as.numeric(geochem[23,4+6*j+i])
		temp$Dy = as.numeric(geochem[24,4+6*j+i])
		temp$Er = as.numeric(geochem[25,4+6*j+i])
		temp$Eu = as.numeric(geochem[26,4+6*j+i])
		temp$Fe = as.numeric(geochem[27,4+6*j+i])
		temp$Ga = as.numeric(geochem[28,4+6*j+i])
		temp$Gd = as.numeric(geochem[29,4+6*j+i])
		temp$Ge = as.numeric(geochem[30,4+6*j+i])
		temp$Hf = as.numeric(geochem[31,4+6*j+i])
		temp$Ho = as.numeric(geochem[32,4+6*j+i])
		temp$In = as.numeric(geochem[33,4+6*j+i])
		temp$Ir = as.numeric(geochem[34,4+6*j+i])
		temp$K = as.numeric(geochem[35,4+6*j+i])
		temp$La = as.numeric(geochem[36,4+6*j+i])
		temp$Lu = as.numeric(geochem[37,4+6*j+i])
		temp$Mg = as.numeric(geochem[38,4+6*j+i])
		temp$Mn = as.numeric(geochem[39,4+6*j+i])
		temp$Mo = as.numeric(geochem[40,4+6*j+i])
		temp$Na = as.numeric(geochem[41,4+6*j+i])
		temp$Nb = as.numeric(geochem[42,4+6*j+i])
		temp$Nd = as.numeric(geochem[43,4+6*j+i])
		temp$Ni = as.numeric(geochem[44,4+6*j+i])
		temp$P = as.numeric(geochem[45,4+6*j+i])
		temp$Pb = as.numeric(geochem[46,4+6*j+i])
		temp$Pr = as.numeric(geochem[47,4+6*j+i])
		temp$Rb = as.numeric(geochem[48,4+6*j+i])
		temp$S = as.numeric(geochem[49,4+6*j+i])
		temp$Sb = as.numeric(geochem[50,4+6*j+i])
		temp$Sc = as.numeric(geochem[51,4+6*j+i])
		temp$Se = as.numeric(geochem[52,4+6*j+i])
		temp$Si = as.numeric(geochem[53,4+6*j+i])
		temp$Sm = as.numeric(geochem[54,4+6*j+i])
		temp$Sn = as.numeric(geochem[55,4+6*j+i])
		temp$Sr = as.numeric(geochem[56,4+6*j+i])
		temp$Ta = as.numeric(geochem[57,4+6*j+i])
		temp$Tb = as.numeric(geochem[58,4+6*j+i])
		temp$Th = as.numeric(geochem[59,4+6*j+i])
		temp$Ti = as.numeric(geochem[60,4+6*j+i])
		temp$Tl = as.numeric(geochem[61,4+6*j+i])
		temp$Tm = as.numeric(geochem[62,4+6*j+i])
		temp$U = as.numeric(geochem[63,4+6*j+i])
		temp$V = as.numeric(geochem[64,4+6*j+i])
		temp$W = as.numeric(geochem[65,4+6*j+i])
		temp$Y = as.numeric(geochem[66,4+6*j+i])
		temp$Yb = as.numeric(geochem[67,4+6*j+i])
		temp$Zn = as.numeric(geochem[68,4+6*j+i])
		temp$Zr = as.numeric(geochem[69,4+6*j+i])

	   	temp <- as.data.frame(temp)
		df_geo <- rbind(df_geo,temp)

		}

		}

   		dbWriteTable(con, "artemis_geochemistry", value = df_geo, append = TRUE, row.names = FALSE)

		################################################################################
		
		extract_data <- read_excel(destination, sheet = 2, col_names = FALSE)
		
		df_extract <- NULL
		
		for(j in 1:11){

		ele <- extract_data[13*j - 10,1]

		for( i in 1:(dim(extract_data)[2]-1)){

		temp <- NULL
		
		temp$site_id = as.numeric(findIDsite(df_site$name))
    		temp$collection_date = NA
    		temp$time_label = 0

    		temp$element <- as.character(ele)

    		temp$min_depth = as.integer(strsplit(as.character(extract_data[3,i +1]),"-")[[1]][1])
    		temp$max_depth = as.integer(strsplit(as.character(extract_data[3,i +1]),"-")[[1]][2])
 
    		temp$H20 = as.numeric(extract_data[13*j - 9,i +1])
    		temp$H20_sd = as.numeric(extract_data[13*j - 8,i +1])
    		temp$AmNO3 = as.numeric(extract_data[13*j - 7,i +1])
    		temp$AmNO3_sd = as.numeric(extract_data[13*j - 6,i +1])
    		temp$AAc = as.numeric(extract_data[13*j - 5,i +1])
    		temp$AAc_sd = as.numeric(extract_data[13*j - 4,i +1])
   		temp$PO4 = as.numeric(extract_data[13*j - 3,i +1])
    		temp$PO4_sd = as.numeric(extract_data[13*j-2 ,i +1])
    		temp$AAO = as.numeric(extract_data[13*j -1,i +1])
    		temp$AAO_sd = as.numeric(extract_data[13*j ,i +1])
    		temp$CDB = as.numeric(extract_data[13*j +1,i +1])
    		temp$CDB_sd = as.numeric(extract_data[13*j +2,i +1])

		temp <- as.data.frame(temp)
		df_extract <- rbind(df_extract,temp)

		}
		}
		
   		dbWriteTable(con, "artemis_extraction", value = df_extract, append = TRUE, row.names = FALSE)

		##############################################
		##############################################
		##############################################
		##############################################


cat('

<p class="p-t-15" style = "font-size: large;">
		Upload Successful!</p>
')

}
}


if(spaceflag == 0){
			cat('<br>')

}


cat('

			<br>
			<p class="p-t-15" style = "font-size: large;">
			Recent Uploads </p>
			<div class="ui segment">	
			<div class="table-container" style="padding-top: 20px; height:350px;overflow-y: scroll;">
			<table class="ui very basic table">
				<thead class="">
				<tr class="">
				<th class="">
				Name</th>
				<th class="">
				Size</th>
				<th class="">
				Date Uploaded</th>
				<th>Download</th>
				<th></th>
				<th></th>
				<th></th>
				</tr>
				
				</thead>

				<tbody class="">')


fpath <- paste0("/var/www/data/", username, "/Hera/Data")
fpath2 <- paste0("/Hera/temp/", username, "/Hera/Data/")

details = file.info(list.files(path = fpath, full.names = TRUE))

details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ]

filenames = sapply( rownames(details), function(x) { strsplit(x,"/")[[1]][8]})

if (length(filenames) != 0){				
	for(i in 1:length(filenames)){
	cat('
				<tr class="" >
				<td class="">')
	cat(filenames[i])
	cat('</td>
				<td class="">') 
	cat(paste0(round(file.size(file.path(fpath, filenames[i]))/1000))," KB")
	cat('</td>
				<td class="">') 
	cat(as.character(file.mtime(file.path(fpath, filenames[i]))))
	cat('</td>
	<td><a href="')
	cat(paste0(fpath2, filenames[i]))
	cat('" download> File </a></td>
	<td></td>
	<td></td>
	<td></td>			
				</tr>')
	}
}

				cat('</tbody>
				</table>
		</div>
</div>
')

} else
{

cat('

<p class="p-t-15" style = "font-size: large; ">
		There was a problem finding your account. Please contact us for assistance.  </p>

')

}

filenames2 = dir(path = "/geoupsession/")

if(length(filenames2) > 0){

for (i in 1:length(filenames2)){

if(difftime(Sys.time(), file.info(file.path("/geoupsession",filenames2[i]))$ctime, units="hours") > 24){


unlink(file.path("/apponefiles",filenames2[i]), recursive = TRUE)

}
}

}




cat('
</div>

<script>
  function loading1() {
  
	document.getElementById("loader1").className = "ui active inverted dimmer";
  
}
</script>


<script type = "text/javascript" src="/AresC/jquery.min.js"></script>
<script type = "text/javascript" src="/AresC/semantic.min.js"></script>

<script type = "text/javascript">
$(".menu .item")
  .tab()
;

</script>

</body>
</html>
')


