library("httr")

args = commandArgs()

print(args)

r <- GET("https://de.cyverse.org/terrain/token", authenticate())

x <- content(r)$access_token

print(x)

auth_header <- paste0('Bearer ', x)

result <- POST("https://de.cyverse.org/terrain/secured/fileio/upload", add_headers(.headers = c("Authorization" = auth_header)),  query = list(dest = as.character(args[2])), body = list( file =  upload_file(as.character(args[3]))))

print(result)