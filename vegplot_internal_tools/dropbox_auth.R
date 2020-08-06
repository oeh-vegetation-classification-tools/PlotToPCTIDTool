library(rdrop2)

# This will launch your browser and request access to your Dropbox account. 
# You will be prompted to log in if you aren't already logged in.
# Once completed, close your browser window and return to R to complete authentication. 

# The authentication details are saved as a token, which is loaded by the app to make the drops

token <- drop_auth()
saveRDS(token, file = "../vegplot/www/drop_token.rds")