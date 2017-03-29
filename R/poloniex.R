#These codes include functions for getting data from Poloniex bitcoin exchange.

#'@title Get Data From Poloniex
#'
#'@description
#'With this function and (optionally API credentials) you can get data from Poloniex exchange. It will return a data frame.
#'
#'@param info_command The commmand you wish to convey. (p.s. This function is for information gathering purposes only.)
#'@param parameters Additional parameters to the command.
#'@param auth_file An authorization RDS file. See \code{\link{set_authorization}}.
#'@export
get_from_poloniex <- function(info_command,parameters=list(),auth_file=""){

    #Check if there is authorization credentials
    if(file.exists(auth_file)){
        auth_info<-readRDS(auth_file)
        auth_string<-paste0(names(auth_info),"=",auth_info,collapse="&")
    }else{
        #If not return empty string
        auth_string<-""
    }

    if(length(parameters)>0){
        par_string <- paste0(names(parameters),"=",parameters,collapse="&")
    }else{
        par_string <- ""
    }

    #Prepare http REST request
    request_string <- paste0("https://poloniex.com/public?command=",
                             info_command,
                             ifelse(auth_string=="","",paste0("&",auth_string)),
                             ifelse(par_string=="","",paste0("&",par_string))
                             )

    #Ask the server
    the_answer<-httr::POST(request_string)
    #Legen...wait for it
    httr::stop_for_status(the_answer)
    #Return the answer
    if(the_answer$status_code == 200){
        return(data.table::rbindlist(httr::content(the_answer)))
    }else{
        return(httr::content(the_answer))
    }

}

#'@title Create a Poloniex Credentials File
#'
#'@description
#'Set up an authorization credentials file consisting of the Poloniex Key and Sign. You need to have a Poloniex account. Then access your credentials from https://poloniex.com/support/api/ (do not share your keys in any way or medium). The output is an rds file.
#'
#'@param Key: Your API key.
#'@param Sign: Your API sign.
#'@param file_name: The file name or file path you want to keep your credentials.
#'@export
set_authorization <- function(Key,Sign,file_name){

    saveRDS(list(Key=Key,Sign=Sign),file=paste0(file_name,".rds"))

    the_file_name_print_out <- ifelse(any(grepl("/|\\",file_name)),file_name,paste0(getwd(),"/",file_name))

    print(paste0("Your authentication info is saved at: ",the_file_name_print_out,".rds"))

}
