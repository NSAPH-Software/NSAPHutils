## Author: Ben Sabath
## Created: June 2019
## Purpose: Functions for RCE user management

#' Return a vector of users in a group
#'
#' @param group group to list the members of
#'
#' @export
list_users <- function(group = "ci3_confounders") {
  command <- paste("getent group", group)
  users <- system(command, intern = T)
  users <- strsplit(users, ":")[[1]][4]
  users <- strsplit(users, ",")[[1]]

  return(users)
}

#' Get full name from RCE user name
#'
#' @param user RCE username
#'
#' @export
get_name <- function(user) {
  command <- paste("finger", user, "| grep Name")
  name <- system(command, intern = T)
  if(length(name)  == 0) {
    return("")
  }
  name <- strsplit(name, "\t")[[1]]
  name <- name[length(name)]
  name <- strsplit(name, ":")[[1]][2]
  name <- substr(name, 2, nchar(name))

  return(name)
}

#' Get email from RCE user name
#'
#' @param user RCE username
#'
#' @export
get_email <- function(user) {
  command <- paste("finger", user, "| grep Office")
  email <- system(command, intern = T)
  if(length(email)  == 0) {
    return("")
  }
  email <- strsplit(email, ":")[[1]][2]
  email<- substr(email, 2, nchar(email))

  return(email)
}
