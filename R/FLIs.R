# Functions to check the type of the object


#' Check if the object is an FLMatrix object
#' @export
is.FLMatrix <- function(object)
{
    if (class(object) == "FLMatrix" |
        class(object) == "FLMatrixBind")
        return (TRUE)
	else return (FALSE)
}

#' Check if the object is an FLVector object
#' @export
is.FLVector <- function(object)
{
	ifelse(class(object)=="FLVector",TRUE,FALSE)
	
}

#' Check if the object is an FLTable object
#' @export
is.FLTable <- function(object)
{
	ifelse(class(object)=="FLTable",TRUE,FALSE)
}
