#' Get Documentation Information for a Function
#' 
#' Get documentation information for a function, including package, title, description, examples, and argument names and descriptions.
#'
#' @param functionName name of the function
#'
#' @return a named list of documentation components
#' 
#' @examples
#' a <- getDocData("glm")
#' toJSON(a)
#' @export
getDocData <- function(functionName) {
   target <- gsub(".*/(.+)/help.+$", "\\1", utils:::index.search(functionName, find.package()))
   if(length(target) == 0)
      stop("Function ", functionName, " not found - make sure the package that has this function is loaded.", call. = FALSE)
   docText <- pkgTopic(target, functionName)
   
   classes <- sapply(docText, function(x) attr(x, "Rd_tag"))
   
   title <- docText[[which(grepl("\\\\title", classes))]]
   desc <- docText[[which(grepl("\\\\description", classes))]]
   args <- docText[[which(grepl("\\\\arguments", classes))]]
   
   title <- as.character(title[[1]])
   
   desc <- stripJunkAndPaste(desc)
   
   argClasses <- sapply(args, function(x) attr(x, "Rd_tag"))
   argItems <- args[which(grepl("\\\\item", argClasses))]
   argNames <- sapply(argItems, function(x) {
      tmp <- as.character(x[[1]])
      if(attr(x[[1]][[1]], "Rd_tag") == "\\dots")
         tmp <- "..."
      tmp
   })
   argDescs <- sapply(argItems, function(x) {
      tmp <- stripJunkAndPaste(x[[2]])
      paste(tmp, collapse = "\n")
   })
   
   args <- do.call(rbind, lapply(seq_along(argNames), function(i) {
      tmp <- strsplit(argNames[i], ",")[[1]]
      tmp <- gsub(" +", "", tmp)
      data.frame(name = tmp, desc = argDescs[i], stringsAsFactors = FALSE)
   }))
   
   examples <- docText[[which(grepl("\\\\examples", classes))]]
   examples$sep = ""
   examples <- do.call(paste, examples)
   
   list(
      functionName = functionName,
      package = target,
      title = title,
      desc = desc,
      args = data.frame(name = argNames, desc = argDescs),
      examples = examples
   )   
}

stripJunkAndPaste <- function(x) {
   if(length(x) == 0)
      x <- list("")
   x$sep <- ""
   x <- do.call(paste, x)
   x <- gsub("\n", "", x)
   x <- gsub(" +", " ", x)   
   x <- gsub("^ +", "", x)   
   x
}

# reference:
# http://stackoverflow.com/questions/8379570/get-functions-title-from-documentation
pkgTopic <- function(package, topic, file = NULL) {
   # Find "file" name given topic name/alias
   if (is.null(file)) {
      topics <- pkgTopicsIndex(package)
      topic_page <- subset(topics, alias == topic, select = file)$file
      
      if(length(topic_page) < 1)
         topic_page <- subset(topics, file == topic, select = file)$file
      
      stopifnot(length(topic_page) >= 1)
      file <- topic_page[1]    
   }
   
   rdb_path <- file.path(system.file("help", package = package), package)
   tools:::fetchRdDB(rdb_path, file)
}

pkgTopicsIndex <- function(package) {
   help_path <- system.file("help", package = package)
   
   file_path <- file.path(help_path, "AnIndex")
   if (length(readLines(file_path, n = 1)) < 1) {
      return(NULL)
   }
   
   topics <- read.table(file_path, sep = "\t", stringsAsFactors = FALSE, comment.char = "", quote = "", header = FALSE)
   
   names(topics) <- c("alias", "file") 
   topics[complete.cases(topics), ]
}


