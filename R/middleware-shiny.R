#' @include globals.R
NULL

reactLogHandler <- function(req) {
  if (! rLog$isLogging()) {
    return(NULL)
  }

  if (identical(req$PATH_INFO, "/reactlog/mark")) {
    sessionToken <- parseQueryString(req$QUERY_STRING)$s
    shinysession <- appsByToken$get(sessionToken)

    # log time
    withReactiveDomain(shinysession, {
      rLog$userMark(getDefaultReactiveDomain())
    })

    return(httpResponse(
      status = 200,
      content = "marked",
      content_type = "text/plain"
    ))

  } else if (identical(req$PATH_INFO, "/reactlog")){

    sessionToken <- parseQueryString(req$QUERY_STRING)$s

    # `renderReactLog` will check/throw if reactlog doesn't exist
    reactlogFile <- renderReactlog(sessionToken)

    return(httpResponse(
      status = 200,
      content = list(
        file = reactlogFile,
        owned = TRUE
      )
    ))

  } else {
    return(NULL)
  }
}

sessionHandler <- function(req) {
  path <- req$PATH_INFO
  if (is.null(path))
    return(NULL)

  matches <- regmatches(path, regexec('^(/session/([0-9a-f]+))(/.*)$', path))
  if (length(matches[[1]]) == 0)
    return(NULL)

  session <- matches[[1]][3]
  subpath <- matches[[1]][4]

  shinysession <- appsByToken$get(session)
  if (is.null(shinysession))
    return(NULL)

  subreq <- as.environment(as.list(req, all.names=TRUE))
  subreq$PATH_INFO <- subpath
  subreq$SCRIPT_NAME <- paste(subreq$SCRIPT_NAME, matches[[1]][2], sep='')

  withReactiveDomain(shinysession, {
    shinysession$handleRequest(subreq)
  })
}

parseCookies <- function(cookie){
  if (is.null(cookie) || nchar(cookie) == 0){
    return(list())
  }
  cookie <- strsplit(cookie, ";", fixed=TRUE)[[1]]
  cookie <- sub("\\s*([\\S*])\\s*", "\\1", cookie, perl=TRUE)

  cookieList <- strsplit(cookie, "=", fixed=TRUE)

  # Handle any non-existent cookie values.
  for (i in 1:length(cookieList)){
    if(length(cookieList[[i]])==1){
      cookieList[[i]][[2]] <- ""
    }
  }

  cookies <- lapply(cookieList, "[[", 2)
  names(cookies) <- sapply(cookieList, "[[", 1)

  return(lapply(cookies, URLdecode))
}

setCookie <- function(req, headers) {
  cookies = parseCookies(req$HTTP_COOKIE)
  sessionID = options("shiny.sessionID")$shiny.sessionID
  if(!is.null(sessionID) && is.null(cookies[[sessionID]]))  {
    cookie = c(sessionID, "=",  uuid::UUIDgenerate(), "; HttpOnly")

    cookieExpires = options("shiny.cookieExpires")$shiny.cookieExpires
    if(!is.null(cookieExpires)) {
      cookie = c(cookie, "; Max-Age=", cookieExpires)
    }

    cookieString = paste(cookie, collapse="")
    headers["Set-Cookie"] = cookieString
  }
  return(headers)
}
