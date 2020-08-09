
dbus.new <- function() {
  observers <- list()
  values <- list()
  self <- list(
    register = function(keys, obs, async, onFulfilled, onRejected) {
      lapply(keys, function(key) {
        if(! key %in% names(observers)) {
          observers[[key]] <<- list()
        }
        observers[[key]][[length(observers[[key]])+1]] <<- list(
          async = async,
          keys = keys,
          observer = obs,
          onFulfilled = onFulfilled,
          onRejected = onRejected
        )
      })
    },
    get = function(key) observers[[key]],
    getValue = function(key) values[[key]],
    setValue = function(key, val) values[[key]] <<- val
  )
  class(self) <- "dbus"
  self
}

dbus.register <- function(self, keys, observer, async=TRUE, onFulfilled=NULL, onRejected=NULL) {
  self$register(keys, observer, async=async, onFulfilled=onFulfilled, onRejected=onRejected)
  vals <- .dbus.getValues(self, keys)
  if(! any(vapply(vals, is.null, TRUE))) {
    .callObserver(observer, vals, async=async, onFulfilled=onFulfilled)
  }
}

dbus.set <- function(self, key, val) {
  self$setValue(key, val)
  .dbus.callObservers(self, key)
}

.dbus.getValues <- function(self, keys) {
  vals <- lapply(keys, function(k) {
    self$getValue(k)
  })
  names(vals) <- keys
  vals
}
  
.dbus.callObservers <- function(self, key) {
  lobs <- self$get(key)
  if(! is.null(lobs)) {
    lapply(lobs, function(l) {
      vals <- .dbus.getValues(self, l$keys)
      .callObserver(l$observer, vals, async=l$async, onFulfilled=l$onFulfilled, onRejected=l$onRejected)
    })
  }
}

.callObserver <- function(obs, vals, async=TRUE, onFulfilled=NULL, onRejected=NULL) {
  if(is.null(onFulfilled) && is.null(onRejected)) {
    if(length(vals) == 1) {
      if(async) {
        future(obs(vals[[1]]))
      } else {
        obs(vals[[1]])
      }
    } else if(async) {
      future(do.call(obs, vals))
    } else {
      do.call(obs, vals)
    }
  } else if(async) {
    .callObserver(obs, vals) %>%
      then(onFulfilled = onFulfilled, onRejected = onRejected)
  } else {
    tryCatch({
      rslt <- .callObserver(obs, vals, async)
      if(! is.null(onFulfilled)) {
        onFulfilled(rslt)
      } else {
        rslt
      }
    },
    error = function(e) {
      if(! is.null(onRejected)) {
        onRejected(e)
      } else {
        stop(e)
      }
    })
  }
}