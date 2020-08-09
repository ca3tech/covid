library(testthat)
library(promises)
library(future)
source("../R/dbus.R")

setup({
  plan(transparent, earlySignal=FALSE)
})

test_that("register/set for one observer, one dependency", {
  srcdf <- data.frame(a=seq(2), b=letters[seq(2)])
  called <- FALSE
  obs <- function(df) {
    called <<- TRUE
    df
  }
  dbus <- dbus.new()
  dbus.register(dbus, "mytype", obs)
  p <- dbus.set(dbus, "mytype", srcdf)
  expect_false(is.null(p), "set return value")
  promise_all(.list = p) %>%
    then(
      onFulfilled = ~{
        expect_true(all(vapply(., function(df) all.equal(df, srcdf), TRUE)), label="observer return values")
      },
      onRejected = ~{
        expect_equal(as.character(.), "", label="observer error")
      }
    ) %...!% {
      expect_equal(as.character(.), "", label="evaluation error")
    }
  expect_true(called, label="observer was called")
})

test_that("register/set for one observer, one dependency, with onFulfilled", {
  srcdf <- data.frame(a=seq(2), b=letters[seq(2)])
  called <- FALSE
  obs <- function(df) {
    called <<- TRUE
    df
  }
  rcalled <- FALSE
  rslt <- function(df) {
    rcalled <<- TRUE
    df
  }
  dbus <- dbus.new()
  dbus.register(dbus, "mytype", obs, onFulfilled=rslt)
  p <- dbus.set(dbus, "mytype", srcdf)
  expect_false(is.null(p), "set return value")
  promise_all(.list = p) %>%
    then(
      onFulfilled = ~{
        expect_true(all(vapply(., function(df) all.equal(df, srcdf), TRUE)), label="observer return values")
        expect_true(called, label="observer was called")
        expect_true(rcalled, label="onFulfilled was called")
      },
      onRejected = ~{
        expect_equal(as.character(.), "", label="observer error")
      }
    ) %...!% {
      expect_equal(as.character(.), "", label="expectation error")
    }
})

test_that("register/set for one observer, one dependency, with onRejected", {
  srcdf <- data.frame(a=seq(2), b=letters[seq(2)])
  obs <- function(df) {
    stop("test error")
  }
  rcalled <- FALSE
  rslt <- function(e) {
    rcalled <<- TRUE
    stop(e)
  }
  dbus <- dbus.new()
  dbus.register(dbus, "mytype", obs, onRejected=rslt)
  p <- dbus.set(dbus, "mytype", srcdf)
  expect_false(is.null(p), "set return value")
  promise_all(.list = p) %>%
    then(
      onFulfilled = ~{
        expect_true(FALSE, label="promise onFulfilled was called")
      },
      onRejected = ~{
        expect_match(as.character(.), "test error", label="observer error")
        expect_true(rcalled, label="onRejected was called")
      }
    ) %...!% {
      expect_equal(as.character(.), "", label="expectation error")
    }
})

test_that("register/set for two observers, one dependency", {
  srcdf <- data.frame(a=seq(2), b=letters[seq(2)])
  called1 <- FALSE
  called2 <- FALSE
  obs1 <- function(df) {
    called1 <<- TRUE
    df
  }
  obs2 <- function(df) {
    called2 <<- TRUE
    df
  }
  dbus <- dbus.new()
  dbus.register(dbus, "mytype", obs1)
  dbus.register(dbus, "mytype", obs2)
  p <- dbus.set(dbus, "mytype", srcdf)
  expect_false(is.null(p), "set return value")
  promise_all(.list = p) %>%
    then(
      onFulfilled = ~{
        expect_true(all(vapply(., function(df) all.equal(df, srcdf), TRUE)), label="observer return values")
      },
      onRejected = ~{
        expect_equal(as.character(.), "", label="observer error")
      }
    ) %...!% {
      expect_equal(as.character(.), "", label="evaluation error")
    }
  expect_true(called1, label="observer 1 was called")
  expect_true(called2, label="observer 2 was called")
})

test_that("register/set for one observer, one dependency, two values", {
  srcdf1 <- data.frame(a=seq(2), b=letters[seq(2)])
  srcdf2 <- data.frame(a=seq(4, 5), b=letters[seq(4, 5)])
  callcount <- 0
  obs <- function(df) {
    callcount <<- callcount + 1
    df
  }
  dbus <- dbus.new()
  dbus.register(dbus, "mytype", obs)
  p1 <- dbus.set(dbus, "mytype", srcdf1)
  expect_false(is.null(p1), "set1 return value")
  p2 <- dbus.set(dbus, "mytype", srcdf2)
  expect_false(is.null(p2), "set2 return value")
  promise_all(.list = p1) %>%
    then(
      onFulfilled = ~{
        expect_true(all(vapply(., function(df) all.equal(df, srcdf1), TRUE)), label="observer return values 1")
      },
      onRejected = ~{
        expect_equal(as.character(.), "", label="observer error 1")
      }
    ) %...!% {
      expect_equal(as.character(.), "", label="evaluation error 1")
    }
  promise_all(.list = p2) %>%
    then(
      onFulfilled = ~{
        expect_true(all(vapply(., function(df) all.equal(df, srcdf2), TRUE)), label="observer return values 2")
      },
      onRejected = ~{
        expect_equal(as.character(.), "", label="observer error 2")
      }
    ) %...!% {
      expect_equal(as.character(.), "", label="evaluation error 2")
    }
  expect_equal(callcount, 2, label="observer call count")
})

test_that("register/set for one observer, two dependencies", {
  srcdf1 <- data.frame(a=seq(2), b=letters[seq(2)])
  srcdf2 <- data.frame(a=seq(4, 5), b=letters[seq(4, 5)])
  expdf <- rbind(srcdf1, srcdf2)
  callcount <- 0
  obs <- function(mytype1, mytype2) {
    callcount <<- callcount + 1
    df <- NULL
    if(! (is.null(mytype1) || is.null(mytype2))) {
      df <- rbind(mytype1, mytype2)
    }
    df
  }
  dbus <- dbus.new()
  dbus.register(dbus, c("mytype1", "mytype2"), obs)
  p1 <- dbus.set(dbus, "mytype1", srcdf1)
  expect_false(is.null(p1), "set1 return value")
  p2 <- dbus.set(dbus, "mytype2", srcdf2)
  expect_false(is.null(p2), "set2 return value")
  promise_all(.list = p1) %>%
    then(
      onFulfilled = ~{
        expect_true(all(vapply(., is.null, TRUE)), label="observer return values 1")
      },
      onRejected = ~{
        expect_equal(as.character(.), "", label="observer error 1")
      }
    ) %...!% {
      expect_equal(as.character(.), "", label="evaluation error 1")
    }
  promise_all(.list = p2) %>%
    then(
      onFulfilled = ~{
        expect_true(all(vapply(., function(df) all.equal(df, expdf), TRUE)), label="observer return values 2")
      },
      onRejected = ~{
        expect_equal(as.character(.), "", label="observer error 2")
      }
    ) %...!% {
      expect_equal(as.character(.), "", label="evaluation error 2")
    }
  expect_equal(callcount, 2, label="observer call count")
})

test_that("register/set for one observer, two dependencies, two values", {
  srcdf1 <- data.frame(a=seq(2), b=letters[seq(2)])
  srcdf2 <- data.frame(a=seq(4, 5), b=letters[seq(4, 5)])
  srcdf3 <- data.frame(a=seq(7, 8), b=letters[seq(7, 8)])
  expdf2 <- rbind(srcdf1, srcdf2)
  expdf3 <- rbind(srcdf1, srcdf3)
  callcount <- 0
  obs <- function(mytype1, mytype2) {
    callcount <<- callcount + 1
    df <- NULL
    if(! (is.null(mytype1) || is.null(mytype2))) {
      df <- rbind(mytype1, mytype2)
    }
    df
  } 
  dbus <- dbus.new()
  dbus.register(dbus, c("mytype1", "mytype2"), obs)
  p1 <- dbus.set(dbus, "mytype1", srcdf1)
  expect_false(is.null(p1), "set1 return value")
  p2 <- dbus.set(dbus, "mytype2", srcdf2)
  expect_false(is.null(p2), "set2 return value")
  p3 <- dbus.set(dbus, "mytype2", srcdf3)
  expect_false(is.null(p2), "set2 return value")
  promise_all(.list = p1) %>%
    then(
      onFulfilled = ~{
        expect_true(all(vapply(., is.null, TRUE)), label="observer return values 1")
      },
      onRejected = ~{
        expect_equal(as.character(.), "", label="observer error 1")
      }
    ) %...!% {
      expect_equal(as.character(.), "", label="evaluation error 1")
    }
  promise_all(.list = p2) %>%
    then(
      onFulfilled = ~{
        expect_true(all(vapply(., function(df) all.equal(df, expdf2), TRUE)), label="observer return values 2")
      },
      onRejected = ~{
        expect_equal(as.character(.), "", label="observer error 2")
      }
    ) %...!% {
      expect_equal(as.character(.), "", label="evaluation error 2")
    }
  promise_all(.list = p3) %>%
    then(
      onFulfilled = ~{
        expect_true(all(vapply(., function(df) all.equal(df, expdf3), TRUE)), label="observer return values 3")
      },
      onRejected = ~{
        expect_equal(as.character(.), "", label="observer error 3")
      }
    ) %...!% {
      expect_equal(as.character(.), "", label="evaluation error 3")
    }
  expect_equal(callcount, 3, label="observer call count")
})