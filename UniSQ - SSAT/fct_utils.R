

p_load(data.table, openssl)

# -----------------------------------------------------------------------------

roundUp <- function(x, to = 0.5) {
  to * (x %/% to + as.logical(x %% to))
}

# -----------------------------------------------------------------------------

choose_SignThr <- function(var, SignThr, SignThrN) {
  var <- gsub("^Rel|^Abs|PD$", "", var)

  if (var %in% c("totalNO3Runoff", "totalNO3Leach", "totalDeNit", "totalNit", "totalNPol", "totalNLoss", "totalSediment")) return(SignThrN)
  return(SignThr)
}

# -----------------------------------------------------------------------------

IsPosGood <- function(var) {
  var <- gsub("^Rel|^Abs|PD$", "", var)

  if (var %in% c("Yield", "sucrose_wt", "GPWUI", "NUE", "totalNUptake", "totalSoilOrgN", "totalSoilInorgN", "totalSoilC", "surfaceom_wt", "cropValue"))
    return(T)
  else
    return(F)
}

# -----------------------------------------------------------------------------

Mapping_Cols <- function(Table, Mapping) {
  ColNames <- colnames(Table)
  for (i in 1:nrow(Mapping)) {
    Old <- as.character(Mapping[i, 1])
    New <- as.character(Mapping[i, 2])
    colnames(Table)[ColNames == Old] <- New
  }
  return(Table)
}

# -----------------------------------------------------------------------------

get_filename <- function(factors, values, what=1) {
  for (i in 1:length(factors)) {
    factors[i] <- paste0(factors[i], '=', values[i])
  }
  name <- paste(factors, collapse='/')

  if (what==1) {
    return(paste0(name,'/aggregate-0.csv'))

  } else if (what==2) {
    return(paste0(name,'/aggregate-0.csv'))

  } else if (what==3) {
    return(paste0(name,'/sowyears-0.csv'))

  } else {
    stop()
  }
}




