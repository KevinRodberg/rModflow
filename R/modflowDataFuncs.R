#source ("//ad.sfwmd.gov/dfsroot/data/wsd/SUP/devel/source/R/ResuableFunctions/tclFuncs.R")

#' @title Defines SFWMD Modflow Model characteristics
#' @description \code{defineMFmodel} defines several SFWMD Modflow Models characteristics
#' @return MFmodel.Params Data.frame providing model characteristics
#' @export
#' @examples
#'      MFmodel.Params <- defineMFmodel()
#'      model <- chooseModel()
#'      M <- as.data.frame(MFmodel.Params[model,])

defineMFmodel <- function() {
  MFmodel = c('ECFTX', 'NPALM', 'LWCSIM','ECFM')
  res = c(1250, 704, 1000, 2400)
  xmin = c(24352.000, 680961.000, 218436.000, 565465.000)
  ymin = c(983097.000, 840454.000, 441788.000, -44448.000)
  nlays = c(11,3,9,7)
  nrows = c(603, 292, 553, 552)
  ncols = c(740, 408, 512, 236)
  startYr = c(1999, 1965, 1999, 1989)
  freq = c('Month', 'Month', 'Month','Month')
  nsp = c(192, 14975, 192, 288)
  mpath =c("//whqhpc01p/hpcc_shared/dbandara/CFWI/ECFTX/Model/Transient/*.*",
           "//whqhpc01p/hpcc_shared/jgidding/LECSR/LOX18/*.*",
           "//ad.sfwmd.gov/dfsroot/data/wsd/MOD/LWCSASIAS/model/*.*",
           "//ad.sfwmd.gov/dfsroot/data/wsd/MOD/ECFM/MB/*.*")
  MFmodel.Params <-
    data.frame(MFmodel, res, xmin, ymin, nlays,nrows, ncols, nsp, startYr,freq,mpath)
  rownames(MFmodel.Params) <- MFmodel.Params$MFmodel
  return(MFmodel.Params)
}

#' @title Exit Function
#' @description \code{exit} Provides Function to exit a little more nicely
#' @return \code{msg} Displays message on exit
#' @export
#' @examples
#'      exit("abnormal termination")

exit <- function(msg){
  cat(paste0("*** ERROR ***: ", msg))
  closeAllConnections()
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
  options(warn=0)

}

#' @title Choose Modflow Model
#' @description \code{chooseModel} Provides Radio button choices of available Modflow models.
#'      The implementation is capable of a variable number of radioButtons
#'      depending on length of MFModels vector via a loop
#' @return \code{model} name from vector of available models in MFModels
#' @import tcltk2
#' @export
#' @examples
#'      MFmodel.Params <- defineMFmodel()
#'      model <- chooseModel()
#'      M <- as.data.frame(MFmodel.Params[model,])

chooseModel <- function() {
  library(tcltk2)
  fontHeading <<- tkfont.create(family = "Arial",size = 24,weight = "bold",slant = "italic")
  done <- tclVar(0)

  #===============================================
  #  define ok and Cancel functions for tcl buttons
  #  and stadardize some tcl vars
  #===============================================
  fnOK <- function() {
    tclvalue(done) <- 1}

  fnCncl <- function() {
    tclvalue(done) <- 2}

  win1 <- tktoplevel()
  tkraise(win1)

  MFmodels = as.vector( MFmodel.Params$MFmodel)
#  MFmodels = c('ECFTX', 'NPALM', 'LWCSIM')
  numIDs = length(MFmodels)

  rBtnVal = tclVar(MFmodels[2])

  lbl.Select <-tk2label(win1, text = "Select Model", font = fontHeading)
  lbl.Blnk <- tk2label(win1, text = " ", font = fontHeading)
  tkgrid(lbl.Select,columnspan = 4,padx = 100,pady = 5)

  # Radio buttons can be created and packed or added to tkgrid on the fly:
  for (num in 1:numIDs) {
    btn <- tk2radiobutton(win1)
    tkconfigure(btn, variable = rBtnVal, value = MFmodels[num])
    tkgrid(tk2label(win1, text = MFmodels[num]),btn,padx = 10,pady =  5)
  }

  btn.OK <-tk2button(win1,text = "OK",width = -6,command = fnOK)
  btn.Cncl <-tk2button(win1,text = "Cancel",width = -6,command = fnCncl)
  tkgrid(lbl.Blnk,btn.OK,btn.Cncl,padx = c(5, 5),pady = c(5, 5))
  tkbind(win1, "<Return>", fnOK)
  tkraise(win1)
  tkwait.variable(done)
  tkdestroy(win1)

  if (tclvalue(done) != 1) {
    exit("User canceled Model Selection")
  }
  return((tclvalue(rBtnVal)))
}

#' @title Read Model Mesh Points
#' @description \code{defineMFmodel} Reads arcGIS shape file of model mesh
#'    -Converts mesh polygons to points referencing the center of the model cells
#'    -Changes spatial reference if necessary
#'    -Converts point coordinates into a data frame
#' @return ModelGridCoords Data.frame providing model row, column and coordinates
#' @export
#' @examples
#'      ModelGridCoords<-readgridPoints(Modelgrd.Path,Model.Shape)
#'
#'      ---Using futures:
#'      g <- future({readgridPoints(Modelgrd.Path,Model.Shape)})
#'      ModelGridCoords <-future::value(g)

readgridPoints<- function(Modelgrd.Path,Model.Shape){
  require(future)
  Modelgrd %<-% rgdal::readOGR(Modelgrd.Path,Model.Shape)
  modCol <- grep("^col$",colnames(Modelgrd@data),ignore.case=T)
  modRow <- grep("^row$",colnames(Modelgrd@data),ignore.case=T)
  gridCentroids <- rgeos::gCentroid(Modelgrd,byid=TRUE)
  print(sp::proj4string(Modelgrd))
  if (!raster::compareCRS(HARNSP17ft,sp::proj4string(Modelgrd))) {
    gridCentroids <- sp::spTransform(gridCentroids,HARNSP17ft)
  }

  # Convert model cell points to a dataframe
  asSFGC <- sf::st_as_sf(gridCentroids)
  ModelGridCoords <- do.call(base::rbind,sf::st_geometry(asSFGC))
  ModelGridCoords <-base::cbind(Modelgrd@data[,modRow], Modelgrd@data[,modCol],ModelGridCoords)

  return(ModelGridCoords)
}


#' @title Define Plot options
#' @description \code{definePlotOpts} provides radio and checkboxes for Ploting options
#' @return list(matrixSource=matrixSrc, printingOn=printingOn, printAnnualOn=printAnnualOn,
#'       Animform=Animform))
#' @export
#' @examples
#'

definePlotOpts <- function(){
  win3 <- tktoplevel()
  frm1 <- tk2frame(win3,borderwidth = 3,relief = "sunken",padding = 10)
  frm2 <- tk2frame(win3,borderwidth = 3,relief = "sunken",padding = 10)
  frm3 <- tk2frame(win3,borderwidth = 3,relief = "sunken",padding = 10)

  tkpack(  tk2label(win3,text = "Define Plot Options",width = 40,
                    justify = "left",background = "#ffffff"),
           side = "top",expand = FALSE,ipadx = 5,ipady = 5,fill = "x")

  tkpack(frm3,side = "bottom",expand = TRUE, fill = "both")
  tkpack(frm1,side = "left",  expand = TRUE, fill = "both")
  tkpack(frm2,side = "right", expand = TRUE, fill = "both")

  matrixSrcLst <- c('NetRCH', 'MFRCH', 'MFEVT', 'asciiVector')
  rBtnMat = tclVar(matrixSrcLst[1])

  btn1 <- tk2radiobutton(frm1)
  btn2 <- tk2radiobutton(frm1)
  btn3 <- tk2radiobutton(frm1)
  btn4 <- tk2radiobutton(frm1)
  tkconfigure(btn1, variable = rBtnMat, value = matrixSrcLst[1])
  tkconfigure(btn2, variable = rBtnMat, value = matrixSrcLst[2])
  tkconfigure(btn3, variable = rBtnMat, value = matrixSrcLst[3])
  tkconfigure(btn4, variable = rBtnMat, value = matrixSrcLst[4])

  tkgrid(tk2label(frm1, text = trimws(matrixSrcLst[1])),btn1,padx = 10,pady =  5)
  tkgrid(tk2label(frm1, text = trimws(matrixSrcLst[2])),btn2,padx = 10,pady =  5)
  tkgrid(tk2label(frm1, text = trimws(matrixSrcLst[3])),btn3,padx = 10,pady =  5)
  tkgrid(tk2label(frm1, text = trimws(matrixSrcLst[4])),btn4,padx = 10,pady =  5)

  pltOptions <-  c("Save Raster png",
                   "Save Annual Raster png",
                   "Create Animation (avi)")

  ckbx1 <- tk2checkbutton(frm2)
  ckbx2 <- tk2checkbutton(frm2)
  ckbx3 <- tk2checkbutton(frm2)

  cbVar1 <- tclVar("0")
  cbVar2 <- tclVar("1")
  cbVar3 <- tclVar("0")

  tkconfigure(ckbx1, text = pltOptions[1], variable = cbVar1)
  tkconfigure(ckbx2, text = pltOptions[2], variable = cbVar2)
  tkconfigure(ckbx3, text = pltOptions[3], variable = cbVar3)

  tkgrid(tk2label(frm2), ckbx1,  padx = 10, pady =  5)
  tkgrid(tk2label(frm2), ckbx2,  padx = 10, pady =  5)
  tkgrid(tk2label(frm2), ckbx3,  padx = 10, pady =  5)

  btn.OK <- tk2button(frm3,text = "OK",width = -6,command = fnOK)
  btn.Cncl <-  tk2button(frm3,text = "Cancel",width = -6,command = fnCncl)
  tkgrid(btn.Cncl, btn.OK, padx = 10, pady = c(5, 15))
  tkraise(win3)
  tkbind(frm3, "<Return>", fnOK)
  tkwait.variable(done)
  tkdestroy(win3)
  if (tclvalue(done) != 1) {
    exit("User canceled Model Selection")
  }
  matrixSrc <- as.character(tclvalue(rBtnMat))
  printingOn = FALSE
  if (as.character(tclvalue(cbVar1)) == '1') {  printingOn = TRUE }
  printAnnualOn = FALSE
  if (as.character(tclvalue(cbVar2)) == '1') {  printAnnualOn = TRUE }
  Animform <- 'off'
  if (as.character(tclvalue(cbVar3)) == '1') {  Animform <- 'AVI' }
  return(list(matrixSource=matrixSrc,
              printingOn=printingOn,
              printAnnualOn=printAnnualOn,
              Animform=Animform))
}


#' @title Choose Source Data
#' @description \code{chooseDataSource} is a function to choose data:  Source of input is either read from asciiFile
#'      or taken from previously generated DiffArray3D R dataset [see \code{ReadCBCbyLayer.R}]
#' @return list(Array3D=Array3D,rasType=rasTyp))
#' @export

chooseDataSource <- function(matrixSource){

  if (matrixSource %in% c('NetRCH', 'MFRCH', 'MFEVT')) {
    if (!exists("DiffVector"))
    {
      exit("DiffVector Data unavailable...Rerun ReadCBCbyLayer.R")
    }
    if (matrixSource == 'NetRCH') {
      rasTyp = 'NetRCH'
      Array3D <-  array(DiffVector, dim = c(ncols, nrows, SPknt))
    } else if (matrixSource == 'MFRCH') {
      rasTyp = 'MFRCH'
      Array3D <-  array(CBCdata1, dim = c(ncols, nrows, SPknt))
    } else if (matrixSource == 'MFEVT') {
      rasTyp = 'MFEVT'
      Array3D <-  array(CBCdata2, dim = c(ncols, nrows, SPknt))* (-1.0)
    }
    st = paste0(startYr, "/02/01")
    dateSeries <-
      seq(as.Date(st), by = "month", length.out = max(SP_rng)) - 1
  } else  {
    win4 <- tktoplevel()
    msg = paste('Identify ASCII File with Dimensions Equal to Model:', MFmodel)
    lbl.message <- tk2label(win4, text = msg, font = fontHeading)
    tkgrid(lbl.message, padx = 30)
    tkraise(win4)
    mpath <- toString(MFmodel.Params[model,]$mpath)
    asciiFile<-choose.files(default=mpath)
    prompt <- "Enter raster Description like [EVT,RCH, etc]"
    rasTyp <- promptUser4Text(prompt)
    res = 1.0
    fileSz <- file.info(asciiFile)$size
    msg2 = paste("Reading ",round(fileSz / 1000000, digits = 2),
                 "MBytes for ",rasTyp)
    msg3 = paste("Scan will take approximately ",
                 round(fileSz / 1000000 / 13, digits = 0),"Seconds ")
    lbl.message2 <- tk2label(win4, text = msg2, font = fontHeading)
    lbl.message3 <- tk2label(win4, text = msg3, font = fontHeading)
    tkgrid(lbl.message2, padx = 30)
    tkgrid(lbl.message3, padx = 30)
    tkraise(win4)
    VectorBySP <- scan(asciiFile, what =)
    if (exists("SP_rng"))
    assign("SP_rng",seq(1,nsp),envir = .GlobalEnv)
    assign("res", 1, envir= .GlobalEnv)
    SPknt <- length(SP_rng)
    Array3D <-  array(VectorBySP, dim = c(ncols, nrows, SPknt))

    rm(VectorBySP)
    gc(verbose=TRUE)
    tkdestroy(win4)
  }
  return(list(Array3D=Array3D,rasType=rasTyp))
}


#' @title Read cell-by-cell budget header record
#' @description \code{readCBCHeader} Reads just the Header record from a binary cell-by-Cell budget file
#' @param fileptr file pointer
#' @return \code{header} as a list c(KSTP,KPER,TEXT,NC,NR,K)
#' @export

readCBCHeader <- function(filPtr) {
  while (length(record <- readBin(filPtr, raw(), 36)) > 0)
  {
    ints <- readBin(record, integer(), 9)
    txt <- intToUtf8(record[8L + seq(16)])
    KSTP <- ints[1]
    KPER <- ints[2]
    NC <- ints[7]
    NR <- ints[8]
    K <- ints[9]
    header <- list(
      KSTP = KSTP,
      KPER = KPER,
      TEXT = txt,
      NC = NC,
      NR = NR,
      K = K
    )
    return(header)
  }
  return(list())
}

#' @title Read Heads header record
#' @description \code{readHeadsHeader} Reads just the Header record from a binary Heads file
#' @param fileptr file pointer
#' @return \code{header} as a list c(KSTP,KPER,PERTIM,TOTTIM,TEXT,NC,NR,K)
#' @export

readHeadsHeader <- function(filPtr) {
  while (length(record <- readBin(filPtr, raw(), 44)) > 0)
  {
    ints <- readBin(record, integer(), 11)
    txt <- intToUtf8(record[16L + seq(16)])
    flts <- readBin(record,"double",n=11,size=4)

    KSTP <- ints[1]
    KPER <- ints[2]
    PERTIM <- flts[3]
    TOTTIM <- flts[4]
    NC <- ints[9]
    NR <- ints[10]
    K <- ints[11]
    header <- list(
      KSTP = KSTP,
      KPER = KPER,
      PERTIM = PERTIM,
      TOTTIM = TOTTIM,
      TEXT = txt,
      NC = NC,
      NR = NR,
      K = K
    )
#    print (paste(header))
    return(header)
  }
  return(list())
}

#' @title Creates list of Budget Term
#' @description \code{listBinHeaders} creates a list of Budget Term Headers available in
#'      Modflow binary cell-by-cell file
#' @param fileptr file pointer
#' @return CBCTermSet as list c(firstHeader$TEXT,firstHeader$K,firstHeader$NR,firstHeader$NC,CBCterms)
#' @export

listBinHeaders <- function(filPtr) {
  #  CBCterms <- vector("list", 100)
  CBCterms <- list()
  firstHeader <- readCBCHeader(filPtr)
  #  print(paste("$K", firstHeader$K, " $NR", firstHeader$NR, " $NC",  firstHeader$NC))
  if (firstHeader$K > 30){
    exit(paste("Invalid data in Header record.  Possibly non Binary datafile:",
               summary(to.read)$description))
  }
  kntFloats <- firstHeader$K * firstHeader$NR * firstHeader$NC
  cbcBlock <- readBin(filPtr, double(), n = kntFloats, size = 4)
  iknt <- 1
  CBCterms[[iknt]] <- firstHeader$TEXT
  repeat {
    iknt <- iknt + 1
    thisHeader <- readCBCHeader(filPtr)
    # Don't read past EOF
    if (length(thisHeader) > 0) {
      if (thisHeader$TEXT == firstHeader$TEXT) {
        cbcBlock <- readBin(filPtr, double(), n = kntFloats, size = 4)
        break
      } else {
        CBCterms[[iknt]] <- thisHeader$TEXT
        #  cbcBlock <-readBin(filPtr, double(), n = kntFloats, size = 4)
        seek(filPtr, (kntFloats * 4), origin = 'current')
      }
    }
    # Prevent Runaway [21 budget terms in output is not likely]
    if (iknt > 20) {
      break
    }
  }
  CBCTermSet <-
    list(firstHeader$TEXT,
         firstHeader$K,
         firstHeader$NR,
         firstHeader$NC,
         CBCterms)

  return(CBCTermSet)
}


#' @title Read budget term by layer
#' @description \code{readCBCbinByTerm} search for a defined budget term
#'      and returns a vector of values by stress periods identified in range of values
#' @param filPtr, term, SP_rng, lay
#' @return bigVector
#' @export

readCBCbinByTerm <- function(filPtr, term, SP_rng, lay) {
  bigVector <- vector('numeric')
  HeaderRead <- readCBCHeader(filPtr)
  kntFloats <- HeaderRead$K * HeaderRead$NR * HeaderRead$NC
  Lay1floats <- HeaderRead$NR * HeaderRead$NC
  cbcBlock <- readBin(filPtr, double(), n = kntFloats, size = 4)
  strt<-1+((lay-1)*Lay1floats)
  end <- lay*Lay1floats
  i <- 1
  cat(paste("0%.."))
  if (HeaderRead$TEXT == term  &&
      is.element(HeaderRead$KPER, SP_rng &&
                 thisHeader$KPER <= max(SP_rng))) {
    bigVector <- c(bigVector , cbcBlock)
    i <- i + 1
  } else{
    repeat {
      thisHeader <- readCBCHeader(filPtr)
      # Don't read past EOF
      if (length(thisHeader) > 0) {
        if (term ==thisHeader$TEXT   &&
            is.element(thisHeader$KPER, SP_rng) &&
            thisHeader$KPER <= max(SP_rng)) {
          i <- i + 1
          cbcBlock <-
            readBin(filPtr, double(), n = kntFloats, size = 4)

          bigVector <- c(bigVector, cbcBlock[strt:end])
          # bigVector <- c(bigVector, cbcBlock[1:Lay1floats])
        } else {
          seek(filPtr, (kntFloats * 4), origin = 'current')
        }
      }
      # don't read everything unless necessary
      if (length(thisHeader) == 0) {
        cat('\n')
        break
      }

      if (thisHeader$KPER > max(SP_rng)) {
        cat('\n')
        break
      }
      # Display % complete
      cat(paste('\r',format(as.numeric(thisHeader$KPER) / max(SP_rng) * 100,digits = 2,nsmall = 2),"%"))
    }
  }
  return(bigVector)
}

#' @title Read Heads by Stress Period
#' @description \code{readHeadsbin} searchs for a Modflow binary Heads Layers
#'      and returns a vector of values by stress periods
#'      identified in range of values
#' @param filPtr, SP_rng [stress period range]
#' @return bigVector
#' @export

#filPtr <-to.read

readHeadsbin <- function(filPtr, SP_rng) {
  bigVector <- vector('numeric')
  HeaderRead <- readHeadsHeader(filPtr)
  kntFloats <- HeaderRead$K * HeaderRead$NR * HeaderRead$NC
  Lay1floats <- HeaderRead$NR * HeaderRead$NC
  HeadBlock <- readBin(filPtr, double(), n = Lay1floats, size = 4)
  bigVector <- c(bigVector, HeadBlock[1:Lay1floats])
  i <- 1
  cat(paste("0%.."))

  repeat {
    HeaderRead <- readHeadsHeader(filPtr)
    # Don't read past EOF
    if (length(HeaderRead) > 0) {
      if (is.element(HeaderRead$KPER, SP_rng) &&
          HeaderRead$KPER <= max(SP_rng)) {
        i <- i + 1
        HeadBlock <-
          readBin(filPtr, double(), n = Lay1floats, size = 4)
        bigVector <- c(bigVector, HeadBlock[1:Lay1floats])
      } else {
        seek(filPtr, (Lay1floats * 4), origin = 'current')
      }
    }
    # don't read everything unless necessary
    if (length(HeaderRead) == 0) {
      cat('\n')
      break
    }

    if (HeaderRead$KPER > max(SP_rng)) {
      cat('\n')
      break
    }
    # Display % complete
    cat(paste('\r',format(as.numeric(HeaderRead$KPER) / max(SP_rng) * 100,digits = 2,nsmall = 2),"%"))
  }

  return(bigVector)
}
#' @title Read Heads by Layer
#' @description \code{readHeadsbinByLay} searchs for a Modflow binary Heads Layer
#'      and returns a vector of values by stress periods
#'      identified in range of values
#' @param filPtr, selectLayer
#' @return bigVector
#' @export

#filPtr <-to.read

readHeadsbinByLay <- function(to.Read, selectLayer,maxSP) {
  filPtr = file(to.Read, "rb")
  bigVector <- vector('numeric')
  HeaderRead <- readHeadsHeader(filPtr)
  kntFloats <- HeaderRead$K * HeaderRead$NR * HeaderRead$NC
  Lay1floats <- HeaderRead$NR * HeaderRead$NC
  HeadBlock <- readBin(filPtr, double(), n = Lay1floats, size = 4)
  bigVector <- c(bigVector, HeadBlock[1:Lay1floats])
  i <- 1
  cat(paste("0%.."))
  SP_rng <- maxSP-1
  repeat {
    HeaderRead <- readHeadsHeader(filPtr)
    # Don't read past EOF
    if (length(HeaderRead) > 0) {
      if (HeaderRead$K == selectLayer) {
        i <- i + 1
        HeadBlock <-
          readBin(filPtr, double(), n = Lay1floats, size = 4)
        bigVector <- c(bigVector, HeadBlock[1:Lay1floats])
      } else {
        seek(filPtr, (Lay1floats * 4), origin = 'current')
      }
    }
    # don't read everything unless necessary
    if (length(HeaderRead) == 0) {
      cat('\n')
      break
    }

    if (HeaderRead$KPER > max(SP_rng)) {
      cat('\n')
      break
    }
    # Display % complete
    cat(paste('\r',format(as.numeric(HeaderRead$KPER) / max(SP_rng) * 100,digits = 2,nsmall = 2),"%"))
  }
  close(filPtr)
  return(bigVector)
}

#' @title Read Heads for a vector of points at specified Stress Periods
#' @description \code{readHeadsbinAtPnts} searchs for Heads by Layer
#'      and returns a vector of values at each Point
#'      by Stress Periods identified in range of values
#' @param filPtr, SP_rng, PointVector
#' @return listOfPnts [with values for stress period range]
#' @export
#filPtr <-to.read

readHeadsbinAtPnts <- function(filPtr, SP_rng, PointVector) {
  bigVector <- vector('numeric')
  HeaderRead <- readHeadsHeader(filPtr)
  kntFloats <- HeaderRead$K * HeaderRead$NR * HeaderRead$NC
  Lay1floats <- HeaderRead$NR * HeaderRead$NC
  HeadBlock <- readBin(filPtr, double(), n = Lay1floats, size = 4)
  bigVector <- c(bigVector, HeadBlock[1:Lay1floats])
  listOfPnts = data.frame(heads=double())
  i <- 1
  cat(paste("0%.."))

  repeat {
    HeaderRead <- readHeadsHeader(filPtr)
    # Don't read past EOF
    if (length(HeaderRead) > 0) {
      if (is.element(HeaderRead$KPER, SP_rng) && HeaderRead$KPER <= max(SP_rng)) {

        HeadBlock <- readBin(filPtr, double(), n = Lay1floats, size = 4)
        # append each layer to bigVector
        bigVector <- c(bigVector, HeadBlock[1:Lay1floats])

        # Reformat bigVector as 3D array using col, row, lay dimensions
        # create dataframe of Head values for this stress period
        # and append them (rbind) to listofPnts
        # with the current stress period (SP)
        if (HeaderRead$K == M$nlays){
          HeadsMatrix<- array(bigVector,c(M$ncols,M$nrows,M$nlays))
          bigVector <-NULL
          df1SP <-as.data.frame(HeadsMatrix[PointVector])
          names(df1SP)<-c("Head")
          df1SP$SP <- HeaderRead$KPER
          listOfPnts <- rbind(listOfPnts,df1SP)
        }
      } else {
        seek(filPtr, (Lay1floats * 4), origin = 'current')
      }
    }
    # don't read everything unless necessary
    if (length(HeaderRead) == 0) {
      cat('\n')
      break
    }

    if (HeaderRead$KPER > max(SP_rng)) {
      cat('\n')
      break
    }
    # Display % complete
    cat(paste('\r',format(as.numeric(HeaderRead$KPER) / max(SP_rng) * 100,digits = 2,nsmall = 2),"%"))
  }

  return(listOfPnts)
}


#' @title Choose Modflow Cell-by-cell Budget Terms
#' @description GUI choices for Modflow Budget Terms
#' @param CBCterms vector created by \code{listBinHeaders}
#' @export
chooseBudgetTerms <- function(CBCterms) {
  win2 <- tktoplevel()
  frame1 <-  tk2frame(win2,borderwidth = 3,relief = "sunken",padding = 10)
  frame2 <-  tk2frame(win2,borderwidth = 3,relief = "sunken",padding = 10)
  frame3 <-  tk2frame(win2,borderwidth = 3,relief = "sunken",padding = 10)
  lbl.CBCSelect <- tk2label(win2, text = "Select Budget Terms to Extract from CBC file", font = fontHeading)
  tkpack(lbl.CBCSelect,  side = "top",  expand = FALSE,  ipadx = 5,  ipady = 5,  fill = "x")
  tkpack(frame3,side = "bottom",expand = TRUE,fill = "both")
  tkpack(frame1,side = "left",expand = TRUE,fill = "both")
  tkpack(frame2,side = "right",expand = TRUE,fill = "both")
  rBtnVal1 = tclVar(trimws(CBCterms[[1]]))
  rBtnVal2 = tclVar(trimws(CBCterms[[1]]))

  # create 2 columns of CBCterms radioButtons
  btns.f1 = vector()
  for (num in seq(1, length(CBCterms))) {
    btn <- tk2radiobutton(frame1)
    tkconfigure(btn, variable = rBtnVal1, value = num)
    tkgrid(tk2label(frame1, text = trimws(CBCterms[[num]])),btn,padx = 10,pady =  5)
    btns.f1 = append(btns.f1, btn)
  }
  btns.f2 = vector()
  for (num in seq(1, length(CBCterms))) {
    btn <- tk2radiobutton(frame2)
    tkconfigure(btn, variable = rBtnVal2, value = num)
    tkgrid(tk2label(frame2, text = trimws(CBCterms[[num]])),btn,padx = 10,pady =  5)
    btns.f2 = append(btns.f2, btn)
  }
  tkgrid(tk2button(frame3,text ="Cancel",width = -6,command = fnCncl),
         tk2button(frame3,text ="OK",    width = -6,command = fnOK  ), padx = 10,pady = c(5, 15))
  tkbind(win2, "<Return>", fnOK)

  tkraise(win2)
  tkwait.variable(done)
  tkdestroy(win2)
  if (tclvalue(done) != 1) {
    exit("User canceled Model Selection")
  }
  n1  <- as.integer((tclvalue(rBtnVal1)))
  n2  <- as.integer((tclvalue(rBtnVal2)))
  return(list(n1=n1, n2=n2))
}
#' @title Choose Modflow Cell-by-cell Budget Terms
#' @description GUI choices for Modflow Budget Terms
#' @param CBCterms vector created by \code{listBinHeaders}
#' @export
chooseBudgetTerms1Col <- function(CBCterms) {
  win2 <- tktoplevel()
  frame1 <-  tk2frame(win2,borderwidth = 3,relief = "sunken",padding = 10)
  frame3 <-  tk2frame(win2,borderwidth = 3,relief = "sunken",padding = 10)
  lbl.CBCSelect <- tk2label(win2, text = "Select Budget Terms to Extract from CBC file", font = fontHeading)
  tkpack(lbl.CBCSelect,  side = "top",  expand = FALSE,  ipadx = 5,  ipady = 5,  fill = "x")
  tkpack(frame3,side = "bottom",expand = TRUE,fill = "both")
  tkpack(frame1,side = "left",expand = TRUE,fill = "both")
  rBtnVal1 = tclVar(trimws(CBCterms[[1]]))

  # create 1 columns of CBCterms radioButtons
  btns.f1 = vector()
  for (num in seq(1, length(CBCterms))) {
    btn <- tk2radiobutton(frame1)
    tkconfigure(btn, variable = rBtnVal1, value = num)
    tkgrid(tk2label(frame1, text = trimws(CBCterms[[num]])),btn,padx = 10,pady =  5)
    btns.f1 = append(btns.f1, btn)
  }

  tkgrid(tk2button(frame3,text ="Cancel",width = -6,command = fnCncl),
         tk2button(frame3,text ="OK",    width = -6,command = fnOK  ), padx = 10,pady = c(5, 15))
  tkbind(win2, "<Return>", fnOK)

  tkraise(win2)
  tkwait.variable(done)
  tkdestroy(win2)
  if (tclvalue(done) != 1) {
    exit("User canceled Model Selection")
  }
  n1  <- as.integer((tclvalue(rBtnVal1)))
  return(list(n1=n1))
}
