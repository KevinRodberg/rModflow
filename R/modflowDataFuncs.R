#source ("//ad.sfwmd.gov/dfsroot/data/wsd/SUP/devel/source/R/ResuableFunctions/tclFuncs.R")
utils::globalVariables(c("fontHeading","fnCncl","fnOK","done",
                         "MFmodel","MFmodel.Params","model","M",
                         'DiffVector','ncols','nrows','CbyCdata1',
                         'CbyCdata2','startYr','SP_rng','nsp','TtlStrPd'))
#' @title Defines SFWMD Modflow Model characteristics
#' @description \code{defineMFmodel} defines several SFWMD Modflow Models characteristics
#' @return MFmodel.Params Data.frame providing model characteristics
#' @export

defineMFmodel <- function() {
  MFmodel = c('ECFTX', 'NPALM', 'LWCSIM','ECFM','WCFM')
  NSP=288
  NCOL=236
  NROW=552
  NLAY=7

  res = c(1250, 704, 1000, 2400, 2400)
  xmin = c(24352.000, 680961.000, 218436.000, 565465.000, 20665.000)
  ymin = c(983097.000, 840454.000, 441788.000, -44448.000, -44448.000)
  nlays = c(11,3,9,7,7)
  nrows = c(603, 292, 553, 552, 552)
  ncols = c(740, 408, 512, 236, 236)
  startYr = c(1999, 1965, 1999, 1989, 1989)
  freq = c('Month', 'Daily', 'Month','Month','Month')
  nsp = c(192, 14975, 192, 288, 288)
  code = c('MF2005-NWT', 'MF2000', 'MF2005', 'SEAWAT-2000','MODFLOW6')
  mpath =c("//whqhpc01p/hpcc_shared/dbandara/CFWI/ECFTX/Model/Transient/*.*",
           "//whqhpc01p/hpcc_shared/jgidding/LECSR/LOX18/*.*",
           "//ad.sfwmd.gov/dfsroot/data/wsd/MOD/LWCSASIAS/model/*.*",
           "//ad.sfwmd.gov/dfsroot/data/wsd/MOD/ECFM/MB/*.*",
           "//whqhpc01p/hpcc_shared/jgidding/WCFM/SENS/ORG2/*.*")
  MFmodel.Params <-
    data.frame(MFmodel, res, xmin, ymin, nlays,nrows, ncols, nsp, startYr,code,freq,mpath)
  rownames(MFmodel.Params) <- MFmodel.Params$MFmodel
  return(MFmodel.Params)
}

#' @title Exit Function
#' @description \code{exit} Provides Function to exit a little more nicely
#' @param msg Message to display on exit
#' @return \code{msg} Displays message on exit
#' @export

exit <- function(msg){
  cat(paste0("*** ERROR ***: ", msg,'\n'))
  closeAllConnections()
  stop()
  # .Internal(.invokeRestart(list(NULL, NULL), NULL))
  # options(warn=0)
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
#'      font = fontHeading

chooseModel <- function() {
  requireNamespace("tcltk2")
  requireNamespace("tcltk")
  tempfontHeading <- tcltk::tkfont.create(family = "Arial",size = 24,weight = "bold",slant = "italic")
  assign("fontHeading",tempfontHeading,envir = .GlobalEnv)
  # fontHeading <<- tkfont.create(family = "Arial",size = 24,weight = "bold",slant = "italic")
  assign("done", tcltk::tclVar(0),envir = .GlobalEnv)
  assign("fnOK", function() {tcltk::tclvalue(done) <- 1},envir = .GlobalEnv)
  assign("fnCncl", function() {tcltk::tclvalue(done) <- 2},envir = .GlobalEnv)

  # #===============================================
  # #  define ok and Cancel functions for tcl buttons
  # #  and stadardize some tcl vars
  # #===============================================
  # fnOK <- function() {
  #   tcltk::tclvalue(done) <- 1}
  #
  # fnCncl <- function() {
  #   tcltk::tclvalue(done) <- 2}

  win1 <- tcltk::tktoplevel()
  tcltk::tkraise(win1)

  MFmodels = as.vector( MFmodel.Params$MFmodel)
#  MFmodels = c('ECFTX', 'NPALM', 'LWCSIM')
  numIDs = length(MFmodels)

  rBtnVal = tcltk::tclVar(MFmodels[2])

  lbl.Select <- tcltk2::tk2label(win1, text = "Select Model", font = fontHeading)
  lbl.Blnk <- tcltk2::tk2label(win1, text = " ", font = fontHeading)
  tcltk::tkgrid(lbl.Select,columnspan = 4,padx = 100,pady = 5)

  # Radio buttons can be created and packed or added to tkgrid on the fly:
  for (num in 1:numIDs) {
    btn <- tcltk2::tk2radiobutton(win1)
    tcltk::tkconfigure(btn, variable = rBtnVal, value = MFmodels[num])
    tcltk::tkgrid(tk2label(win1, text = MFmodels[num]),btn,padx = 10,pady =  5)
  }

  btn.OK <-tk2button(win1,text = "OK",width = -6,command = fnOK)
  btn.Cncl <-tk2button(win1,text = "Cancel",width = -6,command = fnCncl)
  tcltk::tkgrid(lbl.Blnk,btn.OK,btn.Cncl,padx = c(5, 5),pady = c(5, 5))
  tcltk::tkbind(win1, "<Return>", fnOK)
  tcltk::tkraise(win1)
  tcltk::tkwait.variable(done)
  tcltk::tkdestroy(win1)

  if (tcltk::tclvalue(done) != 1) {
    exit("User canceled Model Selection")
  }
  return((tcltk::tclvalue(rBtnVal)))
}

#' @title Read Model Mesh Points
#' @description \code{readgridPoints} Reads arcGIS shape file of model mesh
#'    -Converts mesh polygons to points referencing the center of the model cells
#'    -Changes spatial reference if necessary
#'    -Converts point coordinates into a data frame
#' @param Modelgrd.Path Path to model mesh shapefile
#' @param Model.Shape Name of model mesh shapefile without .shp extension
#' @return ModelGridCoords Data.frame providing model row, column and coordinates
#' @import future
#' @import rgdal
#' @import sp
#' @export
  #' @examples
#'      Modelgrd.Path <- '//ad.sfwmd.gov/dfsroot/data/wsd/GIS/GISP_2012/DataLib/ModelData/LWCSIM'
#'      Model.Shape <- 'LWCSIM_mesh'
#' \dontrun{
#'      ModelGridCoords %<-% readgridPoints(Modelgrd.Path,Model.Shape)
#'      }

readgridPoints<- function(Modelgrd.Path,Model.Shape){
  requireNamespace("future")
  requireNamespace("sp")
  requireNamespace("rgdal")
  #=================================================================
  # NAD83 HARN StatePlane Florida East FIPS 0901 Feet
  #=================================================================
  assign("HARNSP17ft", sp::CRS("+init=epsg:2881"))
  assign("HARNUTM17Nm", sp::CRS("+init=epsg:3747"))
  assign("latlongs", sp::CRS("+proj=longlat +datum=WGS84"))


  Modelgrd <- future(rgdal::readOGR(Modelgrd.Path,Model.Shape))
  modCol <- grep("^col$",colnames(Modelgrd@data),ignore.case=TRUE)
  modRow <- grep("^row$",colnames(Modelgrd@data),ignore.case=TRUE)
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

definePlotOpts <- function(){
  requireNamespace("tcltk2")
  requireNamespace("tcltk")
  win3 <- tcltk::tktoplevel()
  frm1 <- tcltk2::tk2frame(win3,borderwidth = 3,relief = "sunken",padding = 10)
  frm2 <- tcltk2::tk2frame(win3,borderwidth = 3,relief = "sunken",padding = 10)
  frm3 <- tcltk2::tk2frame(win3,borderwidth = 3,relief = "sunken",padding = 10)

  tcltk::tkpack(  tcltk2::tk2label(win3,text = "Define Plot Options",width = 40,
                    justify = "left",background = "#ffffff"),
           side = "top",expand = FALSE,ipadx = 5,ipady = 5,fill = "x")

  tcltk::tkpack(frm3,side = "bottom",expand = TRUE, fill = "both")
  tcltk::tkpack(frm1,side = "left",  expand = TRUE, fill = "both")
  tcltk::tkpack(frm2,side = "right", expand = TRUE, fill = "both")

  matrixSrcLst <- c('NetRCH', 'MFRCH', 'MFEVT', 'asciiVector')
  rBtnMat = tcltk::tclVar(matrixSrcLst[1])

  btn1 <- tcltk2::tk2radiobutton(frm1)
  btn2 <- tcltk2::tk2radiobutton(frm1)
  btn3 <- tcltk2::tk2radiobutton(frm1)
  btn4 <- tcltk2::tk2radiobutton(frm1)
  tcltk::tkconfigure(btn1, variable = rBtnMat, value = matrixSrcLst[1])
  tcltk::tkconfigure(btn2, variable = rBtnMat, value = matrixSrcLst[2])
  tcltk::tkconfigure(btn3, variable = rBtnMat, value = matrixSrcLst[3])
  tcltk::tkconfigure(btn4, variable = rBtnMat, value = matrixSrcLst[4])

  tcltk::tkgrid(tcltk2::tk2label(frm1, text = trimws(matrixSrcLst[1])),btn1,padx = 10,pady =  5)
  tcltk::tkgrid(tcltk2::tk2label(frm1, text = trimws(matrixSrcLst[2])),btn2,padx = 10,pady =  5)
  tcltk::tkgrid(tcltk2::tk2label(frm1, text = trimws(matrixSrcLst[3])),btn3,padx = 10,pady =  5)
  tcltk::tkgrid(tcltk2::tk2label(frm1, text = trimws(matrixSrcLst[4])),btn4,padx = 10,pady =  5)

  pltOptions <-  c("Save Raster png",
                   "Save Annual Raster png",
                   "Create Animation (avi)")

  ckbx1 <- tcltk2::tk2checkbutton(frm2)
  ckbx2 <- tcltk2::tk2checkbutton(frm2)
  ckbx3 <- tcltk2::tk2checkbutton(frm2)

  cbVar1 <- tcltk::tclVar("0")
  cbVar2 <- tcltk::tclVar("1")
  cbVar3 <- tcltk::tclVar("0")

  tcltk::tkconfigure(ckbx1, text = pltOptions[1], variable = cbVar1)
  tcltk::tkconfigure(ckbx2, text = pltOptions[2], variable = cbVar2)
  tcltk::tkconfigure(ckbx3, text = pltOptions[3], variable = cbVar3)

  tcltk::tkgrid(tcltk2::tk2label(frm2), ckbx1,  padx = 10, pady =  5)
  tcltk::tkgrid(tcltk2::tk2label(frm2), ckbx2,  padx = 10, pady =  5)
  tcltk::tkgrid(tcltk2::tk2label(frm2), ckbx3,  padx = 10, pady =  5)

  btn.OK <- tcltk2::tk2button(frm3,text = "OK",width = -6,command = fnOK)
  btn.Cncl <-  tcltk2::tk2button(frm3,text = "Cancel",width = -6,command = fnCncl)
  tcltk::tkgrid(btn.Cncl, btn.OK, padx = 10, pady = c(5, 15))
  tcltk::tkraise(win3)
  tcltk::tkbind(frm3, "<Return>", fnOK)
  tcltk::tkwait.variable(done)
  tcltk::tkdestroy(win3)
  if (tcltk::tclvalue(done) != 1) {
    exit("User canceled Model Selection")
  }
  matrixSrc <- as.character(tcltk::tclvalue(rBtnMat))
  printingOn = FALSE
  if (as.character(tcltk::tclvalue(cbVar1)) == '1') {  printingOn = TRUE }
  printAnnualOn = FALSE
  if (as.character(tcltk::tclvalue(cbVar2)) == '1') {  printAnnualOn = TRUE }
  Animform <- 'off'
  if (as.character(tcltk::tclvalue(cbVar3)) == '1') {  Animform <- 'AVI' }
  return(list(matrixSource=matrixSrc,
              printingOn=printingOn,
              printAnnualOn=printAnnualOn,
              Animform=Animform))
}


#' @title Choose Source Data
#' @description \code{chooseDataSource} is a function to choose data:  Source of input is either read from asciiFile
#'      or taken from previously generated DiffArray3D R dataset [see \code{ReadCBCbyLayer.R}]
#' @param matrixSource defines source of data
#' @return list(Array3D=Array3D,rasType=rasTyp))
#' @export

chooseDataSource <- function(matrixSource){
  requireNamespace("tcltk2")
  requireNamespace("tcltk")
  tempfontHeading <- tcltk::tkfont.create(family = "Arial",size = 24,weight = "bold",slant = "italic")
  assign("fontHeading",tempfontHeading,envir = .GlobalEnv)
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
      Array3D <-  array(CbyCdata1, dim = c(ncols, nrows, SPknt))
    } else if (matrixSource == 'MFEVT') {
      rasTyp = 'MFEVT'
      Array3D <-  array(CbyCdata2, dim = c(ncols, nrows, SPknt))* (-1.0)
    }
    st = paste0(startYr, "/02/01")
    dateSeries <-
      seq(as.Date(st), by = "month", length.out = max(SP_rng)) - 1
  } else  {
    win4 <- tcltk::tktoplevel()
    msg = paste('Identify ASCII File with Dimensions Equal to Model:', MFmodel)
    lbl.message <- tcltk2::tk2label(win4, text = msg, font = fontHeading)
    tcltk::tkgrid(lbl.message, padx = 30)
    tcltk::tkraise(win4)
    mpath <- toString(MFmodel.Params[model,]$mpath)
    asciiFile<-utils::choose.files(default=mpath)
    prompt <- "Enter raster Description like [EVT,RCH, etc]"
    rasTyp <- readline(prompt=prompt)
    res = 1.0
    fileSz <- file.info(asciiFile)$size
    msg2 = paste("Reading ",round(fileSz / 1000000, digits = 2),
                 "MBytes for ",rasTyp)
    msg3 = paste("Scan will take approximately ",
                 round(fileSz / 1000000 / 13, digits = 0),"Seconds ")
    lbl.message2 <- tcltk2::tk2label(win4, text = msg2, font = fontHeading)
    lbl.message3 <- tcltk2::tk2label(win4, text = msg3, font = fontHeading)
    tcltk::tkgrid(lbl.message2, padx = 30)
    tcltk::tkgrid(lbl.message3, padx = 30)
    tcltk::tkraise(win4)
    VectorBySP <- scan(asciiFile, what =)
    if (exists("SP_rng"))
    assign("SP_rng",seq(1,nsp),envir = .GlobalEnv)
    assign("res", 1, envir= .GlobalEnv)
    SPknt <- length(SP_rng)
    Array3D <-  array(VectorBySP, dim = c(ncols, nrows, SPknt))

    rm(VectorBySP)
    gc(verbose=TRUE)
    tcltk::tkdestroy(win4)
  }
  return(list(Array3D=Array3D,rasType=rasTyp))
}


#' @title Read cell-by-cell budget header record
#' @description \code{readCBCHeader} Reads just the Header record from a binary cell-by-Cell budget file
#' @param filPtr file pointer
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
#' @param filPtr file pointer
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
#' @param filPtr file pointer
#' @return CBCTermSet as list c(firstHeader$TEXT,firstHeader$K,firstHeader$NR,firstHeader$NC,CBCterms)
#' @export

listBinHeaders <- function(filPtr) {
  #  CBCterms <- vector("list", 100)
  CBCterms <- list()
  firstHeader <- readCBCHeader(filPtr)
  #  print(paste("$K", firstHeader$K, " $NR", firstHeader$NR, " $NC",  firstHeader$NC))
  if (firstHeader$K > 30){
    exit(paste("Invalid data in Header record.  Possibly non Binary datafile:",
               summary(filPtr)$description))
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
#' @param cbbFile filename to be referenced by file pointer filPtr
#' @param term Modflow budget term
#' @param SP_rng Stress Period range
#' @param lay layer to read
#' @return bigVector
#' @export

readCBCbinByTerm <- function(filPtr, term, SP_rng, lay) {
  # filPtr <- file(cbbFile, "rb")
  bigVector <- vector('numeric')
  HeaderRead <- readCBCHeader(filPtr)
  kntFloats <- HeaderRead$K * HeaderRead$NR * HeaderRead$NC
  Lay1floats <- HeaderRead$NR * HeaderRead$NC
  cbcBlock <- readBin(filPtr, double(), n = kntFloats, size = 4)
  strt<-1+((lay-1)*Lay1floats)
  end <- lay*Lay1floats
  i <- 1
  seek(filPtr,0,origin='start')
  #cat(paste("0%.."))

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
      #cat('\n')
      break
    }

    if (thisHeader$KPER > max(SP_rng)) {
      #cat('\n')
      break
    }
    # Display % complete
    #cat(paste('\r',format(as.numeric(thisHeader$KPER) / max(SP_rng) * 100,digits = 2,nsmall = 2),"%"))
  }
  # }
  close(filPtr)
  return(bigVector)
}

#' @title Read Heads by Stress Period
#' @description \code{readHeadsbin} searchs for a Modflow binary Heads Layers
#'      and returns a vector of values by stress periods
#'      identified in range of values
#' @param filPtr file pointer
#' @param SP_rng [stress period range]
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
#' @param to.Read name of file to assign to file pointer
#' @param selectLayer selected layer to process
#' @param maxSP maximum Stress period ot process
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
#' @param filPtr file pointer
#' @param SP_rng range of stress periods
#' @param PointVector List of points
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
  requireNamespace("tcltk2")
  requireNamespace("tcltk")
  tempfontHeading <- tcltk::tkfont.create(family = "Arial",size = 24,weight = "bold",slant = "italic")
  assign("fontHeading",tempfontHeading,envir = .GlobalEnv)
  win2 <- tcltk::tktoplevel()
  frame1 <-  tcltk2::tk2frame(win2,borderwidth = 3,relief = "sunken",padding = 10)
  frame2 <-  tcltk2::tk2frame(win2,borderwidth = 3,relief = "sunken",padding = 10)
  frame3 <-  tcltk2::tk2frame(win2,borderwidth = 3,relief = "sunken",padding = 10)
  lbl.CBCSelect <- tcltk2::tk2label(win2, text = "Select Budget Terms to Extract from CBC file", font = fontHeading)
  tcltk::tkpack(lbl.CBCSelect,  side = "top",  expand = FALSE,  ipadx = 5,  ipady = 5,  fill = "x")
  tcltk::tkpack(frame3,side = "bottom",expand = TRUE,fill = "both")
  tcltk::tkpack(frame1,side = "left",expand = TRUE,fill = "both")
  tcltk::tkpack(frame2,side = "right",expand = TRUE,fill = "both")
  rBtnVal1 = tcltk::tclVar(trimws(CBCterms[[1]]))
  rBtnVal2 = tcltk::tclVar(trimws(CBCterms[[1]]))

  # create 2 columns of CBCterms radioButtons
  btns.f1 = vector()
  for (num in seq(1, length(CBCterms))) {
    btn <- tcltk2::tk2radiobutton(frame1)
    tcltk::tkconfigure(btn, variable = rBtnVal1, value = num)
    tcltk::tkgrid(tcltk2::tk2label(frame1, text = trimws(CBCterms[[num]])),btn,padx = 10,pady =  5)
    btns.f1 = append(btns.f1, btn)
  }
  btns.f2 = vector()
  for (num in seq(1, length(CBCterms))) {
    btn <- tcltk2::tk2radiobutton(frame2)
    tcltk::tkconfigure(btn, variable = rBtnVal2, value = num)
    tcltk::tkgrid(tcltk2::tk2label(frame2, text = trimws(CBCterms[[num]])),btn,padx = 10,pady =  5)
    btns.f2 = append(btns.f2, btn)
  }
  tcltk::tkgrid(tcltk2::tk2button(frame3,text ="Cancel",width = -6,command = fnCncl),
                tcltk2::tk2button(frame3,text ="OK",    width = -6,command = fnOK  ),
                padx = 10,pady = c(5, 15))
  tcltk::tkbind(win2, "<Return>", fnOK)

  tcltk::tkraise(win2)
  tcltk::tkwait.variable(done)
  tcltk::tkdestroy(win2)
  if (tcltk::tclvalue(done) != 1) {
    exit("User canceled Model Selection")
  }
  n1  <- as.integer((tcltk::tclvalue(rBtnVal1)))
  n2  <- as.integer((tcltk::tclvalue(rBtnVal2)))
  return(list(n1=n1, n2=n2))
}
#' @title Choose Modflow Cell-by-cell Budget Terms
#' @description GUI choices for Modflow Budget Terms
#' @param CBCterms vector created by \code{listBinHeaders}
#' @export
chooseBudgetTerms1Col <- function(CBCterms) {
  requireNamespace("tcltk2")
  requireNamespace("tcltk")
  tempfontHeading <- tcltk::tkfont.create(family = "Arial",size = 24,weight = "bold",slant = "italic")
  assign("fontHeading",tempfontHeading,envir = .GlobalEnv)
  win2 <- tcltk::tktoplevel()
  frame1 <-  tcltk2::tk2frame(win2,borderwidth = 3,relief = "sunken",padding = 10)
  frame3 <-  tcltk2::tk2frame(win2,borderwidth = 3,relief = "sunken",padding = 10)
  lbl.CBCSelect <- tcltk2::tk2label(win2, text = "Select Budget Terms to Extract from CBC file",
                                    font = fontHeading)
  tcltk::tkpack(lbl.CBCSelect,  side = "top",  expand = FALSE,  ipadx = 5,  ipady = 5,  fill = "x")
  tcltk::tkpack(frame3,side = "bottom",expand = TRUE,fill = "both")
  tcltk::tkpack(frame1,side = "left",expand = TRUE,fill = "both")
  rBtnVal1 = tcltk::tclVar(trimws(CBCterms[[1]]))

  # create 1 columns of CBCterms radioButtons
  btns.f1 = vector()
  for (num in seq(1, length(CBCterms))) {
    btn <- tcltk2::tk2radiobutton(frame1)
    tcltk::tkconfigure(btn, variable = rBtnVal1, value = num)
    tcltk::tkgrid(tcltk2::tk2label(frame1, text = trimws(CBCterms[[num]])),btn,padx = 10,pady =  5)
    btns.f1 = append(btns.f1, btn)
  }

  tcltk::tkgrid(tcltk2::tk2button(frame3,text ="Cancel",width = -6,command = fnCncl),
         tcltk2::tk2button(frame3,text ="OK",    width = -6,command = fnOK  ),
         padx = 10,pady = c(5, 15))
  tcltk::tkbind(win2, "<Return>", fnOK)

  tcltk::tkraise(win2)
  tcltk::tkwait.variable(done)
  tcltk::tkdestroy(win2)
  if (tcltk::tclvalue(done) != 1) {
    exit("User canceled Model Selection")
  }
  n1  <- as.integer((tcltk::tclvalue(rBtnVal1)))
  return(list(n1=n1))
}
#' @title Select Stress Period Range
#' @description Accept a string defining range of integers vals which are reformed as a unique sequence
#' @export

readRange <- function() {
  requireNamespace("tcltk2")
  requireNamespace("tcltk")
  winB <- tcltk::tktoplevel()
  msg = paste("Total Number Stress Periods Available=", TtlStrPd,
              "\n\nChoose Range or Periods of interest \n i.e.: 1:3,5,7:100,200 \n")

  lbl.msg <- tcltk2::tk2label(winB, text = msg, font = fontHeading)
  tcltk::tkgrid(lbl.msg, padx = 30)
  tcltk::tkraise(winB)
  entryInit=""
  btn.OK <- tcltk2::tk2button(winB,text = "OK",width = -6,command = fnOK)
  btn.Cncl <- tcltk2::tk2button(winB,text = "Cancel",width = -6,command = fnCncl)
  rangeVarTcl <- tcltk::tclVar(paste(entryInit))
  textEntryWidget <- tcltk2::tk2entry(winB, width = 35, textvariable = rangeVarTcl)
  tcltk::tkgrid(tcltk::tklabel(winB, text = "Range of values",font = fontHeading),
         textEntryWidget, btn.OK,btn.Cncl,padx = 10, pady = 5)
  tcltk::tkbind(winB, "<Return>", fnOK)
  tcltk::tkraise(winB)
  tcltk::tkwait.variable(done)
  tcltk::tkdestroy(winB)
  if (tcltk::tclvalue(done) != 1) {
    exit("User canceled Model Selection")
  }
  #  Convert string of numeric vals to a range
  rngStr <-tcltk::tclvalue(rangeVarTcl)
  rngStr <- gsub(" ", ",", rngStr)
  rngStr <- gsub(",,", ",", rngStr)

  df <- as.vector(rngStr)
  rng <-
    sapply(df, function(x)
      dget(textConnection(paste('c(', x, ')'))))
  rng <- unique(sort(rng))
  return(rng)
}
