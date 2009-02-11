`PhViD.gui` <-
function(dummy=NULL){
require(tcltk)

trunc.fnc <- function() {
        TVal <- as.character(tclvalue(TruncBool))
        if (TVal == 1) tkconfigure(trunc.val, state = "normal")
        else tkconfigure(trunc.val, state = "disabled")
}
seuil.fnc <- function() {
        SVal <- as.character(tclvalue(SeuilBool))
        if (SVal == 1) tkconfigure(seuil.val, state = "normal")
        else tkconfigure(seuil.val, state = "disabled")
}


#########                   fonction Select_Method                      ########
Select_Method <- function() {
  # création de la liste déroulante pour le choix de la méthode
  tclRequire("BWidget") # nécessaire pour créer une liste déroulante
  tmethod <- tktoplevel()
  tkwm.title(tmethod, "Select Method")
  tkwm.deiconify(tmethod)
  tkgrab.set(tmethod)
  tkfocus(tmethod)
  tkgrid(tklabel(tmethod,text="Select a Method"))
  Method <- c("GPS","BCPNN","ROR","PRR","RFET")
  comboBox <- tkwidget(tmethod,"ComboBox",editable=FALSE,values=Method)
  tkgrid(comboBox)

  OnVALID <- function()
  {
    MethodChoice <<- Method[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
    #msg <- paste("You have selected the ",MethodChoice," method!",sep="")
    #tkmessageBox(title="Method Choice",message=msg)
    #assign("Choice", MethodChoice, inherits = TRUE)
    tkgrab.release(tmethod)
    tkdestroy(tmethod)
    #tkdestroy(toption)
    #tkfocus(toption)
  }
  OnCancel <- function() {
    MethodChoice <<- "None"
    tkdestroy(tmethod)
  }
  
  Method.but <- tkbutton(tmethod, text="   Valid   ", command = OnVALID)
  End.but <- tkbutton(tmethod, text = "Cancel", command = OnCancel)
  tkgrid(Method.but,End.but)
  tkfocus(tmethod)
  #tkbind(tmethod, "<Destroy>", function () {tkgrab.release(tmethod);tkfocus(tt)})
  #tkbind(Method, "<Return>", onVALID)
  tkwait.window(tmethod)
  
  return(MethodChoice)
}
#########                  fin fonction Select_Method                   ########


#"############################# LES OPTIONS ####################################
Method <- function() {
 Choice <- Select_Method()
 toption <- tktoplevel()
 if (as.character(Choice) == "None") tkdestroy(toption)
 else {
  DATA <- get("DATA", envir = .GlobalEnv)
  n11 <- tclVar("1")
  decision <- tclVar(1) # 1 = FDR, 2 = Nb of Signals, 3 = RANKSTAT
  rankstat <- tclVar(1) # legend differs according to the method
  RRval <- tclVar("1")
  FDRval <- tclVar("0.05")
  NbSval <- tclVar("5000")
  PPval <- tclVar("")
  text.RR <- "RR"
  title.rankstat <- "RANKSTAT"
  rankstat1.title <- "post.H0"
  tronc <- tclVar("0") # for the GPS method only
  marge <- tclVar("1") # for the GPS method only
  hyper <- tclVar(1) # GPS method only. change (1) the prior param initialization. (2) the final values of prio param
  alpha1.init <- tclVar(".2")
  beta1.init <-  tclVar(".06")
  alpha2.init <- tclVar("1.4")
  beta2.init <- tclVar("1.8")
  q.init <- tclVar(".1")
  alpha1.out <- tclVar("")
  beta1.out <-  tclVar("")
  alpha2.out <- tclVar("")
  beta2.out <- tclVar("")
  q.out <- tclVar("")
  MCMC <- tclVar(1) # for the BCPNN method only (1) usual BCPNN (2) MC method
  nb.monteCarlo <- tclVar("10000")
  PRIOR.PARAM <- NULL
  EVENTval <- tclVar("")
  DRUGval <- tclVar("")
  result <- NULL
  assign("RESULT",result,inherits=FALSE,envir=.GlobalEnv) # on réinitialise RESULT, METHOD
  method <- NULL
  assign("METHOD",method,inherits=FALSE,envir=.GlobalEnv) # et FOUND à NULL dès qu'on ouvre
  found <- NULL
  assign("FOUND",found,inherits=FALSE,envir=.GlobalEnv)   # une nouvelle fenêtre méthode
  text.FDR <- "FDR"
  title.FDR <- "GPS FDR & FNR"

  # functions    
  decision.fnc <- function() {
    if (tclvalue(decision) == 1) {
        tkconfigure(deci1, state = "normal")
        tkconfigure(deci2, state = "disabled")
        tkconfigure(deci3, state = "disabled")
        }
    if (tclvalue(decision) == 2) {
        tkconfigure(deci1, state = "disabled")
        tkconfigure(deci2, state = "normal")
        tkconfigure(deci3, state = "disabled")
        }
    if (tclvalue(decision) == 3) {
        tkconfigure(deci1, state = "disabled")
        tkconfigure(deci2, state = "disabled")
        tkconfigure(deci3, state = "normal")
        }}

  rankstat.fnc <- function() {
    if  (tclvalue(rankstat) == 1) {
      tkconfigure(deci1, state = "normal")
      tkconfigure(deci1.rbtn, state = "normal")
    }
    if  (tclvalue(rankstat) == 2) {
      tkconfigure(deci1, state = "disabled")
      tkconfigure(deci1.rbtn, state = "disabled")
    }
  }
  
  tronc.fnc <- function() {
    if (tclvalue(tronc) == 0) tkconfigure(marge.ety, state = "disabled")
    if (tclvalue(tronc) == 1) tkconfigure(marge.ety, state = "normal")
  }
    
  hyper.fnc <- function() {
      if (tclvalue(hyper) == 1) {
                tkconfigure(alpha1.ety, state = "normal")
                tkconfigure(beta1.ety, state = "normal")
                tkconfigure(alpha2.ety, state = "normal")
                tkconfigure(beta2.ety, state = "normal")
                tkconfigure(q.ety, state = "normal")
                tkconfigure(alpha1.out.ety, state = "disabled")
                tkconfigure(beta1.out.ety, state = "disabled")
                tkconfigure(alpha2.out.ety, state = "disabled")
                tkconfigure(beta2.out.ety, state = "disabled")
                tkconfigure(q.out.ety, state = "disabled")
                }
      if (tclvalue(hyper) == 2) {
                tkconfigure(alpha1.ety, state = "disabled")
                tkconfigure(beta1.ety, state = "disabled")
                tkconfigure(alpha2.ety, state = "disabled")
                tkconfigure(beta2.ety, state = "disabled")
                tkconfigure(q.ety, state = "disabled")
                tkconfigure(alpha1.out.ety, state = "normal")
                tkconfigure(beta1.out.ety, state = "normal")
                tkconfigure(alpha2.out.ety, state = "normal")
                tkconfigure(beta2.out.ety, state = "normal")
                tkconfigure(q.out.ety, state = "normal")
                }
  }
  
  monteCarlo.fnc <- function() {
      if (tclvalue(MCMC) == 1) tkconfigure(nb.monteCarlo.ety, state = "disabled")
      if (tclvalue(MCMC) == 2) tkconfigure(nb.monteCarlo.ety, state = "normal")
  }
  
  postMsg <- function(msg) {
        tkconfigure(message.txt, state = "normal")
        tkinsert(message.txt, "end", msg)
        tkconfigure(message.txt, state = "disabled")
  }
  
  # remove HELP button
  #Help <- function() {
  #  thelp <- tktoplevel()
  #  tkwm.title(thelp, "Help")
  #  xscr <- tkscrollbar(thelp, repeatinterval=5,orient="horizontal",
  #                         command=function(...)tkxview(txt,...))
  #  yscr <- tkscrollbar(thelp, repeatinterval=5,
  #                         command=function(...)tkyview(txt,...))
  #  txt <- tktext(thelp,bg="white",font="courier",
  #      xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(yscr,...),
  #      wrap="none")
  #  tkgrid(txt,yscr)
  #  tkgrid(xscr)
  #  tkgrid.configure(yscr,sticky="ns")
  #  tkgrid.configure(xscr,sticky="ew")
  #  tkinsert(txt,"end",paste("ABOUT CHARACTERISTICS\n\n- RR (or OR) must be a positive double. It represents the value",
  #    "of the risk\nyou want to consider. By default, RR=1.\n\n"))
  #  tkinsert(txt,"end",paste("- Number of notifications : you may prefer to focus your interest only on\nsignals which",
  #    "have at least a certain number on notifications.\nIt does not alter the calculs carried out by the method.\n\n"))
  #  tkconfigure(txt, state="disabled")
  #  tkfocus(txt)
  #}
    
  execute.fnc <- function() {
      #rm(RESULT,METHOD)
      if (is.null(DATA)) postMsg("Error: notifications haven't been read yet.\n")
      else {
          RR0 <- as.numeric(tclvalue(RRval))
          MIN.n11 <- as.numeric(tclvalue(n11))
          DECISION <- as.numeric(tclvalue(decision))
          RANKSTAT <- as.numeric(tclvalue(rankstat))
          if (DECISION == 1) DECISION.THRES <- as.numeric(tclvalue(FDRval))
          else {
              if (DECISION == 2) DECISION.THRES <- as.numeric(tclvalue(NbSval))
              else {
                  DECISION.THRES <- as.numeric(tclvalue(PPval))
              }
          }
          # ADVANCED OPTIONS and prior param initialisation for GPS
          if (as.character(Choice) == "GPS") {
              TRONC <- as.numeric(tclvalue(tronc)) == 1
              seuil.tronc <- 1
              if (TRONC == TRUE) seuil.tronc <- as.numeric(tclvalue(marge))
              if (as.numeric(tclvalue(hyper)) == 1)
              PRIOR.INIT = c(alpha1 = as.numeric(tclvalue(alpha1.init)),
                                   beta1 = as.numeric(tclvalue(beta1.init)),
                                   alpha2 = as.numeric(tclvalue(alpha2.init)),
                                   beta2 = as.numeric(tclvalue(beta2.init)),
                                   q = as.numeric(tclvalue(q.init)))
              else
              PRIOR.PARAM = c(as.numeric(tclvalue(alpha1.out)),
                              as.numeric(tclvalue(beta1.out)),
                              as.numeric(tclvalue(alpha2.out)),
                              as.numeric(tclvalue(beta2.out)),
                              as.numeric(tclvalue(q.out)))
          }
          # ADVANCED OPTIONS for BCPNN
          if (as.character(Choice) == "BCPNN") {
                MC <- as.numeric(tclvalue(MCMC)) == 2
                if (MC == TRUE) NB.monteCarlo <- as.numeric(tclvalue(nb.monteCarlo))
          }
          # ADVANCED OPTIONS for RFET
          if (as.character(Choice) == "RFET") {
                MID.PVAL <- as.numeric(tclvalue(midpval.val)) == 1
          }
          method <- as.character(Choice)
          if ((method == "ROR" | method == "PRR") & (DECISION == 1 & RANKSTAT == 2))
            postMsg(paste("Error: When using ",rankstat2.title," you have to select a decision between Number of Signals and\nRANKSTAT ","\n",sep=""))
          else {
            postMsg("Computing method...\n")
            res.method <- switch(method,
              GPS  =  GPS(DATABASE=DATA, RR0 = RR0, MIN.n11=MIN.n11, DECISION=DECISION,
                      DECISION.THRES=DECISION.THRES, RANKSTAT=RANKSTAT,TRONC = TRONC,
                      TRONC.THRES=seuil.tronc, PRIOR.INIT=PRIOR.INIT, PRIOR.PARAM=PRIOR.PARAM),
              BCPNN =  BCPNN(DATABASE=DATA, RR0=RR0, MIN.n11=MIN.n11,
                      DECISION=DECISION, DECISION.THRES=DECISION.THRES,
                      RANKSTAT = RANKSTAT, MC=MC, NB.MC=NB.monteCarlo) ,
              ROR  =  ROR(DATABASE=DATA, OR0=RR0, MIN.n11=MIN.n11,
                      DECISION=DECISION, DECISION.THRES=DECISION.THRES,
                      RANKSTAT = RANKSTAT) ,
              PRR  =  PRR(DATABASE=DATA, RR0=RR0, MIN.n11=MIN.n11,
                      DECISION = DECISION, DECISION.THRES = DECISION.THRES,
                      RANKSTAT = RANKSTAT) ,
              RFET =  RFET(DATABASE=DATA, OR0=RR0, MIN.n11=MIN.n11,
                      DECISION = DECISION, DECISION.THRES = DECISION.THRES,
                      MID.PVAL = MID.PVAL)
                      )
            postMsg("Done\n\n")
            assign("RESULT", res.method, inherits = FALSE, envir = .GlobalEnv)
            assign("METHOD", method, inherits = FALSE, envir = .GlobalEnv)
          }
      }
  }
    
  SaveOutput.fnc <- function () {
    #if (as.character(Choice)!=METHOD) RESULT <- NULL # pour prévenir des erreurs d'une méthode à l'autre
    RESULT <- get("RESULT", envir = .GlobalEnv)
    if (is.null(RESULT)) postMsg("Error: method hasn't been computed yet.\n")
    else if (class(RESULT) == "list") {
      postMsg("Writing results to file...\n")
      flnm <- tclvalue(tkgetSaveFile())
      if (flnm != "") {
        write.table(RESULT$INPUT.PARAM, file = paste(flnm, "_param.csv", sep =""), sep = ";",
                    dec = ".", row.names = FALSE, col.names = TRUE)
        write.table(RESULT$ALLSIGNALS, file = paste(flnm, "_allsig.csv", sep =""), sep = ";",
                    row.names = FALSE, col.names = TRUE)
        write.table(RESULT$SIGNALS, file = paste(flnm, "_sig.csv", sep =""), sep = ";",
                    row.names = FALSE, col.names = TRUE)
        METHOD <- get("METHOD", envir = .GlobalEnv)
        if (METHOD == "GPS") {
          write.table(RESULT$PARAM$PRIOR.PARAM, file = paste(flnm, "_priorparam.csv", sep =""),
                      sep = ";", row.names = FALSE, col.names = TRUE)
        }
        postMsg("done.\n")
      }
      else postMsg ("aborted.\n")
    }
  }

  plot.fnc <- function () {
   RESULT <- get("RESULT", envir = .GlobalEnv)
   if (is.null(RESULT)) postMsg("Error: method hasn't been computed yet.\n")
   else {
    METHOD <- get("METHOD", envir = .GlobalEnv)
    if (METHOD == "ROR" | METHOD == "PRR" | METHOD == "RFET")
    PLOT <- (as.numeric(tclvalue(FDR.val)) == 1 | as.numeric(tclvalue(statint.val)) == 1)
    else PLOT <- (as.numeric(tclvalue(FDR.val)) == 1 | as.numeric(tclvalue(statint.val)) == 1 | as.numeric(tclvalue(SENSI.val)) == 1 | as.numeric(tclvalue(ROC.val)) == 1)  
    if (PLOT==FALSE) postMsg("Error: No Graph selected.\n")
    else {
      if (METHOD == "ROR" | METHOD == "PRR" | METHOD == "RFET") {
        if (as.numeric(tclvalue(FDR.val)) == 1) {
          BOOL_FDR <- as.numeric(tclvalue(rankstat)) == 1
          #print(BOOL_FDR)
          if (BOOL_FDR == FALSE) postMsg("Error: FDR can not be ploted if you haven't selected the pvalue decision.\n")
          else {
            if (dev.cur() > 1) x11()
            #plot(RESULT$OpChar,type="l",main=paste(METHOD,"FDR"),
            #    xlab="number of signals",ylab="",col=c(2,3),lty=c(1,1),lwd=c(1,1))
            #segments(0,RESULT$OpChar[RESULT$NB.SIGNALS],RESULT$NB.SIGNALS,RESULT$OpChar[RESULT$NB.SIGNALS])
            #segments(RESULT$NB.SIGNALS,0,RESULT$NB.SIGNALS,RESULT$OpChar[RESULT$NB.SIGNALS])
            #legend("right", c(paste("FDR=",round(RESULT$OpChar[RESULT$NB.SIGNALS],3),sep=""),
            #                  paste("Number of Signals=",RESULT$NB.SIGNALS,sep="")))
            plot(RESULT$ALLSIGNALS[,9],type="l",main=paste(METHOD,"FDR"),
                xlab="number of signals",ylab="",col=c(2,3),lty=c(1,1),lwd=c(1,1))
            segments(0,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,9],RESULT$NB.SIGNALS,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,9])
            segments(RESULT$NB.SIGNALS,0,RESULT$NB.SIGNALS,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,9])
            legend("right", c(paste("FDR=",round(RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,9],3),sep=""),
                              paste("Number of Signals=",RESULT$NB.SIGNALS,sep="")))
          }
        }
        if (as.numeric(tclvalue(statint.val)) == 1) {
          
          if (dev.cur() > 1) x11()
          RANKSTAT <- as.numeric(tclvalue(rankstat))
          if (RANKSTAT == 1) statint.lbl <- "pvalue"
          if (RANKSTAT == 2) {
            if(METHOD == "ROR") statint.lbl <- "Q_0.025(log(ROR))"
            if(METHOD == "PRR") statint.lbl <- "Q_0.025(log(PRR))"
          }
          #plot(RESULT$COMPARE$STAT[order(RESULT$COMPARE$STAT)],(RESULT$STATISTIC[,1])[order(RESULT$COMPARE$STAT)],
	  plot(RESULT$ALLSIGNALS[,5],RESULT$ALLSIGNALS[,3],
            log="y",pch=".",
            main=paste(METHOD, "\n Number of notifications according to ",statint.lbl,sep=""),
            xlab="statistic of interest",ylab="number of notifications")
        }
      }
      if (METHOD == "GPS" | METHOD == "BCPNN") {
        if (as.numeric(tclvalue(FDR.val)) == 1) {
          if (dev.cur() > 1) x11()
          matplot(cbind(RESULT$ALLSIGNALS[,9],RESULT$ALLSIGNALS[,10]),
              type="l", main=paste(METHOD,"FDR & FNR"), xlab="number of signals", ylab="", col=c(2,3), lty=c(1,1), lwd=c(1,1))
          segments(0,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,9],RESULT$NB.SIGNALS,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,9])
          segments(RESULT$NB.SIGNALS,0,RESULT$NB.SIGNALS,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,9])
          segments(0,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,10],RESULT$NB.SIGNALS,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,10])
          segments(RESULT$NB.SIGNALS,0,RESULT$NB.SIGNALS,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,10])
          legend("right", c(paste("FDR=",round(RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,9],3),sep=""),
                            paste("FNR=",round(RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,10],3),sep=""),
                            paste("Number of Signals=",RESULT$NB.SIGNALS,sep="")))
          legend("top", legend=c("FDR","FNR"), col=c(2,3), lty=c(1,1), lwd=c(1,1))
        }
        if (as.numeric(tclvalue(SENSI.val)) == 1) {
          if (dev.cur() > 1) x11()
          matplot(cbind(RESULT$ALLSIGNALS[,11],RESULT$ALLSIGNALS[,12]), pch=".",
              main=paste(METHOD,"- Sensibility and Specificity", sep=" "),
              xlab="number of signals", ylab="", col=c(2,3), lty=c(1,1), lwd=c(1,1))
          segments(0,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,11],RESULT$NB.SIGNALS,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,11])
          segments(RESULT$NB.SIGNALS,0,RESULT$NB.SIGNALS,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,11])
          segments(0,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,12],RESULT$NB.SIGNALS,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,12])
          segments(RESULT$NB.SIGNALS,0,RESULT$NB.SIGNALS,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,12])
          legend("bottom", c(paste("Se=",round(RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,11],3),sep=""),
                             paste("Sp=",round(RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,12],3),sep=""),
                             paste("Number of Signals=",RESULT$NB.SIGNALS,sep="")))
          legend("right",legend=c("Se","Sp"),col=c(2,3),lty=c(1,1),lwd=c(1,1))
        }
        if (as.numeric(tclvalue(ROC.val)) == 1) {
          if (dev.cur() > 1) x11()
          plot(1-RESULT$ALLSIGNALS[,12],RESULT$ALLSIGNALS[,11],type="l",main=paste(METHOD,"- ROC curve"),
              xlab="1-Sp", ylab="Se", col=2)
          segments(0,RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,11],1-RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,12],
              RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,11])
          segments(1-RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,12],0,1-RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,12],
              RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,11])
          legend("right", c(paste("1-Sp=",round(1-RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,12],3),sep=""),
                            paste("Se=",round(RESULT$ALLSIGNALS[RESULT$NB.SIGNALS,11],3),sep=""),
                            paste("Number of Signals=",RESULT$NB.SIGNALS,sep="")))
        }
        if (as.numeric(tclvalue(statint.val)) == 1) {
          if (dev.cur() > 1) x11()
          RANKSTAT <- as.numeric(tclvalue(rankstat))
          if (RANKSTAT == 1) statint.lbl <- "post.H0"
          if (RANKSTAT == 2) {
            if(METHOD == "GPS") statint.lbl <- "Q_0.05(lambda)"
            if(METHOD == "BCPNN") statint.lbl <- "Q_0.025(log(IC))"
          }
          if (RANKSTAT == 3) statint.lbl <- "post.E"
          #plot(RESULT$COMPARE$STAT[order(RESULT$COMPARE$STAT)],(RESULT$STATISTIC[,1])[order(RESULT$COMPARE$STAT)],
	  plot(RESULT$ALLSIGNALS[,5],RESULT$ALLSIGNALS[,3],
            log="y", pch=".",
            main=paste(METHOD, "\n Number of notifications according to ",statint.lbl,sep=""),
            xlab="statistic of interest",ylab="number of notifications")
        }
      }
    }
    if (as.character(Choice)!=METHOD) postMsg(paste("Warning, you are trying to make plots from the ",as.character(Choice),
        " method but the last method which has been \ncomputed is the ",METHOD," Method. \nYou need first to compile the ",
        as.character(Choice)," Method from this current window if you want to draw the \ncorrects plots \n",sep=""))
   }
  }
  
  search.fnc <- function() {
    #if (as.character(Choice)!=METHOD) RESULT <- NULL # pour prévenir des erreurs d'une méthode à l'autre
    RESULT <- get("RESULT", envir = .GlobalEnv)
    if (is.null(RESULT)) postMsg("Error: method hasn't been computed yet.\n")
    else {
      #function pour créer la table de sortie
      displayInTable <- function(tclarray,title="Research result",height=-1,width=-1,nrow=-1,ncol=-1) {
        #require(tcltk)
        ttresearch <- tktoplevel()
        #tclRequire("Tktable")
        tkwm.title(ttresearch,title) #titlerows=0,titlecols=1,
        table1 <- tkwidget(ttresearch,"table",rows=nrow,cols=ncol,height=height+1,width=30,titlerows=1,
                     xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...))
        xscr <-tkscrollbar(ttresearch,orient="horizontal", command=function(...)tkxview(table1,...))
        yscr <- tkscrollbar(ttresearch,command=function(...)tkyview(table1,...))

        tkgrid(table1,yscr)
        tkgrid.configure(yscr,sticky="nsw")
        tkgrid(xscr,sticky="new")
        tkconfigure(table1,variable=tclarray,background="white",selectmode="extended")
        return (table1)
      } # fin intern function

      drug.code <- NULL
      event.code <- NULL
      if (as.character(tclvalue(DRUGval)) != "") drug.code <- as.character(tclvalue(DRUGval))
      if (as.character(tclvalue(EVENTval)) != "") event.code <- as.character(tclvalue(EVENTval))
      FIND <- PhViD.search(RESULT = RESULT, DRUG = drug.code, EVENT = event.code)
      # batterie de test pour comprendre où est le problème
      if (is.null(FIND$DRUG) & is.null(FIND$EVENT)) postMsg("Error: you have to specify at least a drug or an event.\n")
      else if ((is.null(FIND$DRUG)==FALSE & is.null(FIND$EVENT)==FALSE) & (FIND$EXIST_DRUG == FALSE & FIND$EXIST_EVENT == FALSE)) postMsg("Error: no such drug and event found in the database.\n")
      else if ((is.null(FIND$DRUG)==FALSE & is.null(FIND$EVENT)==FALSE) & (FIND$EXIST_DRUG == FALSE)) postMsg("Error: no such drug found in the database.\n")
      else if ((is.null(FIND$DRUG)==FALSE & is.null(FIND$EVENT)==FALSE) & (FIND$EXIST_EVENT == FALSE)) postMsg("Error: no such event found in the database.\n")
      else if ((is.null(FIND$DRUG)==FALSE & is.null(FIND$EVENT)==FALSE) & (FIND$EXIST_COUPLE == FALSE)) postMsg("Error: both of this drug and this event exist but the number of notification is either zero or inferior to the\nminimum number of notification you have specify in characteristics.\n")
      else if (FIND$EXIST_DRUG == FALSE) postMsg("Error: no such drug found in the database.\n")
      else if (FIND$EXIST_EVENT == FALSE) postMsg("Error: no such event found in the database.\n")
      else {
        FIND$LIST[,4] <- round(FIND$LIST[,4],2)           # on limite le nb de
        FIND$LIST[,c(5,7)] <- round(FIND$LIST[,c(5,7)],3) # décimales à afficher
        tclRequire("Tktable")
        L <- dim(FIND$LIST)[1] + 1
        C <- dim(FIND$LIST)[2]
        TAB <- tclArray()
        for (j in 1:C)  TAB[[0,j-1]] <- colnames(FIND$LIST)[j]
        for (i in 2:L)
          for (j in 1:C) {
            if (j == 1 | j == 2) TAB[[i-1,j-1]] <- as.character(FIND$LIST[i-1,j])
            else TAB[[i-1,j-1]] <- FIND$LIST[i-1,j]
          }
        table1 <- displayInTable(TAB,nrow=L,ncol=C)
        assign("FOUND", FIND, inherits = FALSE, envir = .GlobalEnv)
      }
    }
  }
  
  Savefindings.fnc <- function() {
    FOUND <- get("FOUND", envir = .GlobalEnv)
    if (is.null(FOUND)) postMsg("Error: research hasn't been computed yet.\n")
    else {
      postMsg("Writing result to file...\n")
      flnm2 <- tclvalue(tkgetSaveFile())
      if (flnm2 != "") {
        write.table(FOUND$LIST, file = paste(flnm2, ".csv", sep =""), sep = ";",
                    row.names = FALSE, col.names = TRUE)
      }
    }
  }
     # fin functions

  if (as.character(Choice) == "GPS") { # DUMOUCHEL
      tkwm.title(toption,"GPS Options")
      text.FDR <- "FDR & FNR"
      rankstat2.title <- "Q_0.05(lambda)"
  }    
  if (as.character(Choice) == "BCPNN") {
      tkwm.title(toption,"BCPNN Options")
      text.FDR <- "FDR & FNR"
      title.FDR <- "BCPNN FDR & FNR"
      rankstat2.title <- "Q_0.025(log(IC))"
  }
  if (as.character(Choice) == "ROR") {
      tkwm.title(toption,"ROR Options")
      text.RR <- "OR"
      rankstat1.title <- "pvalue"
      text.FDR <- "FDR (Non evaluable for the Lower Bound rule decision)"
      title.FDR <- "ROR FDR"
      rankstat2.title <- "Q_0.025(log(ROR))"
  }
  if (as.character(Choice) == "PRR") {
      tkwm.title(toption,"PRR Options")
      rankstat1.title <- "pvalue"
      text.FDR <- "FDR (Non evaluable for the Lower Bound rule decision)"
      title.FDR <- "PRR FDR"
      rankstat2.title <- "Q_0.025(log(PRR))"
  }    
  if (as.character(Choice) == "RFET") { # FISHER
      tkwm.title(toption,"RFET Options")
      text.RR <- "OR"
      title.FDR <- "RFET FDR"
      title.rankstat <- "PVALUE"
  }

  toption.frm <- tkframe (toption, height = 20)
  ############################## CHARACTERISTICS #################################
  characterictics.frm <- tkframe (toption.frm, relief = "groove", bd= 6)
  characterictics.title <- tklabel(characterictics.frm, text = "CHARACTERISTICS",
      font = "Helvetica 12")
  tkpack(characterictics.title, side = "top")
      rr.frm <- tkframe (characterictics.frm)
          RR.lbl <- tklabel(rr.frm, text = text.RR, anchor = "e")
          RR <- tkentry(rr.frm, textvariable = RRval, width = 7, justify = "center")
          tkpack(RR.lbl, side = "left")
          tkpack(RR, side = "left")
        tkpack(rr.frm, side = "top", expand = TRUE)
      n11.frm <- tkframe (characterictics.frm)
          n11.lbl <- tklabel(n11.frm, text = "Minimum number of notifications for a couple to be potentially considered  as a signal", anchor = "e")
          n11.ety <- tkentry(n11.frm, textvariable = n11, width = 7, justify = "center")
          tkpack(n11.lbl, side = "left")
          tkpack(n11.ety, side = "left")
        tkpack(n11.frm, side = "top", expand = TRUE)
  tkpack(characterictics.frm, fill = "both", expand = TRUE)
  
  ################################ RULE DECISION #################################
  decision.frm <- tkframe(toption.frm, relief = "groove", bd = 6)
  decision.title <- tklabel(decision.frm, text = "DECISION RULE", font = "Helvetica 12")
  tkpack(decision.title, side = "top")
    
    deci.left.frm <- tkframe(decision.frm, relief = "groove", bd = 2)
      deci.left.inside.frm <- tkframe(deci.left.frm)
        deci.lbl <- tklabel(deci.left.inside.frm,
            text = "Threshold for signal detection:")
        tkpack(deci.lbl, side = "top")
        
        d1.frm <- tkframe(deci.left.inside.frm, padx = 10)
            deci1.rbtn <- tkradiobutton(d1.frm, text = "FDR", value = 1,
                variable = decision, command = decision.fnc)
            deci1 <- tkentry(d1.frm, textvariable = FDRval, width = 8,
                justify = "center", state = "normal")
            tkpack(deci1.rbtn, side = "top", anchor = "w")
            tkpack(deci1, side = "top")
        tkpack(d1.frm, side = "left", expand = TRUE)
        
        d2.frm <- tkframe(deci.left.inside.frm, padx = 10)
            deci2.rbtn <- tkradiobutton(d2.frm, text = "Number of signals",
                value = 2, variable = decision, command = decision.fnc)
            deci2 <- tkentry(d2.frm, textvariable = NbSval, width = 8,
                justify = "center", state = "disabled")
            tkpack(deci2.rbtn, side = "top", anchor = "w")
            tkpack(deci2, side = "top")
        tkpack(d2.frm, side = "left", expand = TRUE)
        
        d3.frm <- tkframe(deci.left.inside.frm, padx = 10)
            deci3.rbtn <- tkradiobutton(d3.frm, text = title.rankstat, value = 3,
                variable = decision, command = decision.fnc)
            deci3 <- tkentry(d3.frm, textvariable = PPval, width = 8,
                justify = "center", state = "disabled")
            tkpack(deci3.rbtn, side = "top", anchor = "w")
            tkpack(deci3, side = "top")
        tkpack(d3.frm, side = "left", expand = TRUE)
        
      tkpack(deci.left.inside.frm, anchor = "center", expand = TRUE)
    tkpack(deci.left.frm, side = "left", fill = "both", expand = TRUE)
    
    if (as.character(Choice) != "RFET") {
      d3bis.frm <- tkframe(decision.frm, relief = "groove", bd = 2)
        d3bis.inside.frm <- tkframe(d3bis.frm)
          rankstat.lbl <- tklabel(d3bis.inside.frm,
              text = "Ranking statistic :")
          rankstat1.frm <- tkframe(d3bis.inside.frm, padx = 80)
          rankstat2.frm <- tkframe(d3bis.inside.frm, padx = 80)
          if (as.character(Choice) == "GPS" | as.character(Choice) == "BCPNN")
            rankstat1.rbtn <- tkradiobutton(rankstat1.frm, text = rankstat1.title,
                value = 1, variable = rankstat)
          if (as.character(Choice) == "ROR" | as.character(Choice) == "PRR")
            rankstat1.rbtn <- tkradiobutton(rankstat1.frm, text = rankstat1.title,
                value = 1, variable = rankstat, command = rankstat.fnc)
          if (as.character(Choice) == "GPS" | as.character(Choice) == "BCPNN")
            rankstat2.rbtn <- tkradiobutton(rankstat2.frm, text = rankstat2.title,
                value = 2, variable = rankstat)
          if (as.character(Choice) == "ROR" | as.character(Choice) == "PRR")
            rankstat2.rbtn <- tkradiobutton(rankstat2.frm, text = rankstat2.title,
                value = 2, variable = rankstat, command = rankstat.fnc)
          tkpack(rankstat.lbl, anchor = "center")
          tkpack(rankstat1.rbtn, side = "left")
          tkpack(rankstat1.frm, fill = "x", expand = TRUE)
          tkpack(rankstat2.rbtn, side = "left")
          tkpack(rankstat2.frm, fill = "x", expand = TRUE)
          if (as.character(Choice) == "GPS") {
            rankstat3.frm <- tkframe(d3bis.inside.frm, padx = 80)
            rankstat3.rbtn <- tkradiobutton(rankstat3.frm, text = "post E(Lambda)",
                value = 3, variable = rankstat)
            tkpack(rankstat3.rbtn, side = "left")
            tkpack(rankstat3.frm, fill = "x", expand = TRUE)
          }
        tkpack (d3bis.inside.frm, anchor = "center", expand = TRUE)
      tkpack (d3bis.frm, side = "left", fill = "both", expand = TRUE)
    }
  
  tkpack(decision.frm, fill = "both", expand = TRUE)
  
  ############################### ADVANCED OPTIONS ###############################
  if (as.character(Choice) == "GPS" | as.character(Choice) == "BCPNN" | as.character(Choice) == "RFET") {
  AdvancedOption.frm <- tkframe (toption.frm, relief = "groove", bd= 6)
  AdvancedOption.title <- tklabel(AdvancedOption.frm, text = "ADVANCED OPTION",
      font = "Helvetica 12")
  tkpack(AdvancedOption.title, side = "top")
    if (as.character(Choice) == "GPS") {
      marge.frm <- tkframe (AdvancedOption.frm, pady = 10)
        margebis.frm <- tkframe (marge.frm)
        tronc.lbl <- tklabel(margebis.frm, text = "Do you want to truncate your database for the hyper parameter calculation ?",
            anchor = "e")
        tronc0.btn <- tkradiobutton(margebis.frm, text = "No", value = 0,
            variable = tronc, command = tronc.fnc)
        tronc1.btn <- tkradiobutton(margebis.frm, text = "Yes", value = 1,
            variable = tronc, command = tronc.fnc)
        marge.ety <- tkentry(margebis.frm, textvariable = marge, width = 5,
            justify = "center", state = "disabled")
        tkpack(tronc.lbl, side = "left")
        tkpack(tronc0.btn, side = "left")
        tkpack(tronc1.btn, side = "left")
        tkpack(tklabel(margebis.frm, text = "Enter your value :"), side = "left")
        tkpack(marge.ety, side = "left")
        tkpack(margebis.frm, anchor = "center", expand = TRUE)
      tkpack(marge.frm, anchor = "center", expand = TRUE)
  
      hyper.frm <- tkframe (AdvancedOption.frm, relief = "groove", bd = 2)
        hyper1.frm <- tkframe (hyper.frm, pady = 5)
          hyper1.btn <- tkradiobutton(hyper1.frm, text =
              "You can choose to change the prior parameters initialization...",
              value = 1, variable = hyper, command = hyper.fnc)
          alpha1.lbl <- tklabel(hyper1.frm, text = "init(alpha1)", anchor = "e")
          beta1.lbl <- tklabel(hyper1.frm, text = "init(beta1)", anchor = "e")
          alpha2.lbl <- tklabel(hyper1.frm, text = "init(alpha2)", anchor = "e")
          beta2.lbl <- tklabel(hyper1.frm, text = "init(beta2)", anchor = "e")
          q.lbl <- tklabel(hyper1.frm, text = "init(w)", anchor = "e")
          alpha1.ety <- tkentry(hyper1.frm, textvariable = alpha1.init, width = 5,
              justify = "center", state = "normal")
          beta1.ety <- tkentry(hyper1.frm, textvariable = beta1.init, width = 5,
              justify = "center", state = "normal")
          alpha2.ety <- tkentry(hyper1.frm, textvariable = alpha2.init, width = 5,
              justify = "center", state = "normal")
          beta2.ety <- tkentry(hyper1.frm, textvariable = beta2.init, width = 5,
              justify = "center", state = "normal")
          q.ety <- tkentry(hyper1.frm, textvariable = q.init, width = 5,
              justify = "center", state = "normal")
          tkpack(hyper1.btn, side = "top")
          tkpack(alpha1.lbl, side = "left")
          tkpack(alpha1.ety, side = "left")
          tkpack(beta1.lbl, side = "left")
          tkpack(beta1.ety, side = "left")
          tkpack(alpha2.lbl, side = "left")
          tkpack(alpha2.ety, side = "left")
          tkpack(beta2.lbl, side = "left")
          tkpack(beta2.ety, side = "left")
          tkpack(q.lbl, side = "left")
          tkpack(q.ety, side = "left")
        tkpack(hyper1.frm, side = "top", expand = TRUE)
        hyper2.frm <- tkframe (hyper.frm, pady = 5)
          hyper2.btn <- tkradiobutton(hyper2.frm, text = "... or to enter the final values of the prior parameters",
              value = 2, variable = hyper,
              command = hyper.fnc)
          alpha1.out.lbl <- tklabel(hyper2.frm, text = "alpha1", anchor = "e")
          beta1.out.lbl <- tklabel(hyper2.frm, text = "beta1", anchor = "e")
          alpha2.out.lbl <- tklabel(hyper2.frm, text = "alpha2", anchor = "e")
          beta2.out.lbl <- tklabel(hyper2.frm, text = "beta2", anchor = "e")
          q.out.lbl <- tklabel(hyper2.frm, text = "w", anchor = "e")
          alpha1.out.ety <- tkentry(hyper2.frm, textvariable = alpha1.out,
              width = 10, justify = "center", state = "disabled")
          beta1.out.ety <- tkentry(hyper2.frm, textvariable = beta1.out,
              width = 10, justify = "center", state = "disabled")
          alpha2.out.ety <- tkentry(hyper2.frm, textvariable = alpha2.out,
              width = 10, justify = "center", state = "disabled")
          beta2.out.ety <- tkentry(hyper2.frm, textvariable = beta2.out,
              width = 10, justify = "center", state = "disabled")
          q.out.ety <- tkentry(hyper2.frm, textvariable = q.out, width = 10,
              justify = "center", state = "disabled")
          tkpack(hyper2.btn, side = "top")
          tkpack(alpha1.out.lbl, side = "left")
          tkpack(alpha1.out.ety, side = "left")
          tkpack(beta1.out.lbl, side = "left")
          tkpack(beta1.out.ety, side = "left")
          tkpack(alpha2.out.lbl, side = "left")
          tkpack(alpha2.out.ety, side = "left")
          tkpack(beta2.out.lbl, side = "left")
          tkpack(beta2.out.ety, side = "left")
          tkpack(q.out.lbl, side = "left")
          tkpack(q.out.ety, side = "left")
        tkpack(hyper2.frm, side = "top", expand = TRUE)
    tkpack(hyper.frm, fill = "none", expand = TRUE)
    }
    if (as.character(Choice) == "BCPNN") {
      montecarlo.frm <- tkframe (AdvancedOption.frm, pady = 10)
        montecarlo.top.frm <- tkframe (montecarlo.frm)
          montecarlo.lbl <- tklabel(montecarlo.top.frm, text = "Monte Carlo simulations (It can be very long) ?",
              anchor = "e")
          montecarlo1.btn <- tkradiobutton(montecarlo.top.frm, text = "No", value = 1,
            variable = MCMC, command = monteCarlo.fnc)
          montecarlo2.btn <- tkradiobutton(montecarlo.top.frm, text = "Yes", value = 2,
            variable = MCMC, command = monteCarlo.fnc)
          tkpack(montecarlo.lbl, side = "left")
          tkpack(montecarlo1.btn, side = "left")
          tkpack(montecarlo2.btn, side = "left")
        tkpack(montecarlo.top.frm, anchor = "center", expand = TRUE)
        montecarlo.down.frm <- tkframe (montecarlo.frm)
          nb.monteCarlo.ety <- tkentry(montecarlo.down.frm, textvariable = nb.monteCarlo,
              width = 10, justify = "center", state = "disabled")
          tkpack(tklabel(montecarlo.down.frm, text = "If yes, enter the number of MC simulations"), side = "left")
          tkpack(nb.monteCarlo.ety, side = "left")
        tkpack(montecarlo.down.frm, anchor = "center", expand = TRUE)
      tkpack(montecarlo.frm, fill = "both", expand = TRUE)
    }
    if (as.character(Choice) == "RFET") {
      midpval.frm <- tkframe (AdvancedOption.frm, pady = 10)
        midpvalbis.frm <- tkframe (midpval.frm)
          midpval.lbl <- tklabel(midpvalbis.frm, text = "Check the following box if you want to use mid-Pvalues (recommended)",
              anchor = "e")
          midpval <- tkcheckbutton(midpvalbis.frm)
          midpval.val <- tclVar("0")
          tkconfigure(midpval, variable = midpval.val)
          tkpack(midpval.lbl, side = "left")
          tkpack(midpval, side = "left")
        tkpack(midpvalbis.frm, anchor = "center", expand = TRUE)
      tkpack(midpval.frm, fill = "both", expand = TRUE)
    }
  tkpack(AdvancedOption.frm, fill = "both", expand = TRUE)
  }
  
  
  ############################### COMPUTE BUTTON #################################
  compute.frm <- tkframe(toption.frm, relief = "groove", bd= 6)
    compute.title <- tklabel (compute.frm, text = "COMPUTE METHOD", font = "Helvetica 12")
    tkpack (compute.title, side = "top")
    execute.frm <- tkframe(compute.frm, pady = 5)
      execute.btn <- tkbutton(execute.frm, text = "Execute", font = "Helvetica 10", command = execute.fnc)
      tkpack(execute.btn, side = "right")
    tkpack(execute.frm, side = "left", fill = "both", expand = TRUE)
    SaveOutput.frm <- tkframe(compute.frm, pady = 5)
      SaveOutput.btn <- tkbutton(SaveOutput.frm, text = "Save Output", font = "Helvetica 10", command = SaveOutput.fnc)
      tkpack(SaveOutput.btn, side = "left")
    tkpack(SaveOutput.frm, side = "left", fill = "both", expand = TRUE)
    # remove the help button
    #help.frm <- tkframe(compute.frm, pady = 5)
    #  help.but <- tkbutton(help.frm,text="HELP", font = "Helvetica 10", command = Help)
    #  tkpack(help.but)
    #tkpack(help.frm, side = "left", fill = "both", expand = TRUE)
  tkpack(compute.frm, fill = "both", expand = TRUE)
  
  
  ############################### GRAPHICS OPTIONS ###############################
  graphics.frm <- tkframe (toption.frm, relief = "groove", bd= 6)
  graphics.title <- tklabel(graphics.frm, text = "GRAPHICS OPTION", font = "Helvetica 12")
  tkpack(graphics.title, side = "top")
  graphics.left.frm <- tkframe(graphics.frm)
  
    fdr.frm <- tkframe(graphics.left.frm, padx = 10)
        FDR.lbl <- tklabel(fdr.frm, text = text.FDR, anchor = "e")
        FDR <- tkcheckbutton(fdr.frm)
        FDR.val <- tclVar("0")
        tkconfigure(FDR, variable = FDR.val)
        tkpack(FDR, side = "left")
        tkpack(FDR.lbl, side = "left")
    tkpack(fdr.frm, fill = "both", expand = TRUE)
  
  if (as.character(Choice) == "GPS" | as.character(Choice) == "BCPNN") {
    sensi.frm <- tkframe(graphics.left.frm, padx = 10)
        SENSI.lbl <- tklabel(sensi.frm, text = "Sensibility & Specificity", anchor = "e")
        SENSI <- tkcheckbutton(sensi.frm)
        SENSI.val <- tclVar("0")
        tkconfigure(SENSI, variable = SENSI.val)
        tkpack(SENSI, side = "left")
        tkpack(SENSI.lbl, side = "left")
    tkpack(sensi.frm, fill = "both", expand = TRUE)
  
    roc.frm <- tkframe(graphics.left.frm, padx = 10)
        ROC.lbl <- tklabel(roc.frm, text = "ROC Curve", anchor = "e")
        ROC <- tkcheckbutton(roc.frm)
        ROC.val <- tclVar("0")
        tkconfigure(ROC, variable = ROC.val)
        tkpack(ROC, side = "left")
        tkpack(ROC.lbl, side = "left")
    tkpack(roc.frm, fill = "both", expand = TRUE)
  }
  
    statint.frm <- tkframe(graphics.left.frm, padx = 10)
        statint.lbl <- tklabel(statint.frm, text = "Graphical representation of the Statistic of interest according to the number of notifications", anchor = "e")
        statint <- tkcheckbutton(statint.frm)
        statint.val <- tclVar("0")
        tkconfigure(statint, variable = statint.val)
        tkpack(statint, side = "left")
        tkpack(statint.lbl, side = "left")
    tkpack(statint.frm, fill = "both", expand = TRUE)
  tkpack(graphics.left.frm, side = "left", anchor = "w", expand = TRUE)
  
  graphics.right.frm <- tkframe(graphics.frm)
    plot.frm <- tkframe(graphics.right.frm, padx = 10)
        plot.btn <- tkbutton(plot.frm, text = "Plot", font = "Helvetica 10", command = plot.fnc)
        tkpack(plot.btn, side = "right")
    tkpack(plot.frm, fill = "both", expand = TRUE)
    
  tkpack(graphics.right.frm, side = "left", anchor = "center", expand = TRUE)
  
  tkpack(graphics.frm, fill = "both", expand = TRUE)
  
  ################################# RESEARCH #####################################
  research.frm <- tkframe(toption.frm, relief = "groove", bd = 6)
  research.title <- tklabel(research.frm, text = "RESEARCH OPTION", font = "Helvetica 12")
  tkpack(research.title, side = "top")
  
  #tkpack(tklabel(research.frm,
  #  text = paste("You may want to know the rank of a particular notification. Enter in the following boxes the drug and event\n",
  #  "codes as it appear in the base. General statistics (as FDR, rank, expected number...) of the choosen notification(s) will be showed.\n",
  #  "\n If you choose to enter only the drug code, you will generate the General statistics for all notifications where the drug code appears.\n",
  #  "Reciprocity is true, if you enter only the event code you will generate the list of the drugs associated with this event.")
  #  ))
  tkpack(tklabel(research.frm,
    text = paste("Enter the label of a drug, an event or both")
    ))
  
  left.frm <- tkframe(research.frm)
    research.inside.frm <- tkframe(left.frm)
      drug.frm <- tkframe(research.inside.frm, padx = 50, pady = 2)
        drug.lbl <- tklabel(drug.frm, text = "DRUG")
        drug <- tkentry(drug.frm, textvariable = DRUGval, width = 10,
            justify = "center", state = "normal")
        tkpack(drug.lbl, side = "top", anchor = "center")
        tkpack(drug, side = "top")
      tkpack(drug.frm, side = "left", expand = TRUE)
    
      event.frm <- tkframe(research.inside.frm, padx = 50, pady = 2)
        event.lbl <- tklabel(event.frm, text = "EVENT")
        event <- tkentry(event.frm, textvariable = EVENTval, width = 10,
            justify = "center", state = "normal")
        tkpack(event.lbl, side = "top", anchor = "center")
        tkpack(event, side = "top")
      tkpack(event.frm, side = "left", expand = TRUE)
    
    tkpack(research.inside.frm, expand = TRUE)
  tkpack(left.frm, side = "left", expand = TRUE)
  
  right.frm <- tkframe(research.frm)
    right.inside.frm <- tkframe(right.frm)
      search.frm <- tkframe(right.inside.frm)
        search.btn <- tkbutton(search.frm, text = "Search", font = "Helvetica 10", command = search.fnc)
        tkpack(search.btn, side = "right")
      tkpack(search.frm, side = "left", fill = "both", expand = TRUE)
      
      Savefindings.frm <- tkframe(right.inside.frm)
        Savefindings.btn <- tkbutton(Savefindings.frm, text="Save findings", font="Helvetica 10", command=Savefindings.fnc)
        tkpack(Savefindings.btn, side = "left")
      tkpack(Savefindings.frm, side = "left", fill = "both", expand = TRUE)
  
    tkpack(right.inside.frm, anchor = "e", expand = TRUE)
  tkpack(right.frm, side = "right", expand = TRUE)
  
      
  tkpack(research.frm, fill = "both", expand = TRUE)
  
  ############################### ERRORS WINDOW ##################################
  message.frm <- tkframe(toption.frm, relief = "raised", bd = 2)
  message.txt <- tktext(message.frm, bg = "white", font = "Helvetica 10", height = 8, width = 5)
  message.scr <- tkscrollbar(message.frm, command = function(...) tkyview(message.txt,...))
  tkconfigure(message.txt, yscrollcommand = function(...) tkset(message.scr,...))
  tkpack(message.txt, side = "left", fill = "x", expand = TRUE)
  tkpack(message.scr, side = "right", fill = "y")
  tkpack(message.frm, fill = "x")

 } # fin du else

 tkpack(toption.frm)
 tkwm.focusmodel(toption, "active")
}


################################################################################
# corps du programme

tt<-tktoplevel()
tkwm.title(tt,"Pharmacovigilance")
inFileName.var <- tclVar("")
Marge.var <- tclVar("1")
titlefont <- "Helvetica 12"
normalfont <- "Helvetica 10"
Choice <- NULL
DATA <- NULL
inFileName.var <- tclVar("")

findFile <- function() {
    tclvalue(inFileName.var) <- tclvalue(tkgetOpenFile())
}
  
readFile <- function() {
    filename <- tclvalue(inFileName.var)
    s.marge <- as.numeric(tclvalue(Marge.var))
    if (filename == "") {
        tkmessageBox(title="Error",message="ERROR: No file selected.\n")
        }
    else {
	DATA.FRAME <- read.table(file=filename,header=TRUE,sep=";",colClasses=c("factor","factor","double"))
	RES <- as.PhViD(DATA.FRAME,MARGIN.THRES=s.marge)
        #RES <- csv2PhViD(FILE=filename,MARGIN.THRES=s.marge)           
        if (is.null(RES) == FALSE) {
            assign("DATA", RES, inherits=FALSE, envir = .GlobalEnv)
        }
    }
}

    top.frm <- tkframe(tt, borderwidth = 20)
    pharmaco.frm <- tkframe(top.frm, relief = "raised", bd = 1)
        tkpack(tklabel(pharmaco.frm, text = "Read notifications:"), anchor = "w")

        readdata.frm <- tkframe(pharmaco.frm, relief = "groove", bd = 4)
            inFileName.frm <- tkframe(readdata.frm)
                inFileName.lbl <- tklabel(inFileName.frm, text = "File Name:")
                inFileName.ety <- tkentry(inFileName.frm, textvariable = inFileName.var, justify = "center")
                tkpack(inFileName.lbl, side = "left")
                tkpack(inFileName.ety, side = "right", fill = "x", expand = TRUE)
            tkpack(inFileName.frm, fill = "x", expand = TRUE)
            Marge.frm <- tkframe(readdata.frm)
                Marge.lbl <- tklabel(Marge.frm, text = "Margin threshold:")
                Marge.ety <- tkentry(Marge.frm, textvariable = Marge.var, width = 5, justify = "center")
                tkpack(Marge.lbl, side = "left")
                tkpack(Marge.ety, side = "left")
            tkpack(Marge.frm, fill = "x", expand = TRUE)
            pButtons.frm <- tkframe(readdata.frm)
                browse.but <- tkbutton(pButtons.frm, text = "Browse", command = findFile)
                load.but <- tkbutton(pButtons.frm, text = "Load", command = readFile)
                tkgrid(browse.but, load.but)
            tkpack(pButtons.frm, anchor = "e")
            #     #     #  -->  fin importation  <--  #     #     #     #

          method.frm <- tkframe(readdata.frm)
            Method.but <- tkbutton(method.frm,text="  Method  ", command = Method)
            tkgrid(Method.but)
          tkpack(method.frm, anchor = "center")
          
        tkpack(readdata.frm, fill = "x")

    tkpack(pharmaco.frm, fill = "x")
    tkpack(top.frm, fill = "x")
}

