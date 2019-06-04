library(shiny)
library(SEER2R)
library(flexsurvcure)
library(data.table)
options(shiny.maxRequestSize=40*1024^2)

ui <- fluidPage(
  titlePanel("Recurrence Risk"),
  navbarPage("Data Type",
             tabPanel("Group Data",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "SEER*Stat Dic File:",
                                    accept = c(".dic")
                          ),
      
                          fileInput("file2", "SEER*Stat Data File:",
                                    accept = c(".txt")
                          ), 
                          
                          
                          fileInput("file3", "CanSurv CSV File:",
                                    accept = c(".csv")
                          ),
                          
                          selectInput("stage.var", "Stage Variable:", choices = ""),
                          selectInput("stage.dist.value", "Distant Stage Value:",choices = ""),
                          numericInput("r", "Adjustment Factor r:", value=1, min = 1, max = 2, step = 0.05),

                          sliderInput("fup.value","Years of Follow-up:",min = 1,max = 30,value = 25),
                          actionButton("action", "Submit"),
                          tags$hr(),
                          downloadButton("downloadtable", "Download results")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Output",
                                     dataTableOutput("contents")
                            ),
                            tabPanel("Help",
                                     br(),
                                     p("This web application has been developed to estimate the risk of progressing to distant recurrence using disease-specific survival typically provided by cancer registries. 
                     The disease-specific survival is assessed via cause-specific survival using SEER*Stat software. 
                     The cause-specific survival is assumed to follow a mixture-cure model and the risk of recurrence is inferred from the survival among the non-cured fraction. 
                     The cure fraction and parametric survival distribution among those not cured are estimated using CanSurv software. 
                     The current version can handle Weibull and log-logistic distributions for the non-cured survival."),
                                     br(),
                                     HTML("<b>Input</b>
                        <ul><li> <b>SEER*Stat Dic File</b>: the dictionary file exported from SEER*Stat software with .dic extension which contains the information describing the layout of the export data file.
                       </li><li> <b>SEER*Stat Data File</b>: the cause-specific survival data generated from SEER*Stat in .txt format.
                       </li><li> <b>CanSurv CSV File</b>: the CSV format output from CanSurv software including information on strata/covariates and estimated parameters for the mixture cure survival model.
                       </li><li> <b>Stage Variable</b>: the stage variable defined in SEER*Stat data. All variable names in the data set will be listed after uploading the SEER*Stat files. If there are more than 1 stage variable, the user will need to select the one which contains the distant stage.
                       </li><li> <b>Distant Stage Value</b>: the user will need to select the numeric value of distant stage from the listed values of Stage Variable.
                       </li><li> <b>Adjustment Factor r</b>: the factor used to adjust the registry-based survival curves for sensitivity analysis. The user may click the up and down arrows to change the value or type in any value. The default value is 1.
                       </li><li> <b>Years of Follow-up</b>: the range of follow-up years in the output. The default number is 25. If the maximum number of follow-up years (max.num.year) in the SEER*Stat data is less than 25, then the default number will be updated to the max.num.year.
                       </li></ul>"),
                                     HTML("<b>Output</b>
                        <ul><li> <b>link</b>: the parametric survival distribution among those not cured specified in CanSurv.
                       </li><li> <b>cure</b>: the cure fraction estimated from the mixture cure survival model.
                       </li><li> <b>lambda/k</b>: the estimated parameters of the survival distribution for those not cured.
                       </li><li> <b>theta</b>: the exponential hazard of the time from recurrence to cancer death.
                       </li><li> <b>surv_curemodel</b>: the survival estimated from the mixture cure survival model.
                       </li><li> <b>surv_notcure</b>: the estimated survival for the non-cured fraction.
                       </li><li> <b>median_surv_notcured</b>: the median survival time for the non-cured fraction.
                       </li><li> <b>s1_numerical</b>: the numerical estimated survival to recurrence (recurrence-free survival) for the non-cured fraction.
                       </li><li> <b>G_numerical</b>: the numerical estimated survival to recurrence.
                       </li><li> <b>CI_numerical</b>: 1-G_numerical, the numerical estimated cumulative incidence of recurrence which is the probability of progressing to cancer recurrence.
                       </li><li> <b>s1_analytical</b>: the analytical estimated survival to recurrence (recurrence-free survival) for the non-cured fraction.
                       </li><li> <b>G_analytical</b>: the analytical estimated survival to recurrence.
                       </li><li> <b>CI_analytical</b>: 1- G_analytical, the analytical estimated cumulative incidence of recurrence.
                       </li><li> <b>se_CI_analytical</b>: the standard error of CI_analytical.
                       </li><li> <b>obs_surv</b>: the observed survival from SEER*Stat.
                       </li><li> <b>obs_dist_surv</b>: the observed survival for distant stage from SEER*Stat.
                       </li></ul>"),
                                     div(HTML("<br>Example available at <a href='https://github.com/recurrencerisk/recurrence/blob/example/example.zip' target='_blank' download='shinyapp_recurrence_example'>shinyapp_recurrence_example</a>.")),
                                     br(),
                                     br(),
                                     p("References"),
                                     list(
                                       h5("1. Mariotto AB, Zou Z, Zhang F, Howlader N, Kurian AW, Etzioni R. Can We Use Survival Data from Cancer Registries to Learn about Disease Recurrence? The Case of Breast Cancer. Cancer Epidemiol Biomarkers Prev. 2018 Nov; 27(11):1332-1341."),
                                       h5("2. Yu B, Tiwari RC, Cronin KA, McDonald C, Feuer EJ. CANSURV: A Windows program for population-based cancer survival analysis. Comput.Methods Programs Biomed. 2005;80(3):195-203."),
                                       h5("3. Yu B, Tiwari RC, Cronin KA, Feuer EJ. Cure fraction estimation from the mixture cure models for grouped survival data. Stat.Med. 2004;23(11):1733-47."),
                                       h5("4. De Angelis R, Capocaccia R, Hakulinen T, Soderman B, Verdecchia A. Mixture models for cancer survival analysis: application to population-based data with covariates. Stat.Med. 1999;18(4):441-54."),
                                       h5("5. Capocaccia R. Relationships between incidence and mortality in non-reversible diseases. Stat Med. 1993;12(24):2395-415."),
                                       h5("6. Li CS, Taylor JMG, Sy JP. Identifiability of cure models. Statistics & Probability Letters. 2001;54(4):389-95."),
                                       h5("7. Howlader N, Ries LA, Mariotto AB, Reichman ME, Ruhl J, Cronin KA. Improved estimates of cancer-specific survival rates from population-based data. J Natl Cancer Inst. 2010;102(20):1584-98.")
                                     )
                                     
                            )
                          )
                        )
                      )
             ),
             tabPanel("Individual Data",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1.tab2", "CSV Data File:",
                                    accept = c(".csv")
                          ),
                          
                          selectInput("strata.tab2", "Strata:", choices = "",multiple = TRUE),
                          selectInput("covar.tab2", "Covariates:", choices = "",multiple = TRUE),
                          selectInput("time.tab2", "Time Variable:", choices = ""),
                          selectInput("event.tab2", "Event Variable:", choices = ""),
                          selectInput("dist.tab2", "Distribution:",choices = c("Log-logistic","Weibull")),
                          selectInput("stage.var.tab2", "Stage Variable:", choices = ""),
                          selectInput("stage.dist.value.tab2", "Distant Stage Value:",choices = ""),
                          numericInput("r.tab2", "Adjustment Factor r:", value=1, min = 1, max = 2, step = 0.05),
                          sliderInput("fup.value.tab2","Years of Follow-up:",min = 1,max = 30,value = 25),
                          actionButton("action.tab2", "Submit"),
                          tags$hr(),
                          downloadButton("downloadtable.tab2", "Download results")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Output",
                                     dataTableOutput("contents.tab2")
                            ),
                            tabPanel("Help",
                                     br(),
                                     p("This web application has been extended to estimate the risk of progressing to distant recurrence using individual survival data. 
                                       The cause-specific survival is assumed to follow a mixture-cure model and the risk of recurrence is inferred from the survival among the non-cured fraction. 
                                       The cure fraction and parametric survival distribution among those not cured will be estimated using R flexsurvcure package. 
                                       The current version can handle Weibull and log-logistic distributions for the non-cured survival."),
                                     br(),
                                     HTML("<b>Input</b>
                                          <ul><li> <b>CSV Data File</b>: the case-listing unformatted CSV data file exported from SEER*Stat software with modification of variables of interest or user-generated individual data by other statistical software in the CSV format. Note that, 
                                                                         users may not directly use the original unformatted variables, but need to modify the case-listing SEER*Stat data before uploading because 1) it may not contain the event variable for death due to cancer; 
                                                                         2) there may not be categorical but continuous variables for strata and covariates; 
                                                                         3) the stage/strata/covariates variables are categorical but with many levels or with numerical unknown values, the user will need to recode it to better categorize the group;
                                                                         4) the dead events due to other causes should be recoded as censoring events.
                                          </li><li> <b>Strata</b>: the user-defined strata variables which should be categorical coded as integers.
                                          </li><li> <b>Covariates</b>: the user-defined covariates in the mixture cure survival model which should be categorical coded as integers.
                                          </li><li> <b>Time Variable</b>: the variable specified as follow-up time in the survival model.
                                          </li><li> <b>Event Variable</b>: the status indicator, 0=alive, 1=dead (due to cancer). Note that, dead events due to other causes should be defined as censoring events. 
                                          For example, when an unformatted case-listing SEER*Stat data is used, a patient is dead due to cancer if Vitalstatusrecodestudycutoffus=dead and End_Calc_Vital_Status_Adjusted=dead;
                                          and a patient is dead due to other causes if Vitalstatusrecodestudycutoffus=dead and End_Calc_Vital_Status_Adjusted=(untraced or alive).
                                          </li><li> <b>Distribution</b>: the latency distribution for the cure model (non-cured survival). The current version can handle Weibull and log-logistic distributions. 
                                          </li><li> <b>Stage Variable</b>: the stage variable defined in SEER*Stat data or recoded by the user, which should be the same variable selected in Strata if it is defined as a stratum.
                                          All variable names in the data set will be listed after uploading the SEER*Stat files. If there are more than 1 stage variable, the user will need to select the one which contains the distant stage.
                                          </li><li> <b>Distant Stage Value</b>: the user will need to select the numeric value of distant stage from the listed values of Stage Variable.
                                          </li><li> <b>Adjustment Factor r</b>: the factor used to adjust the registry-based survival curves for sensitivity analysis. The user may click the up and down arrows to change the value or type in any value. The default value is 1.
                                          </li><li> <b>Years of Follow-up</b>: the range of follow-up years in the output. The default number is 25. If the maximum number of follow-up years (max.num.year) in the SEER*Stat data is less than 25, then the default number will be updated to the max.num.year.
                                          </li></ul>"),
                                     HTML("<b>Output</b>
                                          <ul><li> <b>link</b>: the parametric survival distribution among those not cured specified in CanSurv.
                                          </li><li> <b>cure</b>: the cure fraction estimated from the mixture cure survival model.
                                          </li><li> <b>lambda/k</b>: the estimated parameters of the survival distribution for those not cured.
                                          </li><li> <b>theta</b>: the exponential hazard of the time from recurrence to cancer death.
                                          </li><li> <b>surv_curemodel</b>: the survival estimated from the mixture cure survival model.
                                          </li><li> <b>surv_notcure</b>: the estimated survival for the non-cured fraction.
                                          </li><li> <b>median_surv_notcured</b>: the median survival time for the non-cured fraction.
                                          </li><li> <b>s1_numerical</b>: the numerical estimated survival to recurrence (recurrence-free survival) for the non-cured fraction.
                                          </li><li> <b>G_numerical</b>: the numerical estimated survival to recurrence.
                                          </li><li> <b>CI_numerical</b>: 1-G_numerical, the numerical estimated cumulative incidence of recurrence which is the probability of progressing to cancer recurrence.
                                          </li><li> <b>s1_analytical</b>: the analytical estimated survival to recurrence (recurrence-free survival) for the non-cured fraction.
                                          </li><li> <b>G_analytical</b>: the analytical estimated survival to recurrence.
                                          </li><li> <b>CI_analytical</b>: 1- G_analytical, the analytical estimated cumulative incidence of recurrence.
                                          </li><li> <b>se_CI_analytical</b>: the standard error of CI_analytical.
                                          </li><li> <b>obs_surv</b>: the observed survival from SEER*Stat.
                                          </li><li> <b>obs_dist_surv</b>: the observed survival for distant stage from SEER*Stat.
                                          </li></ul>"),
                                    # div(HTML("<br>Example available at <a href='https://github.com/recurrencerisk/recurrence/archive/example.zip' target='_blank' download='shinyapp_recurrence_example'>shinyapp_recurrence_example</a>.")),
                                    # br(),
                                     br(),
                                     p("References"),
                                     list(
                                       h5("1. Mariotto AB, Zou Z, Zhang F, Howlader N, Kurian AW, Etzioni R. Can We Use Survival Data from Cancer Registries to Learn about Disease Recurrence? The Case of Breast Cancer. Cancer Epidemiol Biomarkers Prev. 2018 Nov; 27(11):1332-1341."),
                                       h5("2. Yu B, Tiwari RC, Cronin KA, McDonald C, Feuer EJ. CANSURV: A Windows program for population-based cancer survival analysis. Comput.Methods Programs Biomed. 2005;80(3):195-203."),
                                       h5("3. Yu B, Tiwari RC, Cronin KA, Feuer EJ. Cure fraction estimation from the mixture cure models for grouped survival data. Stat.Med. 2004;23(11):1733-47."),
                                       h5("4. De Angelis R, Capocaccia R, Hakulinen T, Soderman B, Verdecchia A. Mixture models for cancer survival analysis: application to population-based data with covariates. Stat.Med. 1999;18(4):441-54."),
                                       h5("5. Capocaccia R. Relationships between incidence and mortality in non-reversible diseases. Stat Med. 1993;12(24):2395-415."),
                                       h5("6. Li CS, Taylor JMG, Sy JP. Identifiability of cure models. Statistics & Probability Letters. 2001;54(4):389-95."),
                                       h5("7. Howlader N, Ries LA, Mariotto AB, Reichman ME, Ruhl J, Cronin KA. Improved estimates of cancer-specific survival rates from population-based data. J Natl Cancer Inst. 2010;102(20):1584-98.")
                                     )
                                     
                            )
                          )
                        )
                        
                      ) 
             )          
  )
)

server <- function(input, output,session) {
  
  
  ### tab 1 group data  
  seerdata<-reactive({
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL) 
    data.seerstat <- read.SeerStat(inFile1$datapath,inFile2$datapath)
    return(data.seerstat)
  })
  
  
  choices.stage = reactive({
    cnames.seer <- colnames(seerdata())
    if(!is.null(cnames.seer)){
      int.pos <- which(cnames.seer=="Interval")
      if("Page_type" %in% cnames.seer){
        pt.pos<-which(cnames.seer=="Page_type")
        cnames.seer.sub<-cnames.seer[(pt.pos+1):(int.pos-1)]
      }
      if(!"Page_type" %in% cnames.seer){
        cnames.seer.sub<-cnames.seer[1:(int.pos-1)]
      }
      return(cnames.seer.sub)
    }
  })
  
  choices.stagevalues = reactive({
    if (input$stage.var == "") return()
    sort(unique(seerdata()[,input$stage.var]))
  })
  
  
  observe({
    updateSelectInput(session, "stage.var", choices = choices.stage())
  })
  
  observe({
    updateSelectInput(session, "stage.dist.value", choices = choices.stagevalues())
  })
  
  max.fup = reactive({
    fup.uni<-unique(seerdata()[,"Interval"])
    if (is.null(fup.uni)) return()
    fup.uni.sort <- sort(fup.uni,decreasing=T,na.last = NA)
    return(fup.uni.sort[1])
  })
  observe({
    updateSliderInput(session, "fup.value", value=25,min=1,max=max.fup())
  })
  
  
  risk.out<-reactive({  
    inFile3 <- input$file3
    if (is.null(inFile3))
      return(NULL)
    csdata <- read.csv(inFile3$datapath,stringsAsFactors=F,check.names=F)
    cnames.seer <- colnames(seerdata())
    cnames.cs <- colnames(csdata)
    
    validate(
      need(max(nchar(cnames.seer))<=32, "Warning: SEER*Stat data contains a variable with name exceeding 32 characters which may cause problems in CanSurv output.")
    ) 
    validate(
      need(is.numeric(input$r)== T, "Warning: Adjustment Factor r should be numeric.")
    )
    
    RR <- as.numeric(input$r)
    interval.max <- as.numeric(input$fup.value)
    stage.dist.name <- input$stage.var 
    stage.dist.value <- as.numeric(input$stage.dist.value)    
    
    surv.name <- cnames.seer[which(grepl("Survival", cnames.seer)==T & grepl("Cum", cnames.seer)==T)]
    survse.name <- cnames.seer[which(grepl("SE", cnames.seer)==T & grepl("Cum", cnames.seer)==T)]
    int.max.seer <- max(seerdata()$Interval,na.rm=T)
    allvar.seer<-cnames.seer[(which(cnames.seer!="Page_type")[1]):(which(cnames.seer=="Interval")-1)]
    cols.keep.seer<-c(allvar.seer,"Interval",surv.name)
    seerdata.keep <- seerdata()[which(seerdata()[,stage.dist.name]!=stage.dist.value),]   
    seerdata.out <- seerdata.keep[,cols.keep.seer]
    seerdata.dist <- seerdata()[which(seerdata()[,stage.dist.name]==stage.dist.value),]   
    seerdata.dist.out <- seerdata.dist[,cols.keep.seer]
    seerdata.dist.out[,stage.dist.name]<-NULL
    colnames(seerdata.out)[which(colnames(seerdata.out)==surv.name)]<-"obs_surv"
    colnames(seerdata.dist.out)[which(colnames(seerdata.dist.out)==surv.name)]<-"obs_dist_surv"
    
    no.stratum <- 0
    no.covar   <- 0
    no.covar.c <- 0
    no.covar.u <- 0
    no.cure    <- 0
    
    if(!"c_int" %in% cnames.cs){
      no.cure<-1 
    }  
    
    if(no.cure==0){
      cint.pos <- which(substr(cnames.cs,1,5)=="c_int")
    }
    uint.pos <- which(substr(cnames.cs,1,5)=="u_int")
    sint.pos <- which(substr(cnames.cs,1,5)=="s_int")
    
    if(no.cure==1){
      csdata[,"c_int"]<-NA
      csdata<-csdata[,c(1:(uint.pos-1),(sint.pos+1),uint.pos:sint.pos)]
      cnames.cs<-colnames(csdata)
      cint.pos <- which(substr(cnames.cs,1,5)=="c_int")
      uint.pos <- which(substr(cnames.cs,1,5)=="u_int")
      sint.pos <- which(substr(cnames.cs,1,5)=="s_int")
    }
    
    
    if(uint.pos-cint.pos==1){
      no.covar.c<-1
      covar.c<-NA 
    }  
    if(sint.pos-uint.pos==1){
      no.covar.u<-1
      covar.u<-NA 
    }  
    
    if(no.covar.c==1 & no.covar.u==1){
      no.covar<-1
    }
    
    if (cint.pos==3){
      no.stratum<-1
      csdata[,"Stratum_1__NoStratum"]<-0
      csdata<-csdata[,c(1,(sint.pos+1),2:sint.pos)]
      cnames.cs<-colnames(csdata)
      cint.pos <- which(substr(cnames.cs,1,5)=="c_int")
      uint.pos <- which(substr(cnames.cs,1,5)=="u_int")
      sint.pos <- which(substr(cnames.cs,1,5)=="s_int")
    }
    
    
    stratum.pos <- which(substr(cnames.cs,1,7)=="Stratum")
    nstratum <- length(stratum.pos)
    stratum.header <- cnames.cs[stratum.pos]
    stratum.str <- strsplit(stratum.header, "__")
    stratum <- sapply(stratum.str, "[", 2)
    ## stratum.vec  Stratum_1 Stratum_2
    stratum.vec <- sapply(stratum.str, "[", 1)
    
    if(length(which(!stratum %in% cnames.seer))>0){
      stratum<-gsub("[^[:alnum:][:space:]_]", "", stratum)
      stratum<-gsub("__+","_",stratum)
      cnames.cs[stratum.pos]<-paste(stratum.vec,"__",stratum,sep="")
    }
    
    if(no.covar.c==0){
      covar.header.c <- cnames.cs[(cint.pos+1):(uint.pos-1)]
      covar.str.c <- strsplit(covar.header.c, "__")
      covar.c <- unique(sapply(covar.str.c, "[", 2))
      ## covar.vec  covar_1 covar_2
      covar.vec.c <- unique(sapply(covar.str.c, "[", 1))
      covar.vec.c <- gsub("c","c_covar",covar.vec.c)
      ncovar.c <- length(covar.c)
      if(length(which(!covar.c %in% cnames.seer))>0){
        covar.c<-gsub("[^[:alnum:][:space:]_]", "", covar.c) 
        covar.c<-gsub("__+","_", covar.c)
        
        covar.c.prefix <- sapply(covar.str.c, "[", 1)
        covar.c.middle <- sapply(covar.str.c, "[", 2)
        covar.c.suffix <- sapply(covar.str.c, "[", 3)
        covar.c.middle <- gsub("[^[:alnum:][:space:]_]", "", covar.c.middle)
        covar.c.middle <- gsub("__+","_", covar.c.middle)
        cnames.cs[(cint.pos+1):(uint.pos-1)]<-paste(covar.c.prefix,"__",covar.c.middle,"__",covar.c.suffix,sep="")
      }
    }
    
    
    if(no.covar.u==0){
      covar.header.u <- cnames.cs[(uint.pos+1):(sint.pos-1)]
      covar.str.u <- strsplit(covar.header.u, "__")
      covar.u <- unique(sapply(covar.str.u, "[", 2))
      ## covar.vec  covar_1 covar_2
      covar.vec.u <- unique(sapply(covar.str.u, "[", 1))
      covar.vec.u <- gsub("u","u_covar",covar.vec.u)
      ncovar.u <- length(covar.u)
      if(length(which(!covar.u %in% cnames.seer))>0){
        covar.u<-gsub("[^[:alnum:][:space:]_]", "", covar.u) 
        covar.u<-gsub("__+","_", covar.u)
        
        covar.u.prefix <- sapply(covar.str.u, "[", 1)
        covar.u.middle <- sapply(covar.str.u, "[", 2)
        covar.u.suffix <- sapply(covar.str.u, "[", 3)
        covar.u.middle <- gsub("[^[:alnum:][:space:]_]", "", covar.u.middle)
        covar.u.middle <- gsub("__+","_", covar.u.middle)
        cnames.cs[(uint.pos+1):(sint.pos-1)]<-paste(covar.u.prefix,"__",covar.u.middle,"__",covar.u.suffix,sep="")
      }
    }
    
    if(no.covar.c==0 | no.covar.u==0){
      covar  <- union(covar.c,covar.u)
      if(no.covar.c==1){
        covar  <- covar.u
      }
      if(no.covar.u==1){
        covar  <- covar.c
      }
      ncovar <- length(covar)
      covar.vec<-paste("covar_", 1:ncovar, sep="")
    }
    
    cnames.cs.new <- cnames.cs
    for (istratum in 1:nstratum){
      stratum.i <- stratum[istratum]
      stratum.i.str <- paste("__",stratum.i,sep="")
      cnames.cs.new <- gsub(stratum.i.str,"",cnames.cs.new)
      if(no.covar==0){
        for (icovar in 1:ncovar){
          covar.i <- covar[icovar]
          covar.i.str <- paste("__",covar.i,"_",sep="")
          cnames.cs.new <- gsub(covar.i.str,"",cnames.cs.new)
        }
      }
    }
    colnames(csdata) <- cnames.cs.new
    
    stratum.level.nvalue <- rep(NA,nstratum)
    stratum.level <- vector("list", nstratum)
    for (istratum in 1:nstratum){
      stratum.col<-paste("Stratum_",istratum,sep="")
      stratum.level[[istratum]] <- unique(csdata[,stratum.col])
      stratum.level.nvalue[istratum] <- length(stratum.level[[istratum]])
    }
    
    level.combo <- expand.grid(stratum.level)
    nlevel <- dim(level.combo)[1]
    
    ## create stratum.combo
    ## for example, stratum_1.value=1, stratum_2.value=0, then stratum.combo=1_0
    
    x.combo<-function(x){
      paste(x,collapse="_",sep="")
    }
    
    stratum.combo <- rep(NA,dim(csdata)[1])
    stratum.combo <- apply(data.frame(csdata[,stratum.vec]), 1, x.combo)
    
    if(no.cure==0){
      cus.str.percombo<-cnames.cs.new[cint.pos:sint.pos]
    }
    if(no.cure==1){
      cus.str.percombo<-cnames.cs.new[uint.pos:sint.pos]
    }
    cus.str <- rep(cus.str.percombo,nlevel)
    rnames.cs<-paste(stratum.combo,"__",cus.str,sep="")
    
    
    ########################################
    ### if long covar name not in seer var
    if(no.covar==0){
      for(icovar in 1:length(covar)){
        covar.i0 <- covar[icovar]
        covar.i<-covar[icovar]
        if(!covar.i %in% allvar.seer){
          seer.check<-gsub("_","",allvar.seer)
          covar.i<-gsub("_","",covar.i)
          covar.i<-allvar.seer[which(grepl(covar.i,seer.check))]
          covar[icovar]<-covar.i
          if(no.covar.c==0){
            covar.c[which(covar.c==covar.i0)]<-covar.i
          }
          if(no.covar.u==0){
            covar.u[which(covar.u==covar.i0)]<-covar.i
          }
        }
      }
    }
    
    if(no.covar==0 & no.stratum==0){
      covar.group.nvalue <- rep(NA,ncovar)
      covar.group <- vector("list", ncovar)
      for (icovar in 1:ncovar){
        #covar.group[[icovar]] <- unique(seerdata().dist[,covar[icovar]])
        covar.group[[icovar]] <- unique(seerdata()[,covar[icovar]])
        covar.group.nvalue[icovar] <- length(covar.group[[icovar]])
      }
      allvar<-c(stratum,covar)
      allvar.vec<-c(stratum.vec,covar.vec)
    }
    
    
    if(no.stratum==1 & no.covar==0){
      allvar<-covar
      allvar.vec<- covar.vec
    }
    if(no.covar==1){
      allvar<-stratum
      allvar.vec<- stratum.vec
    }
    
    nallvar<-length(allvar)
    allvar.group.nvalue <- rep(NA,nallvar)
    allvar.group <- vector("list",nallvar)
    
    for (iallvar in 1:nallvar){
      if(no.stratum==1 & no.covar==1){
        allvar.group[[iallvar]] <- 0
      }
      if(no.stratum==0 | no.covar==0){
        allvar.group[[iallvar]] <- unique(seerdata()[,allvar[iallvar]])
      }
      allvar.group.nvalue[iallvar] <- length(allvar.group[[iallvar]])
    }
    
    
    nallvar.seer<-length(allvar.seer)
    allvar.seer.group.nvalue <- rep(NA,nallvar.seer)
    allvar.seer.group <- vector("list",nallvar.seer)
    
    for (iallvar.seer in 1:nallvar.seer){
      allvar.seer.group[[iallvar.seer]] <- unique(seerdata()[,allvar.seer[iallvar.seer]])
      allvar.seer.group.nvalue[iallvar.seer] <- length(allvar.seer.group[[iallvar.seer]])
    }
    
    group.combo <- expand.grid(allvar.group)
    ngroup <- dim(group.combo)[1]
    
    seer.group.combo <- expand.grid(allvar.seer.group)
    nseergroup <- dim(seer.group.combo)[1]
    
    ##### estimate exponential distribution parameter ####
    
    theta0 <- 0.1
    theta.sum <- array(NA,dim=c(nseergroup,nallvar.seer+3))
    colnames(theta.sum)<-c(allvar.seer,"r","theta","theta.se")
    
    for (iseergroup in 1:nseergroup){
      seer.group.combo.igroup<-seer.group.combo[iseergroup,]
      seer.group.combo.igroup[which(allvar.seer==stage.dist.name)]<-stage.dist.value
      condition.string<-paste("seerdata.dist$",allvar.seer,"==",seer.group.combo.igroup, collapse=" & ",sep="")    
      data.sub <- eval(parse(text=paste("seerdata.dist[",condition.string,",]")))
      y <- data.sub[,surv.name] 
      x <- data.sub$Interval 
      se <- data.sub[,survse.name]
      fit<-NULL
      if(dim(data.sub)[1]>2 & length(which(!is.na(y)))>2 & length(table(y))>2){
        try(fit <- nls(y~exp(-theta*x),start=list(theta=theta0),weights=1/se^2))
        if(is.null(fit)){
          errorgroup.str<-paste(allvar.seer,"==",seer.group.combo.igroup, collapse=" & ",sep="")    
          error.str<-"nls function error in qr.default(.swts * gr): NA/NaN/Inf in foreign function call (arg 1). Theta cannot be estimated. NA returned for the below subgroup: "
          error.print<-paste(error.str,errorgroup.str,sep="")
          print(error.print)
        }
        if(!is.null(fit)){
          theta <- summary(fit)$coefficients[1,1]
          theta.se <- summary(fit)$coefficients[1,2]
          theta.sum[iseergroup,] <- c(as.numeric(seer.group.combo[iseergroup,]),RR,RR*theta,theta.se)
        }
      }
    }
    
    theta.sum<-data.frame(theta.sum)
    theta.sum[,"allvar.seer.combo"] <- NA
    theta.sum$allvar.seer.combo <- apply(data.frame(theta.sum[,allvar.seer]), 1, x.combo)
    
    
    ##### create data frame for output
    intervals<-list(1:int.max.seer)
    out.combo.list <- append(intervals,allvar.group) 
    out.combo <- expand.grid(out.combo.list)
    colnames(out.combo)<-c("fup",allvar.vec)
    
    seer.out.combo.list <- append(intervals,allvar.seer.group) 
    seer.out.combo <- expand.grid(seer.out.combo.list)
    colnames(seer.out.combo)<-c("Interval",allvar.seer)
    seer.out.combo<-seer.out.combo[which(seer.out.combo[,stage.dist.name]!=stage.dist.value),]
    
    if(no.stratum==1){
      out.combo[,stratum.vec] <- 0
    }
    
    if(no.covar==1){
      out.combo[,c("Link","allvar.combo","stratum.combo","c_int","u_int","s_int","cure","lambda","k")] <- NA
    }
    
    if(no.covar==0){
      if(no.covar.c==0 & no.covar.u==1){
        out.combo[,c("Link","allvar.combo","stratum.combo","covar.combo","c_int",covar.vec.c,"u_int","s_int","cure","lambda","k")] <- NA
      }
      if(no.covar.c==1 & no.covar.u==0){
        out.combo[,c("Link","allvar.combo","stratum.combo","covar.combo","c_int","u_int",covar.vec.u,"s_int","cure","lambda","k")] <- NA
      }
      if(no.covar.c==0 & no.covar.u==0){  
        out.combo[,c("Link","allvar.combo","stratum.combo","covar.combo","c_int",covar.vec.c,"u_int",covar.vec.u,"s_int","cure","lambda","k")] <- NA
      }
    }
    
    out.combo$Link <- csdata$Link[1]
    ## create matrix for estimates 
    est.matrix <- csdata[,c(2:(cint.pos-1))]
    est.matrix[,"stratum.combo"] <- stratum.combo
    est.matrix[,"rnames.cs"] <- rnames.cs
    
    
    out.combo$stratum.combo <- apply(data.frame(out.combo[,stratum.vec]), 1, x.combo)
    out.combo$allvar.combo <- apply(data.frame(out.combo[,allvar.vec]), 1, x.combo)
    
    
    #### continued 02/05/9018 ####
    for (krow in 1:dim(out.combo)[1]){
      
      start.row <- match(out.combo$stratum.combo[krow],est.matrix$stratum.combo)
      if(no.cure==0){
        cus.vec <- est.matrix$estimate[start.row:(start.row+sint.pos-cint.pos)]
      }
      if(no.cure==1){
        cus.vec <- est.matrix$estimate[start.row:(start.row+sint.pos-uint.pos)]
        cus.vec <- c(0,cus.vec)
      }
      cus.order <- cnames.cs.new[cint.pos:sint.pos]
      out.combo$c_int[krow] <- cus.vec[which(cus.order=="c_int")]
      out.combo$u_int[krow] <- cus.vec[which(cus.order=="u_int")]
      out.combo$s_int[krow] <- cus.vec[which(cus.order=="s_int")]
      
      c.cols <- c("c_int")
      u.cols <- c("u_int")  
      if(no.covar==0){
        out.combo$covar.combo[krow] <- paste(out.combo[krow,covar.vec],collapse="_", sep="")
        if (no.covar.c==0){
          for(icovar.c in 1:ncovar.c){
            covar.i.c.match <- match(covar.c[icovar.c],covar)
            covar.i.c.match.str <- paste("covar_",covar.i.c.match,sep="")  
            covar.i.c <- covar.vec.c[icovar.c]
            covar.i.c.str <- paste(covar.i.c,"_",out.combo[krow,covar.i.c.match.str],sep="")
            covar.i.c.str <- gsub("covar_","",covar.i.c.str)
            out.combo[krow,covar.i.c]<-cus.vec[which(cus.order==covar.i.c.str)]
          }
          c.cols <- c("c_int",covar.vec.c)
        }
        if (no.covar.u==0){
          for(icovar.u in 1:ncovar.u){
            covar.i.u.match <- match(covar.u[icovar.u],covar)
            covar.i.u.match.str <- paste("covar_",covar.i.u.match,sep="")  
            covar.i.u <- covar.vec.u[icovar.u]
            covar.i.u.str <- paste(covar.i.u,"_",out.combo[krow,covar.i.u.match.str],sep="")
            covar.i.u.str <- gsub("covar_","",covar.i.u.str)
            out.combo[krow,covar.i.u]<-cus.vec[which(cus.order==covar.i.u.str)]
          }
          u.cols <- c("u_int",covar.vec.u)
        }
        
      }
      
      if(no.cure==0){
        out.combo[krow,"cure"]<-1/(1+exp(-sum(out.combo[krow,c.cols])))
      }
      if(no.cure==1){
        out.combo[krow,"cure"]<-0
      }
      out.combo[krow,"lambda"]<-exp(sum(out.combo[krow,u.cols]))
      out.combo[krow,"k"]<-exp(-out.combo[krow,"s_int"])
    }
    
    cols.merge.seer<- cols.keep.seer[which(!cols.keep.seer %in% c(stage.dist.name,surv.name))]
    cols.merge.int <- c(allvar,"Interval")
    cols.merge.seerint <- c(allvar.seer,"Interval")
    
    seer.out.combo[,"allvar.seer.combo"] <- apply(data.frame(seer.out.combo[,allvar.seer]), 1, x.combo)
    seer.out.combo[,"seerint.combo"] <- apply(data.frame(seer.out.combo[,cols.merge.seerint]), 1, x.combo)
    if(no.stratum==0 | no.covar==0){
      seer.out.combo[,"int.combo"] <- apply(data.frame(seer.out.combo[,cols.merge.int]), 1, x.combo)
    }
    if(no.stratum==1 & no.covar==1){
      seer.out.combo[,"int.combo"]<-paste(0,"_",seer.out.combo[,"Interval"],sep="")
    } 
    
    seerdata.out[,"merge.combo"] <- apply(data.frame(seerdata.out[,cols.merge.seer]), 1, x.combo)
    seerdata.out[,"seerint.combo"] <- apply(data.frame(seerdata.out[,cols.merge.seerint]), 1, x.combo)
    seerdata.dist.out[,"merge.combo"] <- apply(data.frame(seerdata.dist.out[,cols.merge.seer]), 1, x.combo)
    
    seerdata.out[,"obs_dist_surv"]<-NA
    rows.match <- match(seerdata.out$merge.combo, seerdata.dist.out$merge.combo)
    seerdata.out$obs_dist_surv[which(!is.na(rows.match))]<-seerdata.dist.out$obs_dist_surv[rows.match[which(!is.na(rows.match))]]
    
    seer.out.combo[,"obs_surv"]<-NA
    seer.out.combo[,"obs_dist_surv"]<-NA
    rows.match <- match(seer.out.combo$seerint.combo, seerdata.out$seerint.combo)
    seer.out.combo$obs_surv[which(!is.na(rows.match))]<-seerdata.out$obs_surv[rows.match[which(!is.na(rows.match))]]
    seer.out.combo$obs_dist_surv[which(!is.na(rows.match))]<-seerdata.out$obs_dist_surv[rows.match[which(!is.na(rows.match))]]
    
    out.combo[,"int.combo"] <- paste(out.combo$allvar.combo,"_",out.combo$fup,sep="")
    out.combo[,"ID.out"] <- 1:dim(out.combo)[1]
    seer.out.combo[,"ID"] <- 1:dim(seer.out.combo)[1]
    
    out.merge <- merge(out.combo,seer.out.combo,by="int.combo")
    out.merge.sort <- out.merge[order(as.numeric(out.merge$ID)),]
    
    theta.sum[,allvar.seer]<-NULL
    out.theta <- merge(out.merge.sort,theta.sum,by="allvar.seer.combo")
    out <- out.theta[order(as.numeric(out.theta$ID)),]
    
    ###################################################################
    
    ## calculate survival 
    if (out$Link[1]=="Weibull"){
      out[,"surv_notcured"] <- with(out,exp(-(fup/lambda)**k))
      out[,"s1_analytical"] <- with(out,surv_notcured*(1-k/theta/lambda*(fup/lambda)**(k-1)))
      out[,"d_mu_int"] <- with(out,(1-cure)*(k*s1_analytical*(fup/lambda)**k+exp(-(fup/lambda)**k)*k*k/theta/lambda*(fup/lambda)**(k-1)))
      out[,"d_sigma"]  <- with(out,(1-cure)*(s1_analytical*k*(fup/lambda)**k*log(fup/lambda)+exp(-(fup/lambda)**k)*k/theta*lambda**(-k)*fup**(k-1)*(1+k*log(fup/lambda))))
      out[,"d_theta"]  <- with(out,(1-cure)*exp(-(fup/lambda)**k)* k/theta**2/lambda*(fup/lambda)**(k-1))
      out[,"median_surv_notcured"]<-with(out,lambda * log(2)**(1/k))
    }
    if (out$Link[1]=="Loglogistic"){
      out[,"surv_notcured"] <- with(out,1/(1+(fup/lambda)**k))
      out[,"s1_analytical"] <- with(out,(1+(fup/lambda)**k-k/theta/lambda*(fup/lambda)**(k-1))/(1+(fup/lambda)**k)**2)
      out[,"d_mu_int"] <- with(out,(1-cure)*k*(s1_analytical*2*(fup/lambda)**k/(1+(fup/lambda)**k)-s1_analytical+1/(1+(fup/lambda)**k)**2))
      out[,"d_sigma"]  <- with(out,(1-cure)*k*(fup/lambda)**k/(1+(fup/lambda)**k)*((1+k*log(fup/lambda)-theta*fup*log(fup/lambda))/theta/fup/(1+(fup/lambda)**k)+2*s1_analytical*log(fup/lambda)))
      out[,"d_theta"]  <- with(out,(1-cure)*k/lambda/theta**2*(fup/lambda)**(k-1)/(1+(fup/lambda)**k)**2)
      out[,"median_surv_notcured"]<-with(out,lambda)
    }
    
    out[,"surv_curemodel"] <- with(out,cure+(1-cure)*surv_notcured)
    d.c.cols  <- gsub("c_","d_c_",c.cols)
    d.mu.cols <- gsub("u_","d_mu_",u.cols)
    out[,d.c.cols]  <- with(out,cure*(1-cure)*(1-s1_analytical))  ## different format from sas formula
    out[,d.mu.cols] <- out$d_mu_int 
    
    cov.matrix <- csdata[,c(cint.pos:sint.pos)]
    cov.matrix[,"stratum.combo"] <- stratum.combo
    cov.matrix[,"estimate"] <- NULL
    
    for (k in 1:dim(out)[1]){
      stratum.combo.k <- out$stratum.combo[k]
      stratum.value.k <- out[k,stratum.vec]
      
      cols.keep <- c("c_int","u_int","s_int")
      
      if(no.covar==0){
        covar.value.k <- out[k,covar.vec]
        if(no.covar.c==0){
          covar.k.c.match <- match(covar.c,covar)
          covar.value.k.c <- covar.value.k[covar.k.c.match]
          covar.cols.c <- paste(covar.vec.c,"_",covar.value.k.c,sep="")
          covar.cols.c <- gsub("covar_","",covar.cols.c)
        }
        if(no.covar.u==0){
          covar.k.u.match <- match(covar.u,covar)
          covar.value.k.u <- covar.value.k[covar.k.u.match]
          covar.cols.u <- paste(covar.vec.u,"_",covar.value.k.u,sep="")
          covar.cols.u <- gsub("covar_","",covar.cols.u)
        }
        if(no.covar.c==0 & no.covar.u==0){
          cols.keep <- c("c_int",covar.cols.c,"u_int",covar.cols.u,"s_int")
        }
        if(no.covar.c==1 & no.covar.u==0){
          cols.keep <- c("c_int","u_int",covar.cols.u,"s_int")
        }
        if(no.covar.c==0 & no.covar.u==1){
          cols.keep <- c("c_int",covar.cols.c,"u_int","s_int")
        }
      }
      
      if(no.cure==1){
        cols.keep <- cols.keep[-which(cols.keep=="c_int")]
      }
      rows.k <- which(cov.matrix$stratum.combo==stratum.combo.k)
      if(no.cure==0){
        cov.matrix.k <- cov.matrix[rows.k,1:(dim(cov.matrix)[2]-1)]
      }
      if(no.cure==1){
        cov.matrix.k <- cov.matrix[rows.k,2:(dim(cov.matrix)[2]-1)]
      }
      cols.keep.pos <- which(colnames(cov.matrix.k) %in% cols.keep)
      covariance <- cov.matrix.k[cols.keep.pos,cols.keep.pos]
      row.0 <- rep(0,dim(covariance)[1])
      covariance.k <- rbind(covariance,row.0)
      col.0 <- rep(0,dim(covariance.k)[2]+1)
      covariance.k <- cbind(covariance.k,col.0)
      covariance.k[dim(covariance.k)[1],dim(covariance.k)[1]]<-out$theta.se[k]^2
      if(no.cure==0){
        d.vec.k <- as.numeric(out[k,c(d.c.cols,d.mu.cols,"d_sigma","d_theta")])
      }
      if(no.cure==1){
        d.vec.k <- as.numeric(out[k,c(d.mu.cols,"d_sigma","d_theta")])
      }
      varcov.k<-t(d.vec.k)%*%as.matrix(covariance.k)%*%d.vec.k
      out[k,"var_CI_analytical"] <- varcov.k
      out[k,"se_CI_analytical"]  <- sqrt(varcov.k)
    }
    
    out[,c(c.cols,u.cols,"s_int","allvar.combo","stratum.combo","covar.combo","int.combo","ID","ID.out")]<-NULL
    out[,c(d.c.cols,d.mu.cols,"d_sigma","d_theta")]<-NULL
    
    t_surv  <- out$surv_notcured
    t2_surv <- out$obs_dist_surv
    
    rows.fup1  <- which(out$fup==1)
    vec.fup1   <- c(rows.fup1,dim(out)[1]+1)
    diff.fup1  <- diff(vec.fup1)
    old_t_surv <- c(1,t_surv[-length(t_surv)])
    old_t_surv[rows.fup1] <- 1
    old_t2_surv <- c(1,t2_surv[-length(t2_surv)])
    old_t2_surv[rows.fup1] <- 1
    
    
    t_pdf  <- old_t_surv-t_surv
    t2_pdf <- old_t2_surv^RR-t2_surv^RR
    t2_pdf[rows.fup1] <- 1-t2_surv[rows.fup1]^RR
    temp_pdf <- cbind(t_pdf,t2_pdf)
    
    
    volterra_continuous <- function(fstar,f2,delta=1){
      f1      <- rep(NA,length(fstar))
      f1_pdf  <- rep(NA,length(fstar))
      f1_surv <- rep(NA,length(fstar))
      f1[1]   <- fstar[1]/(delta*f2[1])
      f1_pdf[1]  <- f1[1]
      f1_surv[1] <- 1-f1[1]*delta
      for (i in 2:length(fstar)){
        f1[i] <- fstar[i]
        for (j in 1:(i-1)){
          f1[i] <- f1[i]-f1[j]*f2[i-j+1]*delta
        }
        f1[i] <- f1[i]/(delta*f2[1])
        f1_pdf[i] <- f1[i]
        f1_surv[i] <- f1_surv[i-1]-f1[i]*delta
      }
      return(f1_surv)
    }
    
    out[,"s1_numerical"]<-NA
    
    for(ifup1 in 1:length(rows.fup1)){
      
      istart <- rows.fup1[ifup1]
      iend   <- (rows.fup1+diff.fup1-1)[ifup1]
      fstar  <- t_pdf[istart:iend]
      f2     <- t2_pdf[istart:iend]
      delta  <- 1
      f1_surv_ifup1 <- volterra_continuous(fstar,f2)
      out$s1_numerical[istart:iend] <- f1_surv_ifup1
    }
    
    out$s1_numerical[which(out$s1_numerical<0)] <- 0
    out$s1_analytical[which(out$s1_analytical<0)] <- 0
    out[,"G_numerical"]   <- with(out,cure+(1-cure)*s1_numerical)
    out[,"CI_numerical"]  <- 1-out$G_numerical
    out[,"G_analytical"]  <- with(out,cure+(1-cure)*s1_analytical)
    out[,"CI_analytical"] <- 1-out$G_analytical
    
    colnames(out)[which(colnames(out)=="Link")] <- "link"
    colnames(out)[which(colnames(out)=="fup")] <- "follow_up"
    othervar<-allvar.seer[which(!allvar.seer %in% allvar)]
    
    out <- out[which(out$Interval<=interval.max),]
    
    if(no.stratum==0 | no.covar==0){
      out <- out[,c(allvar,othervar,"follow_up","link","r","cure","lambda","k","theta",
                    "surv_curemodel","surv_notcured","median_surv_notcured",
                    "s1_numerical","G_numerical","CI_numerical",
                    "s1_analytical","G_analytical","CI_analytical","se_CI_analytical",
                    "obs_surv","obs_dist_surv")]
    }
    if(no.stratum==1 & no.covar==1){
      out <- out[,c(allvar.seer,"follow_up","link","r","cure","lambda","k","theta",
                    "surv_curemodel","surv_notcured","median_surv_notcured",
                    "s1_numerical","G_numerical","CI_numerical",
                    "s1_analytical","G_analytical","CI_analytical","se_CI_analytical",
                    "obs_surv","obs_dist_surv")]
    }  
    
    return(out)
    
  })
  
  
  
  output$contents <- renderDataTable({
    if (input$action == 0)
      return()
    isolate({ 
      risk.out()
    })  
    
  })
  
  output$downloadtable <- downloadHandler(
    filename = function() {
      paste('recurrence_risks_groupdata_output', '.csv', sep='')
    },
    content = function(file) {
      write.csv(risk.out(), file,row.names=F,quote=F)
    }
  )
  
  
### tab 2 individual data  
  seerdata.tab2<-reactive({
    inFile1.tab2 <- input$file1.tab2
    if (is.null(inFile1.tab2)) return(NULL)
    data.seerstat.tab2<-fread(inFile1.tab2$datapath)
    data.seerstat.tab2<-data.frame(data.seerstat.tab2,stringsAsFactors=F)
    return(data.seerstat.tab2)
  })
  choices.cnames.tab2 = reactive({
    cnames.seer <- colnames(seerdata.tab2())
    return(cnames.seer)
  })
  choices.stagevalues.tab2 = reactive({
    if (input$stage.var.tab2 == "") return()
    sort(unique(seerdata.tab2()[,input$stage.var.tab2]))
  })
  
  observe({
    updateSelectInput(session, "strata.tab2", choices = choices.cnames.tab2())
  })
  observe({
    updateSelectInput(session, "covar.tab2", choices = choices.cnames.tab2())
  })
  observe({
    updateSelectInput(session, "time.tab2", choices = choices.cnames.tab2())
  })
  observe({
    updateSelectInput(session, "event.tab2", choices = choices.cnames.tab2())
  })
  observe({
    updateSelectInput(session, "stage.var.tab2", choices = choices.cnames.tab2())
  })
  observe({
    updateSelectInput(session, "stage.dist.value.tab2", choices = choices.stagevalues.tab2())
  })

  max.fup.tab2 = reactive({
    if (input$time.tab2 == "") return()
    fup.uni<-unique(seerdata.tab2()[,input$time.tab2])
    if (is.null(fup.uni)) return()
    fup.uni.sort <- sort(fup.uni,decreasing=T,na.last = NA)
    return(fup.uni.sort[1])
  })
  observe({
    updateSliderInput(session, "fup.value.tab2", value=25,min=1,max=max.fup.tab2())
  })
  
  risk.out.tab2<-reactive({
    
    validate(
      need(is.numeric(input$r)== T, "Adjustment Factor r should be numeric.")
    )
    
    
    seerdata<-seerdata.tab2()
    stratum<-input$strata.tab2
    covar<-input$covar.tab2
    timevar<-input$time.tab2
    eventvar<-input$event.tab2
    stage.dist.name <- input$stage.var.tab2 
    stage.dist.value <- as.numeric(input$stage.dist.value.tab2) 
    RR <- as.numeric(input$r.tab2)
    int.max.out <- as.numeric(input$fup.value.tab2)
    link<-input$dist.tab2
    if(link=="Weibull"){
      distribution<-"weibull"
    }
    if(link=="Log-logistic"){
      distribution<-"llogis"
    }
    
    nstratum<-length(stratum)
    ncovar<-length(covar)

    if(nstratum>0 & ncovar>0){
      stratum.nm.string<-paste("!is.na(seerdata$",stratum,")", collapse=" & ",sep="")
      covar.nm.string<-paste("!is.na(seerdata$",covar,")", collapse=" & ",sep="")  
      allvar.nm.string<-paste(stratum.nm.string," & ",covar.nm.string,sep="")
    }
    
    if(ncovar==0 & nstratum>0){
      stratum.nm.string<-paste("!is.na(seerdata$",stratum,")", collapse=" & ",sep="")
      allvar.nm.string<-stratum.nm.string
    }
    if(nstratum==0 & ncovar>0){
      covar.nm.string<-paste("!is.na(seerdata$",covar,")", collapse=" & ",sep="")  
      allvar.nm.string<-covar.nm.string
    }
    if(nstratum==0 & ncovar==0){
      stratum<-"nostratum"
      seerdata[,"nostratum"]<-0
      data<-seerdata
    }
    
    if(nstratum>0 | ncovar>0){
      seerdata.nm <- eval(parse(text=paste("seerdata[",allvar.nm.string,",]")))
      data<-seerdata.nm
    }
    
    int.max<-max(data[,timevar],na.rm=T) 
    
    allvar<-c(covar,stratum)
    nallvar<-length(allvar)
    allvar.group.nvalue <- rep(NA,nallvar)
    allvar.group <- vector("list",nallvar)
    
    for (iallvar in 1:nallvar){
      if(nstratum==0 & ncovar==0){
        allvar.group[[iallvar]] <- 0
      }
      if(nstratum>0 | ncovar>0){
        #iallvar.group<-attributes(table(seerdata[,allvar[iallvar]]))$dimnames[[1]]
        iallvar.group<-unique(seerdata[,allvar[iallvar]])
        iallvar.group<-sort(iallvar.group, na.last = NA)
        iallvar.group<-iallvar.group[which(!is.na(iallvar.group))]
        allvar.group[[iallvar]] <- iallvar.group
      }
      allvar.group.nvalue[iallvar] <- length(allvar.group[[iallvar]])
    }
    intervals<-list(1:int.max)
    out.combo.list <- append(intervals,allvar.group) 
    out.combo <- expand.grid(out.combo.list)
    colnames(out.combo)<-c("fup",allvar)
    
    if(ncovar>0){
      covar.char<-paste(covar,".char",sep="")
      out.combo[,covar.char]<-NA
      for(icovar in 1:length(covar)){
        out.combo[,covar.char[icovar]]<-paste(covar[icovar],out.combo[,covar[icovar]],sep="")
      }
    }
    if(stage.dist.name %in% allvar){
      out.combo<-out.combo[-which(out.combo[,stage.dist.name]==stage.dist.value),]
    }
    addcols<-c("cure","lambda","k","r","theta","theta.se","obs_surv","obs_dist_surv","surv_curemodel",
               "surv_notcured","median_surv_notcured","s1_analytical","se_CI_analytical",
               "d_c_int","d_u_int","d_s_int","d_theta","d_cure_int","d_lambda_int","d_k_int")
    out.combo[,addcols]<-NA
    x.combo<-function(x){
      paste(x,collapse="_",sep="")
    }
    
    if(nstratum>0){
      stratum.group<-allvar.group[which(allvar %in% stratum)] 
      stratum.group.nvalue<-allvar.group.nvalue[which(allvar %in% stratum)] 
      stratum.all <- expand.grid(stratum.group)
      colnames(stratum.all)<-stratum
      if(!stagevar %in% stratum){
        stratum.nodist<-stratum.all
      }
      if(stagevar %in% stratum){
        stratum.nodist<-stratum.all[which(stratum.all[,stage.dist.name]!=stage.dist.value),] 
      }
      if(nstratum==1 & (stagevar %in% stratum)){
        dist.pos<-which(stratum.all[,stage.dist.name]==stage.dist.value)
        stratum.nodist<-expand.grid(stratum.group[[1]][c(-dist.pos)])
        colnames(stratum.nodist)<-stratum
      }
      if(nstratum==1 & (!stagevar %in% stratum)){
        stratum.nodist<-expand.grid(stratum.group[[1]])
        colnames(stratum.nodist)<-stratum
      }
      out.combo[,"stratum.combo"]<-apply(data.frame(out.combo[,stratum]), 1, x.combo)
    }
    if(ncovar>0){
      covar.group<-allvar.group[which(allvar %in% covar)] 
      covar.group.nvalue<-allvar.group.nvalue[which(allvar %in% covar)] 
      covar.all <- expand.grid(covar.group)
      colnames(covar.all)<-covar
      
      c.cols<-paste("c_",covar,sep="")
      u.cols<-paste("u_",covar,sep="")
      d.c.cols<-gsub("c_","d_c_",c.cols)
      d.u.cols<-gsub("u_","d_u_",u.cols)
      covar.all[,c("cure","lambda",c.cols,u.cols)]<-NA
      out.combo[,c(d.c.cols,d.u.cols)]<-NA
      
      all.c.cols<-vector("list", ncovar)
      all.u.cols<-vector("list", ncovar)
      cov.c.cols<-vector("list", ncovar)
      cov.u.cols<-vector("list", ncovar)
      
      data[,covar.char]<-NA
      for(icovar in 1:ncovar){
        data[,covar.char[icovar]]<-paste(covar[icovar],data[,covar[icovar]],sep="")
        all.c.cols[[icovar]]<-paste(c.cols[icovar],"_",covar.group[[icovar]],sep="")
        all.u.cols[[icovar]]<-paste(u.cols[icovar],"_",covar.group[[icovar]],sep="")
        cov.c.cols[[icovar]]<-paste(c.cols[icovar],"_",covar.group[[icovar]][-1],sep="")
        cov.u.cols[[icovar]]<-paste(u.cols[icovar],"_",covar.group[[icovar]][-1],sep="")
      }
      out.combo[,"covar.combo"]<-apply(data.frame(out.combo[,covar]), 1, x.combo)
      if(stage.dist.name %in% covar){
        covar.all<-covar.all[-which(covar.all[,stage.dist.name]==stage.dist.value),]
      }
    }
    out.combo$r<-RR
    str.km<-1
    str<-1
    if(ncovar>0){
      str.km<-paste(covar,collapse="+",sep="")
      str.scale<-paste("scale(",covar.char,")",collapse=" + ", sep="")
      str.cure<-paste(covar.char,collapse="+",sep="")
      str<-paste(str.cure,"+",str.scale,sep="")
    }
    str.km<-paste("Surv(",timevar,",", eventvar,")~",str.km,sep="")
    form.km<-as.formula(str.km)
    
    str<-paste("Surv(",timevar,",", eventvar,")~",str,sep="")
    form<-as.formula(str)
    
    if(nstratum==0){
      nstratumgroup<-1
    }
    if(nstratum>0){
      nstratumgroup<-dim(stratum.nodist)[1]
    }
    
    for(istratumgroup in 1:nstratumgroup){
      if(nstratum>0){
        stratum.value<-stratum.nodist[istratumgroup,]
        stratum.combo<-x.combo(stratum.value)
        stratum.dist.value<-stage.dist.value
        if(nstratum>1 & (stagevar %in% stratum)){
          stratum.dist.value<-stratum.value
          stratum.dist.value[which(stratum==stage.dist.name)]<-stage.dist.value
        }
        condition.string<-paste("data$",stratum,"==",stratum.value, collapse=" & ",sep="")
        data.km <- eval(parse(text=paste("data[",condition.string,",]")))
        dist.string<-paste("data$",stratum,"==",stratum.dist.value, collapse=" & ",sep="")   
        distdata.km <- eval(parse(text=paste("data[",dist.string,",]")))
        if(!stagevar %in% stratum){
          dist.string<-paste(condition.string, " & data$", stage.dist.name, "==",stage.dist.value,sep="")   
          distdata.km <- eval(parse(text=paste("data[",dist.string,",]")))
        }
      }
      
      if(nstratum==0){
        data.km<-data
        #data.km<-data[which(data[,stage.dist.name]!=stage.dist.value),]
        distdata.km<-data[which(data[,stage.dist.name]==stage.dist.value),]
      }
      
      km.model<-survfit(form.km, data=data.km,type="kaplan-meier")
      fit.km<-summary(km.model)
      nobs<-km.model$strata
      
      km.model.dist<-survfit(form.km, data=distdata.km,type="kaplan-meier")
      fit.km.dist<-summary(km.model.dist)
      nobs.dist<-km.model.dist$strata
      
      cure.model<-flexsurvcure(form, data=data.km,dist=distribution, mixture=T)
      
      res<-cure.model$res
      cov<-cure.model$cov
      if(ncovar==0){
        row.names(res)<-c("cure_int","lambda_int","k_int")
      }
      if(ncovar>0){
        row.names(res)<-c("cure_int","lambda_int","k_int",unlist(cov.c.cols),unlist(cov.u.cols))
        scale.pos<-which(substr(row.names(res),1,2)=="u_")
        cure.pos<-c(4:(scale.pos[1]-1))
        c.covar.vec<-vector("list", ncovar)
        u.covar.vec<-vector("list", ncovar)
        for(icovar in 1:ncovar){
          covar.i<-covar[icovar]
          if(icovar==1){
            covar.i.curepos<-cure.pos[1:(covar.group.nvalue[1]-1)]
            covar.i.scalepos<-scale.pos[1:(covar.group.nvalue[1]-1)]
          }
          if(icovar>1){
            covar.i.curepos<-cure.pos[(sum(covar.group.nvalue[1:(icovar-1)]-1)+1):sum(covar.group.nvalue[1:(icovar)]-1)]
            covar.i.scalepos<-scale.pos[(sum(covar.group.nvalue[1:(icovar-1)]-1)+1):sum(covar.group.nvalue[1:(icovar)]-1)]
          }
          c.covar.vec[[icovar]]<-c(0,res[covar.i.curepos,1])
          u.covar.vec[[icovar]]<-c(0,res[covar.i.scalepos,1])
        }
      }
      colnames(cov)<-row.names(res)
      rownames(cov)<-colnames(cov)
      
      cure.int<-res[1,1]
      k.int<-res[2,1]
      lambda.int<-res[3,1]
      
      c.int<-log(cure.int/(1-cure.int))
      l.int<-log(lambda.int)
      
      if(nstratum==0){
        rows.istratum<-1:dim(out.combo)[1]
      }
      if(nstratum>0){
        rows.istratum<-which(out.combo$stratum.combo==stratum.combo)
      }
      
      out.combo[rows.istratum,"k"]<-k.int
      s.int<--log(k.int)
      out.combo[rows.istratum,"c_int"]<-c.int
      out.combo[rows.istratum,"u_int"]<-l.int
      out.combo[rows.istratum,"s_int"]<-s.int
      
      theta0 <- 0.1
      
      if(ncovar==0){
        ncovargroup<-1
        out.combo[rows.istratum,"cure"]<-cure.int
        out.combo[rows.istratum,"lambda"]<-lambda.int
      }  
      if(ncovar>0){
        ncovargroup<-dim(covar.all)[1]
      }
      
      for(icovargroup in 1:ncovargroup){
        
        d.vec.keep<-c("d_cure_int","d_lambda_int","d_k_int","d_theta")  
        cols.keep<-c("cure_int","lambda_int","k_int")
        ### vector dataframe value change
        if(ncovar>0){
          covar.value<-covar.all[icovargroup,covar]
          covar.char.value<-paste(covar,covar.value,sep="")
          covar.string<-paste(covar,"=",covar.value, collapse=", ",sep="")
          covar.char.string<-paste(covar.char,"=",covar.char.value, collapse=",",sep="")
          d.vec.keep<-c("d_cure_int","d_lambda_int","d_k_int",d.c.cols,d.u.cols,"d_theta")  
          cols.keep.c<-vector("list", ncovar)
          cols.keep.u<-vector("list", ncovar)
          
          for(icovar in 1:ncovar){
            covar.all[icovargroup,c.cols[icovar]]<-c.covar.vec[[icovar]][which(covar.group[[icovar]]==covar.all[icovargroup,covar[[icovar]]])]
            covar.all[icovargroup,u.cols[icovar]]<-u.covar.vec[[icovar]][which(covar.group[[icovar]]==covar.all[icovargroup,covar[[icovar]]])]
            cols.keep.c[[icovar]]<-all.c.cols[[icovar]][which(covar.group[[icovar]]==covar.all[icovargroup,covar[[icovar]]])]
            cols.keep.u[[icovar]]<-all.u.cols[[icovar]][which(covar.group[[icovar]]==covar.all[icovargroup,covar[[icovar]]])]
            if(covar.all[icovargroup,covar[[icovar]]]==covar.group[[icovar]][1]){
              cols.keep.c[[icovar]]<-"cure_int"
              cols.keep.u[[icovar]]<-"lambda_int"
              d.vec.keep[which(d.vec.keep==d.c.cols[icovar])]<-NA
              d.vec.keep[which(d.vec.keep==d.u.cols[icovar])]<-NA
            }
          }
          d.vec.keep<-d.vec.keep[which(!is.na(d.vec.keep))]
          cols.keep<-c("cure_int","lambda_int","k_int",unlist(cols.keep.c),unlist(cols.keep.u))
          covar.all[icovargroup,"cure"]<-1/(1+exp(-(c.int+sum(covar.all[icovargroup,c.cols]))))
          covar.all[icovargroup,"lambda"]<-exp(l.int+sum(covar.all[icovargroup,u.cols]))
        }
        
        cols.keep.pos<-which(colnames(cov) %in% cols.keep)
        cov.icovargroup<-cov[cols.keep.pos,cols.keep.pos]
        
        if(ncovar==0){
          surv<-fit.km$surv
          surv.se<-fit.km$std.err
          distsurv<-fit.km.dist$surv
          distsurv.se<-fit.km.dist$std.err
          if(length(fit.km$surv)-length(fit.km.dist$surv)>0){
            surv<-c(surv,rep(NA,int.max-length(surv)))
            surv.se<-c(surv.se,rep(NA,int.max-length(surv.se)))
            distsurv<-c(distsurv,rep(distsurv[length(distsurv)],length(fit.km$surv)-length(distsurv)),rep(NA,int.max-length(fit.km$surv)))
            distsurv.se<-c(distsurv.se,rep(distsurv.se[length(distsurv.se)],length(fit.km$std.err)-length(distsurv.se)),rep(NA,int.max-length(fit.km$std.err)))
          }
          if(length(fit.km$surv)-length(fit.km.dist$surv)<=0){
            distsurv<-c(distsurv,rep(NA,int.max-length(distsurv)))
            distsurv.se<-c(distsurv.se,rep(NA,int.max-length(distsurv.se)))
            surv<-c(surv,rep(surv[length(surv)],length(fit.km.dist$surv)-length(surv)),rep(NA,int.max-length(fit.km.dist$surv)))
            surv.se<-c(surv.se,rep(surv.se[length(surv.se)],length(fit.km.dist$std.err)-length(surv.se)),rep(NA,int.max-length(fit.km.dist$std.err)))
            
          }
          surv.cure<-summary(cure.model, t=seq(from=1,to=int.max,by=1), type="survival")[[1]][,"est"]
        }
        
        if(ncovar>0){
          surv<-fit.km$surv[which(fit.km$strata==covar.string)]
          surv.se<-fit.km$std.err[which(fit.km$strata==covar.string)]
          surv<-c(surv,rep(surv[length(surv)],int.max-length(surv)))
          surv.se<-c(surv.se,rep(surv.se[length(surv.se)],int.max-length(surv.se)))
          strata.pos<-which(names(km.model$strata)==covar.string)
          if(nobs[strata.pos]<int.max){
            surv[c((nobs[strata.pos]+1):length(surv))]<-NA
            surv.se[c((nobs[strata.pos]+1):length(surv.se))]<-NA
          }
          
          if((stage.dist.name %in% covar) & ncovar==1){
            distsurv<-fit.km.dist$surv
            distsurv.se<-fit.km.dist$std.err
            distsurv<-c(distsurv,rep(distsurv[length(distsurv)],nobs[strata.pos]-length(distsurv)),rep(NA,int.max-nobs[strata.pos]))
            distsurv.se<-c(distsurv.se,rep(distsurv.se[length(distsurv.se)],nobs[strata.pos]-length(distsurv.se)),rep(NA,int.max-nobs[strata.pos]))
          }
          if((stage.dist.name %in% covar) & ncovar>1){
            dist.covar.value<-covar.value
            dist.covar.value[which(covar==stage.dist.name)]<-stage.dist.value
            dist.covar.string<-paste(covar,"=",dist.covar.value, collapse=", ",sep="")
            distsurv<-fit.km.dist$surv[which(fit.km.dist$strata==dist.covar.string)]
            distsurv.se<-fit.km.dist$std.err[which(fit.km.dist$strata==dist.covar.string)]
            distsurv<-c(distsurv,rep(distsurv[length(distsurv)],int.max-length(distsurv)))
            distsurv.se<-c(distsurv.se,rep(distsurv.se[length(distsurv.se)],int.max-length(distsurv.se)))
            strata.dist.pos<-which(names(km.model.dist$strata)==dist.covar.string)
            if(nobs.dist[strata.dist.pos]<int.max){
              distsurv[c((nobs.dist[strata.dist.pos]+1):length(distsurv))]<-NA
              distsurv.se[c((nobs.dist[strata.dist.pos]+1):length(distsurv.se))]<-NA
            }
          }
          if(!stage.dist.name %in% covar){
            distsurv<-fit.km.dist$surv[which(fit.km.dist$strata==covar.string)]
            distsurv.se<-fit.km.dist$std.err[which(fit.km.dist$strata==covar.string)]
            distsurv<-c(distsurv,rep(distsurv[length(distsurv)],int.max-length(distsurv)))
            distsurv.se<-c(distsurv.se,rep(distsurv.se[length(distsurv.se)],int.max-length(distsurv.se)))
            strata.dist.pos<-which(names(km.model.dist$strata)==covar.string)
            if(nobs.dist[strata.dist.pos]<int.max){
              distsurv[c((nobs.dist[strata.dist.pos]+1):length(distsurv))]<-NA
              distsurv.se[c((nobs.dist[strata.dist.pos]+1):length(distsurv.se))]<-NA
            }
          }
          
          surv.cure<-summary(cure.model, t=seq(from=1,to=int.max,by=1), type="survival")[[strata.pos]][,"est"]
        }
        
        if(istratumgroup==1){
          rows.add<-(int.max*(icovargroup-1)+1):(icovargroup*int.max)
        }
        if(istratumgroup>1){
          rows.add<-(int.max*(icovargroup-1)+1):(icovargroup*int.max)+sum(table(out.combo$stratum.combo)[1:(istratumgroup-1)])
        }
        out.combo[rows.add,"obs_surv"]<-surv
        out.combo[rows.add,"obs_dist_surv"]<-distsurv
        out.combo[rows.add,"surv_curemodel"]<-surv.cure
        
        if(ncovar>0){
          out.combo[rows.add,"cure"]<-covar.all[icovargroup,"cure"]
          out.combo[rows.add,"lambda"]<-covar.all[icovargroup,"lambda"]
        }
        
        fup <- 1:int.max
        fit<-NULL
        if(dim(distdata.km)[1]>2 & length(which(!is.na(distsurv)))>2 & length(table(distsurv))>2){
          try(fit <- nls(distsurv~exp(-theta*fup),start=list(theta=theta0),weights=1/distsurv.se^2))
          if(!is.null(fit)){
            theta <- summary(fit)$coefficients[1,1]
            theta.se <- summary(fit)$coefficients[1,2]
            out.combo[rows.add,"theta"] <- RR*theta
            out.combo[rows.add,"theta.se"] <- theta.se
          }
        }
        
        if (distribution=="weibull"){
          out.combo[rows.add,"surv_notcured"] <- with(out.combo[rows.add,],exp(-(fup/lambda)**k))
          out.combo[rows.add,"s1_analytical"] <- with(out.combo[rows.add,],surv_notcured*(1-k/theta/lambda*(fup/lambda)**(k-1)))
          out.combo[rows.add,"d_u_int"] <- with(out.combo[rows.add,],(1-cure)*(k*s1_analytical*(fup/lambda)**k+exp(-(fup/lambda)**k)*k*k/theta/lambda*(fup/lambda)**(k-1)))
          out.combo[rows.add,"d_s_int"]  <- with(out.combo[rows.add,],(1-cure)*(s1_analytical*k*(fup/lambda)**k*log(fup/lambda)+exp(-(fup/lambda)**k)*k/theta*lambda**(-k)*fup**(k-1)*(1+k*log(fup/lambda))))
          out.combo[rows.add,"d_theta"]  <- with(out.combo[rows.add,],(1-cure)*exp(-(fup/lambda)**k)* k/theta**2/lambda*(fup/lambda)**(k-1))
          out.combo[rows.add,"median_surv_notcured"]<-with(out.combo[rows.add,],lambda * log(2)**(1/k))
          
        }
        if (distribution=="llogis"){
          out.combo[rows.add,"surv_notcured"] <- with(out.combo[rows.add,],1/(1+(fup/lambda)**k))
          out.combo[rows.add,"s1_analytical"] <- with(out.combo[rows.add,],(1+(fup/lambda)**k-k/theta/lambda*(fup/lambda)**(k-1))/(1+(fup/lambda)**k)**2)
          out.combo[rows.add,"d_u_int"] <- with(out.combo[rows.add,],(1-cure)*k*(s1_analytical*2*(fup/lambda)**k/(1+(fup/lambda)**k)-s1_analytical+1/(1+(fup/lambda)**k)**2))
          out.combo[rows.add,"d_s_int"]  <- with(out.combo[rows.add,],(1-cure)*k*(fup/lambda)**k/(1+(fup/lambda)**k)*((1+k*log(fup/lambda)-theta*fup*log(fup/lambda))/theta/fup/(1+(fup/lambda)**k)+2*s1_analytical*log(fup/lambda)))
          out.combo[rows.add,"d_theta"]  <- with(out.combo[rows.add,],(1-cure)*k/lambda/theta**2*(fup/lambda)**(k-1)/(1+(fup/lambda)**k)**2)
          out.combo[rows.add,"median_surv_notcured"]<-with(out.combo[rows.add,],lambda)
        }
        
        out.combo[rows.add,"d_c_int"]  <- with(out.combo[rows.add,],cure*(1-cure)*(1-s1_analytical))  ## different format from sas formula
        out.combo[rows.add,"d_cure_int"]<- out.combo[rows.add,"d_c_int"]*(1/cure.int-1/(1-cure.int))
        out.combo[rows.add,"d_lambda_int"]<- out.combo[rows.add,"d_u_int"]*1/lambda.int
        out.combo[rows.add,"d_k_int"]<- out.combo[rows.add,"d_s_int"]*(-1/k.int)
        
        if(ncovar>0){
          out.combo[rows.add,d.c.cols] <- out.combo[rows.add,"d_c_int"]
          out.combo[rows.add,d.u.cols] <- out.combo[rows.add,"d_u_int"]
        }
        
        cov.sub<-cov.icovargroup
        row.0 <- rep(0,dim(cov.sub)[1])
        cov.sub <- rbind(cov.sub,row.0)
        col.0 <- rep(0,dim(cov.sub)[2]+1)
        cov.sub <- cbind(cov.sub,col.0)
        colnames(cov.sub)[dim(cov.icovargroup)[1]+1]<-"theta"
        rownames(cov.sub)[dim(cov.icovargroup)[1]+1]<-"theta"
        cov.sub[dim(cov.sub)[1],dim(cov.sub)[1]]<-theta.se^2
        
        ## currently work for model with 1 covariate, need to test for multiple covars, could add columns 0's for group 0 
        for(k in 1:length(rows.add)){
          d.vec.k<-out.combo[rows.add[k],d.vec.keep]
          d.vec.k<-as.numeric(d.vec.k)
          varcov.k<-t(d.vec.k)%*%as.matrix(cov.sub)%*%d.vec.k
          out.combo[rows.add[k],"se_CI_analytical"]<-sqrt(varcov.k[1,1])
        }
      }
    }
    del.cols<-c("c_int","u_int","s_int","d_c_int","d_u_int","d_s_int","d_theta",
                "d_cure_int","d_lambda_int","d_k_int","theta.se")
    
    if(nstratum>0 & ncovar>0){
      del.cols<-c(del.cols,d.c.cols,d.u.cols, covar.char,"covar.combo","stratum.combo")
    }
    if(nstratum==0 & ncovar>0){
      del.cols<-c(del.cols,d.c.cols,d.u.cols, covar.char,"covar.combo")
    }
    if(nstratum>0 & ncovar==0){
      del.cols<-c(del.cols,"stratum.combo")
    }
    
    out.combo[,del.cols]<-NULL
    out<-out.combo
    
    t_surv  <- out$surv_notcured
    t2_surv <- out$obs_dist_surv
    
    rows.fup1  <- which(out$fup==1)
    vec.fup1   <- c(rows.fup1,dim(out)[1]+1)
    diff.fup1  <- diff(vec.fup1)
    old_t_surv <- c(1,t_surv[-length(t_surv)])
    old_t_surv[rows.fup1] <- 1
    old_t2_surv <- c(1,t2_surv[-length(t2_surv)])
    old_t2_surv[rows.fup1] <- 1
    
    
    t_pdf  <- old_t_surv-t_surv
    t2_pdf <- old_t2_surv^RR-t2_surv^RR
    t2_pdf[rows.fup1] <- 1-t2_surv[rows.fup1]^RR
    temp_pdf <- cbind(t_pdf,t2_pdf)
    
    
    volterra_continuous <- function(fstar,f2,delta=1){
      f1      <- rep(NA,length(fstar))
      f1_pdf  <- rep(NA,length(fstar))
      f1_surv <- rep(NA,length(fstar))
      f1[1]   <- fstar[1]/(delta*f2[1])
      f1_pdf[1]  <- f1[1]
      f1_surv[1] <- 1-f1[1]*delta
      for (i in 2:length(fstar)){
        f1[i] <- fstar[i]
        for (j in 1:(i-1)){
          f1[i] <- f1[i]-f1[j]*f2[i-j+1]*delta
        }
        f1[i] <- f1[i]/(delta*f2[1])
        f1_pdf[i] <- f1[i]
        f1_surv[i] <- f1_surv[i-1]-f1[i]*delta
      }
      return(f1_surv)
    }
    
    out[,"s1_numerical"]<-NA
    
    for(ifup1 in 1:length(rows.fup1)){
      
      istart <- rows.fup1[ifup1]
      iend   <- (rows.fup1+diff.fup1-1)[ifup1]
      fstar  <- t_pdf[istart:iend]
      f2     <- t2_pdf[istart:iend]
      delta  <- 1
      f1_surv_ifup1 <- volterra_continuous(fstar,f2)
      out$s1_numerical[istart:iend] <- f1_surv_ifup1
    }
    
    out$s1_numerical[which(out$s1_numerical<0)] <- 0
    out$s1_analytical[which(out$s1_analytical<0)] <- 0
    out[,"G_numerical"]   <- with(out,cure+(1-cure)*s1_numerical)
    out[,"CI_numerical"]  <- 1-out$G_numerical
    out[,"G_analytical"]  <- with(out,cure+(1-cure)*s1_analytical)
    out[,"CI_analytical"] <- 1-out$G_analytical
    out[,"link"]<-link
    
    
    out <- out[which(out$fup<=int.max.out),]
    
    if(nstratum==0 & ncovar==0){
      stratum<-NULL
    }
    out <- out[,c(stratum,covar,"fup","link","r","cure","lambda","k","theta",
                  "surv_curemodel","surv_notcured","median_surv_notcured",
                  "s1_numerical","G_numerical","CI_numerical",
                  "s1_analytical","G_analytical","CI_analytical","se_CI_analytical",
                  "obs_surv","obs_dist_surv")]
    colnames(out)[which(colnames(out)=="fup")]<-"follow-up"
    return(out)
  })
  
  
  output$contents.tab2 <- renderDataTable({
    if (input$action.tab2 == 0)
      return()
    isolate({ 
      risk.out.tab2()
    })  
    
  })
  
  output$downloadtable.tab2 <- downloadHandler(
    filename = function() {
      paste('recurrence_risks_individualdata_output', '.csv', sep='')
    },
    content = function(file) {
      write.csv(risk.out.tab2(), file,row.names=F,quote=F)
    }
  )
  
}
shinyApp(ui, server)
