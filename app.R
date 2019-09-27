###*Initiate Library####
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyLP)
library(shinyBS)
library(shinyjs)
library(slickR)
library(splitstackshape)
library(XML)
library(stringr)
library(raster)
library(rgdal)
library(rgeos)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
library(cleangeo)
library(DT)

library(leaflet)

###*Define Variables####
source('variables.R')
source('kugi5.R')

###*Setting Up Interface####
ui <- source('interface.R')

###*Preparing Server#### 
server <- function(input, output, session) {
  ###*Connect to PostgreSQL Database####
  # driver <- PostgreSQL(max.con = 100)
  driver <- dbDriver("PostgreSQL")
  
  pg_user<-"postgres"
  pg_host<-"localhost"
  pg_port<-"5432"
  pg_pwd<-"root"
  
  pg_raw_db<-"rawdata"
  pg_md_db<-"metadata"
  pg_kugi_db<-"kugi5"
  pg_comp_db<-"compilation"
  # pg_igd_db<-"IGD"
  # pg_int_db<-"integration"
  # pg_sync_db<-"sync"
  # pg_onemap_db<-"onemap"
  
  connectDB <- function(pg_db){
    tryCatch({
      dbConnect(driver, dbname=pg_db, host=pg_host, port=pg_port, user=pg_user, password=pg_pwd )
    }, error=function(e){
      print("Database connection failed")
      return(FALSE)
    })
  }
  
  disconnectDB <- function(name="Database", pg_db){
    print(paste0(name, " disconnected."))
    dbDisconnect(pg_db)
  }
  
  countMetadataTbl <- function(){
    metadata<-connectDB(pg_md_db)
    count_metadata <- dbGetQuery(metadata, "select count(id_metadata) from metadata;")
    disconnectDB("metadata", metadata)
    
    return(count_metadata)
  }
  
  countCompTbl <- function(){
    compilation<-connectDB(pg_comp_db)
    count_compilation <- length(dbListTables(compilation))-3
    disconnectDB("compilation", compilation)
    
    return(count_compilation)
  }
  
  # countIntTbl <- function(){
  #   integration<-connectDB(pg_int_db)
  #   count_integration <- length(dbListTables(integration))-3
  #   disconnectDB("integration", integration)
  #   
  #   return(count_integration)
  # }
  
  getApprovalTbl <- function(){
    # return(dbReadTable(DB, c("public", "metadata")))
    metadata<-connectDB(pg_md_db)
    tblApproval <- dbGetQuery(metadata, "select * from approval;")
    disconnectDB("metadata", metadata)
    
    return(tblApproval)
  }
  
  getMetadataTbl <- function(){
    # return(dbReadTable(DB, c("public", "metadata")))
    metadata<-connectDB(pg_md_db)
    tblMetadata <- dbGetQuery(metadata, "select file_identifier, individual_name, organisation_name, approval, status from metadata;")
    disconnectDB("metadata", metadata)
    
    return(tblMetadata)
  }
  
  listOfTbl <- reactiveValues(metadata=getMetadataTbl(),
                              approval=getApprovalTbl(),
                              numOfMetadata=countMetadataTbl(),
                              numOfCompilated=countCompTbl(),
                              # numOfIntegrated=countIntTbl(),
                              recentMetadata=data.frame(),
                              selectedRawdata="",
                              recentValidityData=data.frame(),
                              tableKugi="",
                              recentTableWithKugi=data.frame(),
                              recentAttributeTable=NULL,
                              recentAttributeKugi=NULL,
                              listMatch=data.frame())
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  output$slideshow <- renderSlickR({
    images <- list.files("www/slideshow/", pattern="j[0-9]", full.names=TRUE)
    slickR(images)
  })
  
  output$countData <- renderUI({
    tags$ul(class="list-group",
      tags$li(class="list-group-item", span(class="badge", listOfTbl$numOfMetadata$count), "Data Input"),
      tags$li(class="list-group-item", span(class="badge", listOfTbl$numOfCompilated), "Compilated Data")
      # tags$li(class="list-group-item", span(class="badge", listOfTbl$numOfIntegrated), "Integrated Data")
    )
  })
  
  ###*Observe Shapefile Input####
  observe({
    inShp <- input$shpData
    inShpType <- inShp$type
    inShpPath <- inShp$datapath
    
    # print(paste0("Shapefile location: ", inShpPath))
    
    if(is.null(inShp)){
      # print("Shapefile.. NULL")
      val <- x_min <- x_max <- y_min <- y_max <- shp_dim <- shp_title <- ""
    } else {
      temp_dir <- dirname(inShpPath)
      unzip(inShpPath, exdir = temp_dir)
      file_shp <- dir(temp_dir, pattern="*.shp$")
      val <- str_remove(basename(file_shp), ".shp")
      shp_title <- val
      
      full_file_shp <- paste0(temp_dir, "/", val, ".shp")
      if(file.exists(full_file_shp)){
        shp_file <- readOGR(dsn = full_file_shp, layer = val)
        
        print("Topology.. Checking")
        if(clgeo_IsValid(shp_file)){
          print("Topology.. OK")
          # input to postgres
          # write to xml
          # print report
        } else {
          # collect invalid issue
          report_shp<-clgeo_CollectionReport(shp_file)
          
          # reset row numbers of original data
          shp_data <- shp_file@data
          row.names(shp_data) <- NULL
          
          # select FALSE validity
          print("Topology.. INVALID")
          shp_invalid <- report_shp[report_shp$valid==FALSE,]
          
          # merge shp_data with report_shp
          final_report_shp <- merge(shp_data, shp_invalid, by="row.names")
          listOfTbl$recentValidityData <- final_report_shp
          
          # clean topology
          print("Topology.. CLEANING")
          showModal(ui=modalDialog("Cleaning topology process. Please wait..", footer = NULL), session=session)
          running_time <- system.time({
            shp_file_clean <- clgeo_Clean(shp_file)
          })
          removeModal(session)
          print(running_time)
          
          # check projection
          print("Projection.. Checking")
          wgs84_proj <- CRS("+proj=longlat +datum=WGS84")
          shp_proj <- crs(shp_file_clean) 
          if(paste0(shp_proj) != paste0(wgs84_proj)){
            print("Projection.. TRANSFORM")
            shp_file <- spTransform(shp_file_clean, wgs84_proj)
          } else {
            print("Projection.. MATCH!")
            shp_file <- shp_file_clean
          }
          
        }        
        
        # insert shp to postgresql
        rawdata<-connectDB(pg_raw_db)
        kugi<-connectDB(pg_kugi_db)
        
        tableKugi <- tolower(unlist(strsplit(input$kugiName, " "))[1])
        insertShp <- tryCatch({ pgInsert(rawdata, tableKugi, shp_file) }, error=function(e){ return(FALSE) })
        if(insertShp){
          print("Shapefile has been imported into database")
          
          # mix and match data with kugi
          alterTableSQL <- paste0("ALTER TABLE ", tableKugi, " ")

          tableKugiInfo <- dbTableInfo(kugi, tableKugi)
          tblkugilen <- nrow(tableKugiInfo)-1
          
          for(i in 2:tblkugilen){
            nullable <- ""
            if(tableKugiInfo$is_nullable[i] == "NO") nullable <- " NOT NULL DEFAULT '0'" 
            
            datatype_length <- ""
            if(!is.na(tableKugiInfo$character_maximum_length[i])) datatype_length <- paste0("(", tableKugiInfo$character_maximum_length[i], ")")
            
            alterTableSQL <- paste0(alterTableSQL,
                                    "ADD COLUMN ",
                                    tableKugiInfo$column_name[i], " ",
                                    tableKugiInfo$data_type[i],
                                    datatype_length, 
                                    nullable
                                  )
            
            alterTableSQL <- ifelse(i != tblkugilen, paste0(alterTableSQL, ", "), paste0(alterTableSQL, ";"))
          }
          dbSendQuery(rawdata, alterTableSQL)
          val <- tableKugi
          
          disconnectDB("rawdata", rawdata)
          disconnectDB("kugi", kugi)
        } else {
          print("Shapefile.. FAILED TO IMPORT")
          showModal(ui=modalDialog("Failed to upload. Please try again..", footer = NULL), session=session)
          removeModal(session)
          return()
        }
      } else {
        print("Shapefile doesn't exist")
        showModal(ui=modalDialog("Shapefile doesn't exist. Please try again..", footer = NULL), session=session)
        removeModal(session)        
        return()
      }
      
      val <- paste0(val, "_", format(Sys.time(), "%Y%m%d%H%M%S"))
    }
    
  })
  
  ###*DATA Page####
  output$app_data <- renderDataTable({
    approval <- listOfTbl$approval
    # action_approve <- shinyInput(actionButton, nrow(approval), 'approve_', label="Approve", onclick='Shiny.onInputChange(\"approve_button\", this.id)')
    # action_reject <- shinyInput(actionButton, nrow(approval), 'reject_', label="Reject", onclick='Shiny.onInputChange(\"reject_button\", this.id)')
    # approval <- cbind(approval, action_approve, action_reject)
    
    # approval$URL <- paste0('<u>Edit Attribute Data</u>')
    datatable(approval, selection="none", class = 'cell-border strip hover', escape=F) %>% formatStyle(1, cursor = 'pointer')
  })
  
  output$comp_data <- renderDataTable({
    metadata <- listOfTbl$metadata
    
    # metadata$URL <- paste0('<u>Edit Attribute Data</u>')
    datatable(metadata, selection="none", class = 'cell-border strip hover', escape=F) %>% formatStyle(1, cursor = 'pointer')
  })
  
  observeEvent(input$approve_button, {
    selected_row <- as.numeric(strsplit(input$approve_button, "_")[[1]][2])
    
    approval <- listOfTbl$approval
    selected_approval <- approval[selected_row, ]
    
    metadata<-connectDB(pg_md_db)
    
    updateTableQuery <- paste0("UPDATE approval SET status = 'Approve', approval= '<button id=\"approve_", selected_approval$id, "\" type=\"button\" class=\"btn btn-default action-button\" onclick=\"Shiny.onInputChange(&quot;approve_button&quot;, this.id)\" disabled>Approve</button>', rejection= '<button id=\"reject_", selected_approval$id, "\" type=\"button\" class=\"btn btn-default action-button\" onclick=\"Shiny.onInputChange(&quot;reject_button&quot;, this.id)\" disabled>Reject</button>' WHERE unique_id='", selected_approval$unique_id, "' AND file_name='", selected_approval$file_name, "' AND individual_name='", selected_approval$individual_name, "';")
    dbSendQuery(metadata, updateTableQuery)
    
    updateTableQuery <- paste0("UPDATE metadata SET approval = 'Approve' WHERE id_metadata=", selected_approval$id, ";")
    dbSendQuery(metadata, updateTableQuery)
    
    disconnectDB("metadata", metadata)
    
    listOfTbl$approval <- getApprovalTbl()
    listOfTbl$metadata <- getMetadataTbl()
  })
  
  observeEvent(input$reject_button, {
    selected_row <- as.numeric(strsplit(input$reject_button, "_")[[1]][2])
    
    approval <- listOfTbl$approval
    selected_approval <- approval[selected_row, ]
    
    metadata<-connectDB(pg_md_db)
    
    updateTableQuery <- paste0("UPDATE approval SET status = 'Reject', approval= '<button id=\"approve_", selected_approval$id, "\" type=\"button\" class=\"btn btn-default action-button\" onclick=\"Shiny.onInputChange(&quot;approve_button&quot;, this.id)\" disabled>Approve</button>', rejection= '<button id=\"reject_", selected_approval$id, "\" type=\"button\" class=\"btn btn-default action-button\" onclick=\"Shiny.onInputChange(&quot;reject_button&quot;, this.id)\" disabled>Reject</button>' WHERE unique_id='", selected_approval$unique_id, "' AND file_name='", selected_approval$file_name, "' AND individual_name='", selected_approval$individual_name, "';")
    dbSendQuery(metadata, updateTableQuery)
    
    updateTableQuery <- paste0("UPDATE metadata SET approval = 'Reject' WHERE id_metadata=", selected_approval$id, ";")
    dbSendQuery(metadata, updateTableQuery)
    
    disconnectDB("metadata", metadata)
    
    listOfTbl$approval <- getApprovalTbl()
    listOfTbl$metadata <- getMetadataTbl()
  })
  
}

###*Run the application#### 
shinyApp(ui = ui, server = server)

