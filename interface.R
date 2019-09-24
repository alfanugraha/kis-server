navbarPage(title = appsTitle, theme = shinytheme("cerulean"), id="compilationApps",
  ###Home####
  tabPanel("Home", value="tabHome",
    slickROutput("slideshow", width="100%"),
    hr(),
    jumbotron(div(img(src="jypr-small.png"), "SATU PETA"), "Mempermudah proses penatagunaan lahan. Menghindari konflik penatagunaan lahan. Mempercepat proses perizinan penatagunaan lahan.", button=FALSE),
    uiOutput("countData"),
    fluidRow(
      column(6, panel_div("primary", panel_title="Data Status", "active")),
      column(6, panel_div("warning", panel_title="Data Reminder", "active"))
    ),
    fluidRow(
      column(6, panel_div("success", panel_title="Last Login", Sys.time())),
      column(6, panel_div("info", panel_title="Last Activity", Sys.Date()))
    )
  ),
  ###Compilation####
  navbarMenu("Kompilasi",
    ###Upload####
    tabPanel("Upload", value="tabUpload", icon = icon("upload"),
      navlistPanel(widths = c(2, 10),
        ###input shapefile####
        # "Upload Data",
        tabPanel("Upload Data",
          selectInput("shapeGeom", "Tipe data vektor", choices=c(`Titik`="_PT_", `Garis`="_LN_", `Poligon`="_AR_")),
          uiOutput("listOfKugi"),
          fileInput("shpData", "Upload Shapefile", buttonLabel="Browse...", placeholder="No file selected", accept = c('zip', 'ZIP', 'ZIP (Archive File)', '.zip', '.ZIP'))
        ),
        "Input Data",
        ###input metadata####
        tabPanel(appsMenu$menuItem[1], 
          textInput(mdEntity$vars[1], mdEntity$name[1], value="", width=NULL, placeholder=""),
          textInput(mdEntity$vars[2], mdEntity$name[2], value="", width=NULL, placeholder=""),
          textInput(mdEntity$vars[3], mdEntity$name[3], value="", width=NULL, placeholder=""),
          textInput(mdEntity$vars[4], mdEntity$name[4], value="", width=NULL, placeholder=""),
          textInput(mdEntity$vars[5], mdEntity$name[5], value="", width=NULL, placeholder=""),
          textInput(mdEntity$vars[6], mdEntity$name[6], value="", width=NULL, placeholder=""),
          dateInput(mdEntity$vars[7], mdEntity$name[7]),
          textInput(mdEntity$vars[8], mdEntity$name[8], value="", width=NULL, placeholder="")
        ),
        ###input contact####
        tabPanel(appsMenu$menuItem[2],
          tabBox(width = "200px",
            tabPanel(title = "Identity", 
              textInput(ctEntity$vars[1], ctEntity$name[1], value="", width=NULL, placeholder="Fill the name"),
              textInput(ctEntity$vars[2], ctEntity$name[2], value="", width=NULL, placeholder=""),
              textInput(ctEntity$vars[3], ctEntity$name[3], value="", width=NULL, placeholder=""),
              textInput(ctEntity$vars[4], ctEntity$name[4], value="", width=NULL, placeholder=""),
              textInput(ctEntity$vars[5], ctEntity$name[5], value="", width=NULL, placeholder="")
            ),
            tabPanel(title = "Location",
              textInput(ctEntity$vars[6], ctEntity$name[6], value="", width=NULL, placeholder=""),
              textInput(ctEntity$vars[7], ctEntity$name[7], value="", width=NULL, placeholder=""),
              textInput(ctEntity$vars[8], ctEntity$name[8], value="", width=NULL, placeholder=""),
              textInput(ctEntity$vars[9], ctEntity$name[9], value="", width=NULL, placeholder=""),
              textInput(ctEntity$vars[10], ctEntity$name[10], value="", width=NULL, placeholder=""),
              textInput(ctEntity$vars[11], ctEntity$name[11], value="", width=NULL, placeholder="")
            ),
            tabPanel(title = "Function",
              textInput(ctEntity$vars[12], ctEntity$name[12], value="", width=NULL, placeholder=""),
              textInput(ctEntity$vars[13], ctEntity$name[13], value="", width=NULL, placeholder=""),
              textInput(ctEntity$vars[14], ctEntity$name[14], value="", width=NULL, placeholder=""),
              textInput(ctEntity$vars[15], ctEntity$name[15], value="", width=NULL, placeholder=""),
              textInput(ctEntity$vars[16], ctEntity$name[16], value="", width=NULL, placeholder=""),
              textInput(ctEntity$vars[17], ctEntity$name[17], value="", width=NULL, placeholder="")
            )
          )
        ),
        ###input spatial representation info####
        tabPanel(appsMenu$menuItem[3],
          tabBox(width = "200px",
            tabPanel(title = "Properties",
              textInput(sriEntity$vars[1], sriEntity$name[1], value="", width=NULL, placeholder=""),
              textInput(sriEntity$vars[2], sriEntity$name[2], value="", width=NULL, placeholder=""),
              textInput(sriEntity$vars[3], sriEntity$name[3], value="", width=NULL, placeholder=""),
              textInput(sriEntity$vars[4], sriEntity$name[4], value="", width=NULL, placeholder=""),
              textInput(sriEntity$vars[5], sriEntity$name[5], value="", width=NULL, placeholder=""),
              textInput(sriEntity$vars[6], sriEntity$name[6], value="", width=NULL, placeholder="")
            ),
            tabPanel(title = "Dimension",
              textInput(sriEntity$vars[7], sriEntity$name[7], value="", width=NULL, placeholder=""),
              textInput(sriEntity$vars[8], sriEntity$name[8], value="", width=NULL, placeholder=""),
              textInput(sriEntity$vars[9], sriEntity$name[9], value="", width=NULL, placeholder=""),
              textInput(sriEntity$vars[10], sriEntity$name[10], value="", width=NULL, placeholder=""),
              textInput(sriEntity$vars[11], sriEntity$name[11], value="", width=NULL, placeholder=""),
              textInput(sriEntity$vars[12], sriEntity$name[12], value="", width=NULL, placeholder=""),
              textInput(sriEntity$vars[13], sriEntity$name[13], value="", width=NULL, placeholder="")
            )
          )
        ),
        ###input reference system info####
        tabPanel(appsMenu$menuItem[4],
          tabBox(width = "200px",
            tabPanel(title = "Info",  
              textInput(refSysEntity$vars[1], refSysEntity$name[1], value="European Petroleum Survey Group (EPSG) Geodetic Parameter Registry", width=NULL, placeholder=""),
              dateInput(refSysEntity$vars[2], refSysEntity$name[2]),
              textInput(refSysEntity$vars[3], refSysEntity$name[3], value="publication", width=NULL, placeholder=""),
              textInput(refSysEntity$vars[4], refSysEntity$name[4], value="EPSG", width=NULL, placeholder=""),
              textInput(refSysEntity$vars[5], refSysEntity$name[5], value="http://www.epsg-registry.org", width=NULL, placeholder=""),
              textInput(refSysEntity$vars[6], refSysEntity$name[6], value="originator", width=NULL, placeholder=""),
              textInput(refSysEntity$vars[7], refSysEntity$name[7], value="urn:ogc:def:crs:EPSG:4326", width=NULL, placeholder=""),
              textInput(refSysEntity$vars[8], refSysEntity$name[8], value="", width=NULL, placeholder="")
            ),
            tabPanel(title = "CRS",  
              textInput(refSysEntity$vars[9], refSysEntity$name[9], value="", width=NULL, placeholder=""),
              textInput(refSysEntity$vars[10], refSysEntity$name[10], value="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", width=NULL, placeholder=""),
              textInput(refSysEntity$vars[11], refSysEntity$name[11], value="", width=NULL, placeholder=""),
              textInput(refSysEntity$vars[12], refSysEntity$name[12], value="", width=NULL, placeholder="")
            )
          )
        ),
        ###input identification info####
        tabPanel(appsMenu$menuItem[5],
          tabBox(width = "200px",
            tabPanel(title = "Description",  
              textInput(idInfoEntity$vars[1], idInfoEntity$name[1], value="", width=NULL, placeholder=""),
              textInput(idInfoEntity$vars[2], idInfoEntity$name[2], value="", width=NULL, placeholder=""),
              dateInput(idInfoEntity$vars[3], idInfoEntity$name[3]),
              textInput(idInfoEntity$vars[4], idInfoEntity$name[4], value="", width=NULL, placeholder=""),
              textInput(idInfoEntity$vars[5], idInfoEntity$name[5], value="", width=NULL, placeholder=""),
              textInput(idInfoEntity$vars[6], idInfoEntity$name[6], value="", width=NULL, placeholder=""),
              textInput(idInfoEntity$vars[7], idInfoEntity$name[7], value="", width=NULL, placeholder="")
            ),
            tabPanel(title = "Info",  
              textInput(idInfoEntity$vars[8], idInfoEntity$name[8], value="", width=NULL, placeholder=""),
              textInput(idInfoEntity$vars[9], idInfoEntity$name[9], value="", width=NULL, placeholder=""),
              textInput(idInfoEntity$vars[10], idInfoEntity$name[10], value="", width=NULL, placeholder=""),
              textInput(idInfoEntity$vars[11], idInfoEntity$name[11], value="", width=NULL, placeholder=""),
              textInput(idInfoEntity$vars[12], idInfoEntity$name[12], value="utf-8", width=NULL, placeholder=""),
              uiOutput("listOfTopicCategory")
            ),
            tabPanel(title = "Coordinates",         
              textInput(idInfoEntity$vars[14], idInfoEntity$name[14], value="", width=NULL, placeholder=""),
              textInput(idInfoEntity$vars[15], idInfoEntity$name[15], value="", width=NULL, placeholder=""),
              textInput(idInfoEntity$vars[16], idInfoEntity$name[16], value="", width=NULL, placeholder=""),
              textInput(idInfoEntity$vars[17], idInfoEntity$name[17], value="", width=NULL, placeholder="")
            )
          )
        ),
        ###input distribution info####
        tabPanel(appsMenu$menuItem[6],
          h2("Distributor"),
          tabBox(width = "200px",
            tabPanel(title = "Identity",
              textInput(disInfoEntity$vars[1], disInfoEntity$name[1], value="", width=NULL, placeholder="Fill the name"),
              textInput(disInfoEntity$vars[2], disInfoEntity$name[2], value="", width=NULL, placeholder=""),
              textInput(disInfoEntity$vars[3], disInfoEntity$name[3], value="", width=NULL, placeholder=""),
              textInput(disInfoEntity$vars[4], disInfoEntity$name[4], value="", width=NULL, placeholder=""),
              textInput(disInfoEntity$vars[5], disInfoEntity$name[5], value="", width=NULL, placeholder="")
            ),
            tabPanel(title = "Location",
              textInput(disInfoEntity$vars[6], disInfoEntity$name[6], value="", width=NULL, placeholder=""),
              textInput(disInfoEntity$vars[7], disInfoEntity$name[7], value="", width=NULL, placeholder=""),
              textInput(disInfoEntity$vars[8], disInfoEntity$name[8], value="", width=NULL, placeholder=""),
              textInput(disInfoEntity$vars[9], disInfoEntity$name[9], value="", width=NULL, placeholder=""),
              textInput(disInfoEntity$vars[10], disInfoEntity$name[10], value="", width=NULL, placeholder=""),
              textInput(disInfoEntity$vars[11], disInfoEntity$name[11], value="", width=NULL, placeholder="")
            ),
            tabPanel(title = "Function",
              textInput(disInfoEntity$vars[12], disInfoEntity$name[12], value="", width=NULL, placeholder=""),
              textInput(disInfoEntity$vars[13], disInfoEntity$name[13], value="", width=NULL, placeholder=""),
              textInput(disInfoEntity$vars[14], disInfoEntity$name[14], value="information", width=NULL, placeholder=""),
              textInput(disInfoEntity$vars[15], disInfoEntity$name[15], value="Tidak Ada", width=NULL, placeholder=""),
              textInput(disInfoEntity$vars[16], disInfoEntity$name[16], value="Tidak Ada", width=NULL, placeholder=""),
              textInput(disInfoEntity$vars[17], disInfoEntity$name[17], value="distributor", width=NULL, placeholder="")
            )
          )
        ),
        ###input transfer option####
        tabPanel(appsMenu$menuItem[7],
          tabBox(width = "200px",
            tabPanel(title = "Transfer Options WFS",
              textInput(paste0(trfOptWFSEntity$vars[1]), trfOptWFSEntity$name[1], value="", width=NULL, placeholder=""),
              textInput(paste0(trfOptWFSEntity$vars[2]), trfOptWFSEntity$name[2], value="", width=NULL, placeholder=""),
              textInput(paste0(trfOptWFSEntity$vars[3]), trfOptWFSEntity$name[3], value="", width=NULL, placeholder=""),
              textInput(paste0(trfOptWFSEntity$vars[4]), trfOptWFSEntity$name[4], value="RAW", width=NULL, placeholder=""),
              textInput(paste0(trfOptWFSEntity$vars[5]), trfOptWFSEntity$name[5], value="", width=NULL, placeholder="")
            ),
            tabPanel(title = "Transfer Options WMS", 
              textInput(paste0(trfOptWMSEntity$vars[1]), trfOptWMSEntity$name[1], value="", width=NULL, placeholder=""),
              textInput(paste0(trfOptWMSEntity$vars[2]), trfOptWMSEntity$name[2], value="", width=NULL, placeholder=""),
              textInput(paste0(trfOptWMSEntity$vars[3]), trfOptWMSEntity$name[3], value="", width=NULL, placeholder=""),
              textInput(paste0(trfOptWMSEntity$vars[4]), trfOptWMSEntity$name[4], value="ImageWMS", width=NULL, placeholder=""),
              textInput(paste0(trfOptWMSEntity$vars[5]), trfOptWMSEntity$name[5], value="", width=NULL, placeholder="")
            ),
            tabPanel(title = "Transfer Options ZIP Shapefile",
              textInput(paste0(trfOptZIPEntity$vars[1]), trfOptZIPEntity$name[1], value="", width=NULL, placeholder=""),
              textInput(paste0(trfOptZIPEntity$vars[2]), trfOptZIPEntity$name[2], value="", width=NULL, placeholder=""),
              textInput(paste0(trfOptZIPEntity$vars[3]), trfOptZIPEntity$name[3], value="", width=NULL, placeholder=""),
              textInput(paste0(trfOptZIPEntity$vars[4]), trfOptZIPEntity$name[4], value="ZIP Shapefile", width=NULL, placeholder=""),
              textInput(paste0(trfOptZIPEntity$vars[5]), trfOptZIPEntity$name[5], value="", width=NULL, placeholder="")
            ),
            tabPanel(title = "Transfer Options ImageWFS", 
              textInput(paste0(trfOptIWMSEntity$vars[1]), trfOptIWMSEntity$name[1], value="", width=NULL, placeholder=""),
              textInput(paste0(trfOptIWMSEntity$vars[2]), trfOptIWMSEntity$name[2], value="", width=NULL, placeholder=""),
              textInput(paste0(trfOptIWMSEntity$vars[3]), trfOptIWMSEntity$name[3], value="", width=NULL, placeholder=""),
              textInput(paste0(trfOptIWMSEntity$vars[4]), trfOptIWMSEntity$name[4], value="ImageWMS", width=NULL, placeholder=""),
              textInput(paste0(trfOptIWMSEntity$vars[5]), trfOptIWMSEntity$name[5], value="", width=NULL, placeholder="")
            )
          )
        ),
        ###input metadata maintenance####
        tabPanel(appsMenu$menuItem[8],
          textInput(mdMtncEntity$vars[1], mdMtncEntity$name[1], value="asNeeded", width=NULL, placeholder=""),
          textInput(mdMtncEntity$vars[2], mdMtncEntity$name[2], value="Dibuat oleh Alat Bantu Kompilasi", width=NULL, placeholder="")
        ),
        ###input metadata constraints####
        tabPanel(appsMenu$menuItem[9],
          textInput(mdConstEntity$vars[1], mdConstEntity$name[1], value="", width=NULL, placeholder=""),
          textInput(mdConstEntity$vars[2], mdConstEntity$name[2], value="", width=NULL, placeholder=""),
          textInput(mdConstEntity$vars[3], mdConstEntity$name[3], value="", width=NULL, placeholder="")
        ),
        ###submit####
        "Save Data",
        tabPanel(
          actionButton("saveButton", "Save")
        ),
        tabPanel(
          downloadButton("reportTopology", "Download Topology Report")
        )
      )
    ),
    ###Data####
    tabPanel("Data", value="tabData", icon = icon("database"),
      dataTableOutput("comp_data")
    ),
    ###Edit Attribut KUGI####
    tabPanel("Edit Attribute Table", value="tabEditKugi", icon = icon("edit"),
      h1(textOutput("rawTitle")),
      hr(),
      dataTableOutput("editAttribute"),
      hr(),
      "Padu padan kolom atribut",
      uiOutput("listOfShpColumn"),
      uiOutput("listOfKugiAttrib"),
      actionButton("matchButton", "Match"),
      actionButton("finishMatchButton", "Finish")
    )
  ),

  ###About####
  tabPanel("About"
  )
)
