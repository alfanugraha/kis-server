navbarPage(title = appsTitle, theme = shinytheme("flatly"), id="compilationApps",
  ###Home####
  tabPanel("Home", value="tabHome",
    # slickROutput("slideshow", width="100%"),
    # hr(),
    jumbotron("KIS SATU PETA", "Mempermudah proses penatagunaan lahan. Menghindari konflik penatagunaan lahan. Mempercepat proses perizinan penatagunaan lahan.", button=FALSE),
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
    ###Data####
    tabPanel("Otorisasi Data", value="tabData", icon = icon("database"),
      dataTableOutput("app_data")
    ),
    tabPanel("Kompilasi Data", value="tabData", icon = icon("database"),
      dataTableOutput("comp_data")
    )
  ),

  ###About####
  tabPanel("Tentang"
  )
)
