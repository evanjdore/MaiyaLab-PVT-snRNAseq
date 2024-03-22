fluidPage(
  dashboardPage(
    skin = "purple",  # Skin attribute goes here
    dashboardHeader(title = "MAIYA LAB - PVT"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Project Description", tabName = "project", icon = icon("house")),
        menuItem("Dimensional Reduction", tabName = "dimreduction", icon = icon("circle-nodes")),
        menuItem("Gene Expression", tabName = "expression", icon = icon("dna")),
        menuItem("Coexpression", tabName = "coexpression", icon = icon("user-group")),
        menuItem("Differential Expression", tabName = "diffexp", icon = icon("plus-minus")),
        menuItem("Known Subtype Markers", tabName = "knownmarkers", icon = icon("brain")),
        menuItem("Novel Subtype Markers", tabName = "novelmarkers", icon = icon("code-branch"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "project",
                h2("Project Description"),
                htmlOutput("html")
        ),
        tabItem(tabName = "dimreduction",
                h2("Dimensional Reduction"),
                fluidRow(
                  column(6,align="center",
                         selectInput("groupByIdentity1", "Group by for Plot 1:",
                                     choices = c("Known Subtypes" = "known_subtypes", 
                                                 "Novel Subtypes" = "novel_subtypes")),
                         submitButton(text = "Submit", icon("dna"), width = "50%"),
                         plotOutput("dimReductionPlot1", width = "75%", height = "500px")
                  ),
                  column(6,align="center",  # Second column for the second plot
                         selectInput("groupByIdentity2", "Group by for Plot 2:",
                                     choices = c("Novel Subtypes" = "novel_subtypes",
                                                 "Known Subtypes" = "known_subtypes")),
                         submitButton(text = "Submit", icon("dna"), width = "50%"),
                         plotOutput("dimReductionPlot2", width = "75%", height = "500px"))),
                fluidRow(
                  column(6, align = "center",
                         tags$img(src = "top10_knownsubtypes.png", style = "height: auto; width: 100%")),
                  column(6, align = "center",
                         tags$img(src = "top5_novelsubtypes.png", style = "height: auto; width: 100%"))
                         
                  )
                ),
        
        
        tabItem(tabName = "expression",
                h2("Gene Expression"),
                fluidRow(
                  column(3,align="center",
                         selectInput("groupByIdentityGeneExp", "Group by:",
                                     choices = c("Known Subtypes" = "known_subtypes",
                                                 "Novel Subtypes" = "novel_subtypes"))),
                  column(6,align="center",
                         textInput("geneName", "Enter Gene Name:", placeholder = "Type gene name here")),
                  
                  column(3, align = "center",
                         submitButton(text = "Submit", icon("dna"), width = "100%"))),
                fluidRow(
                  column(6,align="center",
                         plotOutput("dimReductionGeneExp", width = "100%")
                  ),
                  column(6,align="center",
                         plotOutput("geneExpViolinPlot", width = "100%")
                  )),
                fluidRow(
                  column(12,align="center",
                         dataTableOutput("genePositiveCellsTable")
                  )
                )
        ),
        
        
        tabItem(tabName = "coexpression",
                h2("Coexpression"),
                fluidRow(
                  column(3,align="center",
                         textInput("geneName2", "Enter Gene Name:", placeholder = "Type gene name here")),
                  column(3,align="center",
                         textInput("geneName3", "Enter Gene Name:", placeholder = "Type gene name here")),
                  
                  column(3, align = "center",
                         submitButton(text = "Submit", icon("dna"), width = "100%"))),
                fluidRow(
                  
                  column(4, align = "center",
                         plotOutput("coexpfig1", width = "100%")),
                  
                  column(4, align = "center",
                         plotOutput("coexpfig2", width = "100%")),
                  
                  column(4, align = "center",
                         plotOutput("coexpfig3", width = "100%"))),
                fluidRow(
                  
                  column(12,align="center",
                         selectInput("groupcoexp", "Group by:",
                                     choices = c("Known Subtypes" = "known_subtypes",
                                                 "Novel Subtypes" = "novel_subtypes"))),
                  column(12,align="center",
                         dataTableOutput("groupcoexptable")
                  )
                )
        ),
        tabItem(tabName = "diffexp",
                h2("Differential Expression"),
                fluidRow(
                  column(6,align="center",
                         textInput("geneName4", "Enter Gene Name:", placeholder = "Type gene name here")),
                  column(6, align = "center",
                         submitButton(text = "Submit", icon("dna"), width = "50%"))),
                fluidRow(
                  column(12, align = "center",
                       plotOutput("diffexpfig1", width = "100%")),
                fluidRow(
                  column(12, align = "center",
                         DT::dataTableOutput("diffexptable"))),
              
              
        )),
        
        tabItem(tabName = "knownmarkers",
                h2("Known Marker Genes"),
                fluidRow(
                  column(6,align="center",
                         selectInput("kmarkergroup", "PVT Subtype:",
                                     choices = c("PVT-1" = "PVT-1",
                                                 "PVT-2" = "PVT-2",
                                                 "PVT-3" = "PVT-3",
                                                 "PVT-4" = "PVT-4",
                                                 "PVT-5" = "PVT-5"))),
                  column(6, align = "center",
                         submitButton(text = "Submit", icon("dna"), width = "50%"))),
                fluidRow(
                  column(12,align="center",
                         DT::dataTableOutput("kmarkertab"))),
                fluidRow(
                  column(12, align = "center",
                         tags$img(src = "known_clustree_plot.png", style = "height: 500px; width: auto;")))
        ),
        
        tabItem(tabName = "novelmarkers",
                h2("Novel Marker Genes"),
                fluidRow(
                  column(6,align="center",
                         selectInput("nmarkergroup", "PVT Subtype:",
                                     choices = c("PVT-1A" = "PVT-1A",
                                                 "PVT-1B" = "PVT-1B",
                                                 "PVT-2A" = "PVT-2A",
                                                 "PVT-2B" = "PVT-2B",
                                                 "PVT-2C" = "PVT-2C",
                                                 "PVT-2D" = "PVT-2D",
                                                 "PVT-2E" = "PVT-2E",
                                                 "PVT-3" = "PVT-3",
                                                 "PVT-4" = "PVT-4",
                                                 "PVT-5" = "PVT-5"))),
                  column(6, align = "center",
                         submitButton(text = "Submit", icon("dna"), width = "50%"))),
                fluidRow(
                  column(12,align="center",
                         DT::dataTableOutput("nmarkertab"))),
                fluidRow(
                  column(12, align = "center",
                         tags$img(src = "novel_clustree_plot.png", style = "height: 500px; width: auto;"))
                )
        )
      ))))
