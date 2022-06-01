#https://www.r-bloggers.com/2019/07/excel-report-generation-with-shiny/
library("ERICDataProc")
library("openxlsx")
library("readxl")
library("stringr")

version_no <- "Version 0.1"
options(shiny.maxRequestSize=3000*1024^2)


  # minimal Shiny UI
ui <- fluidPage(
  titlePanel("Input checker"),
  textOutput("version"),
  tags$br(),

  fileInput("slafile", "Choose Input File",
            accept = c(
              "application/vnd.ms-excel",
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
              ".xlsx")
  ),

  radioButtons(inputId = "dataformat",label="Data source",c("Standard"="std","iRecord"="irecord","ERIC website"="eric","BirdTrack"="bt")),

  textInput(inputId = "recordername",label = "Recorder name (for BirdTrack data)",value = ""),
  checkboxInput(inputId = "locCheck",label = "Check for blank locations?",FALSE),

  textInput(inputId = "outputfile",label = "Output filename",value = ""),
  textOutput(outputId = "msg"),



  downloadButton(
    outputId = "okBtn",
    label = "Check input data")


)

# minimal Shiny server
server <- function(input, output) {

  output$version <- renderText(version_no)
  output$okBtn <- downloadHandler(
    filename = function() {
      ifelse(str_ends(input$outputfile,'xlsx'),input$outputfile,paste0(input$outputfile,'.xlsx'))

    },
    content = function(file) {

      input_config <- setup_input_config_values()
      #Create output workbook
      XL_wb <- createWorkbook()


      inFile <- input$slafile

      if (is.null(inFile))
        return(NULL)


      #Assume only a single sheet
      input_data <- read_excel(inFile$datapath,sheet = 1, col_names = TRUE,col_types = "text")



      #Perform checks

      if (!(input$dataformat == "bt" && input$recordername == "")) {
        #Rename the columns before we start & check we have the ones we need
        raw_data <- rename_columns(input_data,input$dataformat)
        if (check_required_columns(raw_data,input$dataformat)) {

          #Format the date
          raw_data$Date <- formatDates(raw_data$Date)
          browser()
          outputData <- format_and_check_input_data(raw_data,input$locCheck,input$dataformat,input$recordername)

          #Output the data
          sheet_name = 'Checked data'
          XL_wb <- openxlsx::createWorkbook()


          XL_wb <- format_input_Excel_output(XL_wb,sheet_name,outputData,input_config)


          openxlsx::saveWorkbook(XL_wb,file,overwrite = TRUE)


        } 
      } 


    }
  )


}

shinyApp(ui, server)
