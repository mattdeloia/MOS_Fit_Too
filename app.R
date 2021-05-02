library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(readxl)
library(tidyverse)
library(DT)
library(shinyjs)
shinyWidgetsGallery()
library(ggimage)
library(plotly)
library(bslib)
library(thematic)
library(shinyAce)
library(mailR)
library(blastula)


#read in data reference
df <- read_excel("MOS_ReferenceFile.xlsx")
df_b <-read_excel("MOSList.xlsx") 

scale_I <- c("start", "Very low", "Low", "Moderate", "High", "Very high")
scale_P <- c("start", "Well below average", "Below average", "Average", "Above average", "Well above average")
scale_S <- c("start","D", "C-","C", "C+", "B-","B","B+", "A-", "A", "A+")
scale_Phy <- c("start","Well below average", "Below average", "Average", "Above average", "Well above average")
skills <- c("animal care", "auto repair", "computer science", "construction", "culinary arts", "finance", "fire rescue", "health care", "languages", "legal services", "law enforcement", "music", "photography (visual arts)", "psychology", "religious affairs", "scuba diving" )
pass <- "xx"
thematic_on()
onStop(thematic_off)
#################################################################
ui <- navbarPage(
      id="wizard",
      theme = shinytheme("united"),
    
      title="MOS Fit Tool",
      header=tagList(useShinydashboard()),
      position = "fixed-top",
      
        tabPanel("Welcome", icon = icon("flag-usa"),br(), br(), br(),              
            box(title="Explore Army Specialties in 10 Minutes!", status="success",  solidHeader = TRUE, width = 12, footer = em("Version: 0.9", style="color:darkgray"),
            h4("Welcome to the MOS Fit reporting tool.  This tool helps a recruit navigate the Army's 190 occupational specialties to find those well-suited to their own interests, personality traits, and abilities.  MOS 'Fit' calculations are based on your self-assessment entered here.", style="color:blue"), br(),
           h4("FIRST, complete the Interest, Personality, and Skill questionnaires.  Use the BUTTONS at the top or bottom of the screen to navigate."), 
            h4("SECOND, review and download personalized results within the 'MOS Explorer', 'Report' and 'Vis' pages."), br(),
            helpText("You will use slider bars, like the one below, to complete the questionnaire. Response options are defined on the scales and not continuous, with the exception of reporting your ASVAB scores where you can select any point on the full scale.  Please respond honestly to all features of the questionnaire for accurate results."),
            br(),
            sliderTextInput("demo", "XX. SPORTS Example: Rate your Interest in watching sports such as basketball, football, and soccer.", choices = scale_I, grid=TRUE),
           em("Note: Your entries and personalized results are deleted upon exit from this tool. ‘Fit Scores' are for demonstration purposes only and based on available open and published sources. Additional research and data are needed for this prototype v0.9 to be operationally tested and validated. This prototype is limited in its analysis to 120 specialties.", style="color:red"), br(),br(),
            #actionBttn("page_21" ,"prev", icon = icon("backward"), style="simple", color="success", size="md"),
            actionBttn("page_23" ,"next", icon = icon("forward"), style="simple", color="success", size="md") )
          ),
    
    tabPanel("Interest", icon = icon("table"),
             br(), br(), br(),
             actionBttn("page_32" ,"prev", icon = icon("backward"), style="simple", color="success", size="md"),
             actionBttn("page_34" ,"next", icon = icon("forward"), style="simple", color="success", size="md"),
            br(), br(),
        
            box(title="Interests", status="primary",  solidHeader = TRUE, width = 12, footer = em("Version: 0.9", style="color:darkgray"),
            helpText("INSTRUCTIONS: Rate your interest in nine (9) vocational areas using the slider bars; please distribute your responses across the scale.", style="color:blue"),
            sliderTextInput("construction", "1. CONSTRUCTION: designing and/or building things or maintaining structures with one's hands or using tools and materials. ", choices = scale_I, selected="start" , grid=TRUE),
            sliderTextInput("protection", "2. PROTECTION: guarding, ensuring safety, and enforcing rules and laws. Operating weapons and equipment in military operations; performing reconnaissance operations.", choices = scale_I, selected="start", grid=TRUE ),
            sliderTextInput("mechanical", "3. MECHANICAL: building, maintaining, repairing and using small and large machinery, including driving and operating heavy equipment or large vehicles. ", choices = scale_I, selected="start", grid=TRUE ),
            sliderTextInput("outdoor", "4. OUTDOOR: working in the outdoors.", choices = scale_I, selected="start", grid=TRUE),
            sliderTextInput("medical", "5. MEDICAL: applying medical knowledge and skills to the diagnosis, prevention, and treatment of disease and injury.", choices = scale_I, selected="start" , grid=TRUE),
            sliderTextInput("mathematics", "6. MATHEMATICS: working with data and applying quantitative and statistical concepts and mathematical formulas.", choices = scale_I, selected="start", grid=TRUE ),
            sliderTextInput("information", "7. INFORMATION TECHNOLOGY: Developing, maintaining, and using computer systems, software, and networks for the processing and distribution of data.", choices = scale_I, selected="start", grid=TRUE ),
            sliderTextInput("leadership", "8. LEADERSHIP: Leading others and influencing people and decisions.", choices = scale_I, grid=TRUE),
            sliderTextInput("office", "9. OFFICE WORK: Performing clerical, administrative, and business related activities.", choices = scale_I, grid=TRUE) )
    ),
        
    tabPanel("Personality", icon = icon("table"),
             br(), br(), br(),
             actionBttn("page_43" ,"prev", icon = icon("backward"), style="simple", color="success", size="md"),
             actionBttn("page_45" ,"next", icon = icon("forward"), style="simple", color="success", size="md"),
             br(), br(),
            
             box(title="Personality", status="primary",  solidHeader = TRUE, width = 12, footer = em("Version: 0.9", style="color:darkgray"),
             helpText("INSTRUCTIONS: Rate your expression of nine (9) personality traits using the slider bars. It may be helpful to compare yourself to school mates, friends and work colleagues to decide your placement on the scale.", style="color:blue"),
             sliderTextInput("adjustment", "1. ADJUSTMENT: well adjusted, worry free, and handle stress well.", choices = scale_P, selected="start", grid=TRUE),
             sliderTextInput("adventure", "2. ADVENTURE SEEKING: enjoy participating in extreme sports and outdoor activities.", choices = scale_P, selected="start", grid=TRUE ),
             sliderTextInput("attention", "3. ATTENTION SEEKING: tend to engage in behaviors that attract social attention. They are loud, loquacious, entertaining, and even boastful.", choices = scale_P, selected="start" , grid=TRUE),
             sliderTextInput("dominance", "4. DOMINANCE: domineering, “take charge” and are often referred to by their peers as natural leaders.", choices = scale_P, selected="start" , grid=TRUE ),
             sliderTextInput("intellectual", "5. INTELLECTUAL EFFICIENCY: believe they process information and make decisions quickly; they see themselves (and they may be perceived by others) as knowledgeable, astute, or intellectual.", choices = scale_P, selected="start" , grid=TRUE),
             sliderTextInput("nondelinquency", "6. NONDELINQUENCY: tend to comply with rules, customs, norms, and expectations, and they tend not to challenge authority.", choices = scale_P, selected="start",  grid=TRUE ),
             sliderTextInput("order", "7. ORDER: tend to organize tasks and activities and desire to maintain neat and clean surroundings.", choices = scale_P, selected="start" ,  grid=TRUE),
             sliderTextInput("physical", "8. PHYSICAL CONDITIONING: tend to engage in activities to maintain their physical fitness and are more likely participate in vigorous sports or exercise.", choices = scale_P, selected="start", grid=TRUE ),
             sliderTextInput("tolerance", "9. TOLERANCE: interested in other cultures and opinions that may differ from their own; they are willing to adapt to novel environments and situations.", choices = scale_P,  selected = "start", grid=TRUE, force_edges = TRUE) )
        ),
                 
     tabPanel("Skills", icon = icon("table"), 
              br(), br(), br(),
              actionBttn("page_54" ,"prev", icon = icon("backward"), style="simple", color="success", size="md"),
              actionBttn("page_56" ,"next", icon = icon("forward"), style="simple", color="success", size="md"),
              br(), br(),
         
            box(title="ASVAB Results", status="primary",  solidHeader = TRUE, width = 12, footer = em("Version: 0.9", style="color:darkgray"),
            helpText("INSTRUCTIONS: Enter sub-test scores from the ASVAB.", style="color:blue"),
            sliderInput("gt", "ASVAB-GT (General Technical)", min=70, max=140, value=70, step=1),
            sliderInput("st", "ASVAB-ST (Science and Technology)", min=70, max=140, value=70, step=1),
            sliderInput("mm", "ASVAB-MM (Mechanical Maintenance)", min=70, max=140, value=70, step=1),
            sliderInput("el", "ASVAB-EL (Electronics)", min=70, max=140, value=70, step=1),
            # sliderInput("cl", "ASVAB-CL (Clerical)", min=70, max=140, value=100, step=1),
            # sliderInput("fa", "ASVAB-FA (Field Artillery)", min=70, max=140, value=100, step=1),
            # sliderInput("co", "ASVAB-CO (Combat)", min=70, max=140, value=100, step=1),
            # sliderInput("gm", "ASVAB-GM (General Maintenance)", min=70, max=140, value=100, step=1),
            # sliderInput("sc", "ASVAB-SC (Surveillance and Communication)", min=70, max=140, value=100, step=1),
            # sliderInput("of", "ASVAB-OF (Food and Operators)", min=70, max=140, value=100, step=1) 
              ),
              
            box(title="Secondary Education", status="primary",  solidHeader = TRUE, width = 12, footer = em("Version: 0.9", style="color:darkgray"),
            helpText("INSTRUCTIONS: Enter the average grades you achieved in High School for four core areas.", style="color:blue"),
            sliderTextInput("math", "1. Mathematics (algebra, geometry, trigonometry, calculus)", choices = scale_S, selected="start", grid=TRUE),
            sliderTextInput("science", "2. Science (biology, chemistry, physics)", choices = scale_S, selected="start", grid=TRUE),
            sliderTextInput("social", "3. Social Studies (economics, geography, government, history)", choices = scale_S, selected="start", grid=TRUE),
            sliderTextInput("language", "4. Language Arts (literature, speech, writing)", choices = scale_S, selected="start", grid=TRUE)
           ), 
           
           box(title="Physical Fitness", status="primary",  solidHeader = TRUE, width = 12, footer = em("Version: 0.9", style="color:darkgray"),
               helpText("INSTRUCTIONS: Rate your current physical fitness based on muscular strength, endurance from regular exercise, and proper nutrition.  In your rating, consider a comparison to others you may know in the military, law enforcement, and other occupations requiring physical activities.", style="color:blue"),
               sliderTextInput("fitness", "1. Physical Ability", choices = scale_Phy, selected="start", grid=TRUE)
           ),
           
            box(title="Previous Work Experience and Skills", status="primary",  solidHeader = TRUE, width = 12, footer = em("Version: 0.9", style="color:darkgray"),
                helpText("INSTRUCTIONS: Check the skills that you have developed extensively through previous workforce experience, college education, or practice from hobbies.", style="color:blue"),
                awesomeCheckboxGroup("specialskills", "List of Special Skills", choices = skills),
              # verbatimTextOutput(outputId = "Skills"),
               )
     ),
    
    tabPanel("MOS_Explorer", icon = icon("dashboard"), 
             br(), br(), br(), 
             actionBttn("page_65" ,"prev", icon = icon("backward"), style="simple", color="success", size="md"),
             actionBttn("page_67" ,"next", icon = icon("forward"), style="simple", color="success", size="md"),
             br(),
             h5("This page includes 12 career areas to organize your results. Your 'Fit Scores' are used to rank order specialties within each area.", style="color:blue"),
             helpText("The ASVAB column notes if your scores 'meet' MOS requirements or are 'flagged' as below the minimum. Abbreviations: GT = General Technical; ST = Science and Technology; MM = Mechanical Maintenance; EL = Electronics.", style="color:blue"),
             em("Note: ‘Fit Scores' are for demonstration purposes only and based on available open and published sources. Additional research and data are needed for this prototype v0.9 to be operationally tested and validated.", style="color:red"),
             br(), br(),
  
            box(title="Administrative", status= "primary",
              dataTableOutput("table_Administrative")
            ), 
            
            box(title="Arts and Media", status= "primary",
                dataTableOutput("table_Arts")
            ),

            box(title="Aviation", status= "primary",
                  dataTableOutput("table_Aviation")
            ), 
            
            box(title="Combat", status= "primary",
                dataTableOutput("table_Combat")
            ),
            
            box(title="Combat Support", status= "primary",
                 dataTableOutput("table_Combatsupport")
            ),
            
            box(title="Computers and Technology", status= "primary",
                 dataTableOutput("table_Computers")
            ),
            
            box(title="Construction and Engineering", status= "primary",
                  dataTableOutput("table_Construction")
            ),
            
            box(title="Intelligence", status= "primary",
                  dataTableOutput("table_Intelligence")
            ),
            
            box(title="Legal and Law Enforcement", status= "primary",
                  dataTableOutput("table_Legal")
            ),
            
            box(title="Mechanics", status= "primary",
                 dataTableOutput("table_Mechanics")
            ),
            
            box(title="Medical", status= "primary",
                 dataTableOutput("table_Medical")
            ),
            
            box(title="Transportation", status= "primary",
                dataTableOutput("table_Transportation")
            )

            ),
    
    tabPanel("Report", icon = icon("bar-chart-o"), 
             br(), br(), br(),
             actionBttn("page_76" ,"prev", icon = icon("backward"), style="simple", color="success", size="md"),
               actionBttn("page_78" ,"next", icon = icon("forward"), style="simple", color="success", size="md"), br(), br(),
            # downloadButton("downloadReport", "Download Report"),
            dataTableOutput("IndividualReport") ) ,
    
    tabPanel("Vis", icon = icon("chart-line"), 
             br(), br(), br(),
             actionBttn("page_87" ,"prev", icon = icon("backward"), style="simple", color="success", size="md"),
             actionBttn("page_89" ,"next", icon = icon("forward"), style="simple", color="success", size="md"), 
            br(), br(),
            textInput("to", "-DISABLED- (will not function) Email address:", value="to@gmail.com"),
            actionButton("mailButton", "-DISABLED- Email report", icon = icon("envelope")),
            downloadButton("downloadVis", "Download Vis"),
            br(),br(),
            plotOutput("plot1", height = "600px") 
             ),
    
    tabPanel("Vis2", icon = icon("chart-line"), br(), br(), br(),
             actionBttn("page_98" ,"prev", icon = icon("backward"), style="simple", color="success", size="md"),
             br(),
             sliderInput("cutoff", "Fit score threshold value", min=0, max=1, value=.5, step=.01),
             plotlyOutput("plot2") 
            ),
    
    tabPanel("Info", icon = icon("info-circle"), 
             br(), br(), br(), br(),
             
             box(title="Project Review", status="success", icon = icon("info-circle"), solidHeader = TRUE, width = 12, footer = em("Version: 0.9", style="color:darkgray"),
                 em("Please provide feedback and recommendations to xx.", style="color:green"),
                 h4("Overview", style="color:black"),
                 p("This 'MOS Fit' reporting tool is designed to leverage assessments across interest, personality, and aptitude.  The tool design resembles data already collected upon entry to the Army through the Armed Services Vocational Aptitude Battery (ASVAB), the Tailored Adaptive Personality Assessment System (TAPAS), and potentially soon with the Adaptive Vocational Interest Diagnostic (AVID). Full results from these assessments can be directly entered into this tool for better accuracy.  This tool can launch on-line for wide use by Army recruiters and recruits."),
                 p(" Results and data presented in the 'MOS Explorer' and 'Report' features were informed by the goarmy.com pages for careers and jobs. ‘Fit Scores' are for demonstration purposes only and based on available open and published sources. Additional research and data are needed for this prototype v0.9 to be operationally tested and validated."),
                 br(),
                 h4("Background", style="color:black"),    
                 p("The 2019 publication of The Army People Strategy describes the aim to place the right people in the right place at the right time.  Key implementation actions have included leader assessment programs, such as the Battalion Commander Assessment Program.  One near term priority is to 'attract and align new Soldiers with best-fit jobs across the Army, which will reduce attrition.' The Strategy lists experimentation and piloting of new innovations as a way to effective solutions."),
                 p("Based on the suite of assessments performed on Army recruits, an opportunity exists to use this information about one's knowledge, skills and behaviors (KSB). A specially tailored MOS report, prototyped here, could increase a recruit's understanding of Army specialties that align well to their talents and behaviors, potentially leading to longer, more satisfying Service for the tens of thousands that enlist into the Army each year."),

                 br(), br(),
                 #actionBttn("page_12" ,"next", icon = icon("forward"), style="simple", color="success", size="md")
             )
    )
     )
#####################################################################
server <- function(input, output, session) {


  
# Interest outputs
        I1 <- reactive({ifelse(input$construction=="Very low", 1, ifelse(input$construction=="Low", 1.5, ifelse(input$construction=="Moderate", 2, ifelse(input$construction=="High", 2.5, ifelse(input$construction =="Very high", 3, 0)))))})
        I2 <- reactive({ifelse(input$protection=="Very low", 1, ifelse(input$protection=="Low", 1.5, ifelse(input$protection=="Moderate", 2, ifelse(input$protection=="High", 2.5, ifelse(input$protection =="Very high", 3, 0)))))})
        I3 <- reactive({ifelse(input$mechanical=="Very low", 1, ifelse(input$mechanical=="Low", 1.5, ifelse(input$mechanical=="Moderate", 2, ifelse(input$mechanical=="High", 2.5, ifelse(input$mechanical =="Very high", 3, 0)))))})
        I4 <- reactive({ifelse(input$outdoor=="Very low", 1, ifelse(input$outdoor=="Low", 1.5, ifelse(input$outdoor=="Moderate", 2, ifelse(input$outdoor=="High", 2.5, ifelse(input$outdoor =="Very high", 3, 0)))))})
        I5 <- reactive({ifelse(input$medical=="Very low", 1, ifelse(input$medical=="Low", 1.5, ifelse(input$medical=="Moderate", 2, ifelse(input$medical=="High", 2.5, ifelse(input$medical =="Very high", 3, 0)))))})
        I6 <- reactive({ifelse(input$mathematics=="Very low", 1, ifelse(input$mathematics=="Low", 1.5, ifelse(input$mathematics=="Moderate", 2, ifelse(input$mathematics=="High", 2.5, ifelse(input$mathematics =="Very high", 3, 0)))))})
        I7 <- reactive({ifelse(input$information=="Very low", 1, ifelse(input$information=="Low", 1.5, ifelse(input$information=="Moderate", 2, ifelse(input$information=="High", 2.5, ifelse(input$information =="Very high", 3, 0)))))})
        I8 <- reactive({ifelse(input$leadership=="Very low", 1, ifelse(input$leadership=="Low", 1.5, ifelse(input$leadership=="Moderate", 2, ifelse(input$leadership=="High", 2.5, ifelse(input$leadership =="Very high", 3, 0)))))})
        I9 <- reactive({ifelse(input$office=="Very low", 1, ifelse(input$office=="Low", 1.5, ifelse(input$office=="Moderate", 2, ifelse(input$office=="High", 2.5, ifelse(input$office =="Very high", 3, 0)))))})
        output$Interest1 <- renderPrint(I9() )
      
# Personality outputs
        P1 <- reactive({ifelse(input$adjustment=="Well below average", 1, ifelse(input$adjustment=="Below average", 1.5, ifelse(input$adjustment=="Average", 2, ifelse(input$adjustment=="Above average", 2.5, ifelse(input$adjustment =="Well above average", 3, 0)))))})
        P2 <- reactive({ifelse(input$adventure=="Well below average", 1, ifelse(input$adventure=="Below average", 1.5, ifelse(input$adventure=="Average", 2, ifelse(input$adventure=="Above average", 2.5, ifelse(input$adventure =="Well above average", 3, 0)))))})
        P3 <- reactive({ifelse(input$attention=="Well below average", 1, ifelse(input$attention=="Below average", 1.5, ifelse(input$attention=="Average", 2, ifelse(input$attention=="Above average", 2.5, ifelse(input$attention =="Well above average", 3, 0)))))})
        P4 <- reactive({ifelse(input$dominance=="Well below average", 1, ifelse(input$dominance=="Below average", 1.5, ifelse(input$dominance=="Average", 2, ifelse(input$dominance=="Above average", 2.5, ifelse(input$dominance =="Well above average", 3, 0)))))})
        P5 <- reactive({ifelse(input$intellectual=="Well below average", 1, ifelse(input$intellectual=="Below average", 1.5, ifelse(input$intellectual=="Average", 2, ifelse(input$intellectual=="Above average", 2.5, ifelse(input$intellectual =="Well above average", 3, 0)))))})
        P6 <- reactive({ifelse(input$nondelinquency=="Well below average", 1, ifelse(input$nondelinquency=="Below average", 1.5, ifelse(input$nondelinquency=="Average", 2, ifelse(input$nondelinquency=="Above average", 2.5, ifelse(input$nondelinquency =="Well above average", 3, 0)))))})
        P7 <- reactive({ifelse(input$order=="Well below average", 1, ifelse(input$order=="Below average", 1.5, ifelse(input$order=="Average", 2, ifelse(input$order=="Above average", 2.5, ifelse(input$order =="Well above average", 3, 0)))))})
        P8 <- reactive({ifelse(input$physical=="Well below average", 1, ifelse(input$physical=="Below average", 1.5, ifelse(input$physical=="Average", 2, ifelse(input$physical=="Above average", 2.5, ifelse(input$physical =="Well above average", 3, 0)))))})
        P9 <- reactive({ifelse(input$tolerance=="Well below average", 1, ifelse(input$tolerance=="Below average", 1.5, ifelse(input$tolerance=="Average", 2, ifelse(input$tolerance=="Above average", 2.5, ifelse(input$tolerance =="Well above average", 3, 0)))))})
        output$Personality1 <- renderPrint(P9())
        
        
# Skill Test Scores    
        
        Math <- reactive({ ifelse(input$math=="A+", 1, ifelse(input$math=="A",.95, ifelse(input$math=="A-",.93, ifelse(input$math=="B+",.9, ifelse(input$math=="B", .85, ifelse(input$math=="B-",.83, ifelse(input$math=="C+",.80, ifelse(input$math=="C",.75, ifelse(input$math=="C-", .7, ifelse(input$math=="D", .6, .5) ))))))))) })
        Sci <- reactive({ ifelse(input$science=="A+", 1, ifelse(input$science=="A",.95, ifelse(input$science=="A-",.93, ifelse(input$science=="B+",.9, ifelse(input$science=="B", .85, ifelse(input$science=="B-",.83, ifelse(input$science=="C+",.80, ifelse(input$science=="C",.75, ifelse(input$science=="C-", .7, ifelse(input$science=="D", .6, .5) ))))))))) })
        Social <- reactive({ ifelse(input$social=="A+", 1, ifelse(input$social=="A",.95, ifelse(input$social=="A-",.93, ifelse(input$social=="B+",.9, ifelse(input$social=="B", .85, ifelse(input$social=="B-",.83, ifelse(input$social=="C+",.80, ifelse(input$social=="C",.75, ifelse(input$social=="C-", .7, ifelse(input$social=="D", .6, .5) ))))))))) })
        Language <- reactive({ ifelse(input$language=="A+", 1, ifelse(input$language=="A",.95, ifelse(input$language=="A-",.93, ifelse(input$language=="B+",.9, ifelse(input$language=="B", .85, ifelse(input$language=="B-",.83, ifelse(input$language=="C+",.80, ifelse(input$language=="C",.75, ifelse(input$language=="C-", .7, ifelse(input$language=="D", .6, .5) ))))))))) })
        
        Fitness <-  reactive({ifelse(input$fitness=="Well below average", 1, ifelse(input$fitness=="Below average", 1.5, ifelse(input$fitness=="Average", 2, ifelse(input$fitness=="Above average", 2.5, ifelse(input$fitness =="Well above average", 3, 0)))))})
        
        output$Skills <- renderPrint(str(input$specialskills))
        
df2 <- reactive({ df %>% 
        mutate(I_score= 1-( abs(Construction-I1())+
                                abs(Protection-I2())+
                                abs(Mechanical-I3())+
                                abs(Outdoor-I4())+
                                abs(Medical-I5())+
                                abs(Mathematics-I6())+
                                abs(Information-I7())+
                                abs(Leadership-I8())+
                                abs(Office-I9()) )/(9*2),
               P_score= 1-( abs(Adjustment-P1())+
                                abs(Adventure-P2())+
                                abs(Attention-P3())+
                                abs(Dominance-P4())+
                                abs(Intellectual-P5())+
                                abs(NonDelinquency-P6())+
                                abs(Order-P7())+
                                abs(Physical-P8())+
                                abs(Tolerance-P9()))/(9*2),
               Phy_score = 1 - (abs(ACFT - Fitness())/(1*2)),
               S_score= ( 
                   ifelse(GT>100, input$gt/GT, 1) +
                   ifelse(ST>100, input$st/ST, 1)+
                   ifelse(MM>100, input$mm/MM, 1)+
                   ifelse(EL>100, input$el/EL, 1)+
                   # ifelse(CL>0, input$cl/CL, 1)+
                   # ifelse(CO>0, input$co/CO, 1)+
                   # ifelse(GM>0, input$gm/GM, 1)+
                   # ifelse(SC>0, input$sc/SC, 1)+
                   # ifelse(OF>0, input$sc/OF, 1) +
                   .5*(ifelse(HS_Math=="Y", Math(), 1)) +
                    .5*(ifelse(HS_Science=="Y", Sci(), 1)) +
                    .5*(ifelse(HS_SocialStudies=="Y", Social(), 1)) +
                    .5*(ifelse(HS_LanguageArts=="Y", Language(), 1)))/6 +
                     if_else( Skills %in% input$specialskills, .40, 0) ,
               ASVAB = if_else(input$gt<GT, "Flag GT-Score", if_else(input$st<ST, "Flag ST-Score", if_else(input$mm<MM, "Flag MM-Score", if_else(input$el<EL, "Flag EL-Score", "Met")))),
               Compo = (if_else(RA =="Y" & AR =="N" & NG=="Y", "AR,NG", if_else(RA =="N" & AR =="Y" & NG=="Y", "AR, NG", "All"))),
               Fit_Score=round((I_score+P_score+ Phy_score + S_score)/4 ,2),
               image = if_else(Bonus=="Y", "check.png","check.png" )) %>%
                
                arrange(desc(Fit_Score)) %>% 
                mutate(rank = rank(-Fit_Score, ties.method = "first"))
             })

#Function of MOS Tables by Category
MOSTable <- function (category) {df2() %>% 
    filter(Category==category) %>%
    select(Fit_Score, MOS, Title, ASVAB) %>%
    datatable(class = "display compact", 
              rownames= FALSE, 
              options = list (
                pageLength = 5 ))  %>%
    formatStyle ('Fit_Score', backgroundColor = styleInterval (c(.7, .8) , c("blank","lightyellow", "lightgreen")) ) %>%
    formatStyle("ASVAB", color = styleEqual(levels = c ("Met", "Flag GT-Score", "Flag ST-Score", "Flag MM-Score", "Flag EL-Score"), values = c("green", "red", "red","red","red"))  )
}

    output$table_Combat <- renderDataTable ({ 
        MOSTable("Combat")
    })
    
    output$table_Administrative <- renderDataTable ({ 
        MOSTable("Administrative Support")
    })
    
    output$table_Arts <- renderDataTable ({ 
        MOSTable("Arts and Media") 
    })
        
    output$table_Intelligence <- renderDataTable ({ 
       MOSTable("Intelligence")
    })
    
    output$table_Combatsupport <- renderDataTable ({ 
         MOSTable("Combat Support") 
    })    
    
    output$table_Legal <- renderDataTable ({ 
        MOSTable("Legal and Law Enforcement") 
    })
    
    output$table_Mechanics <- renderDataTable ({ 
        MOSTable("Mechanics")
    })
        
    output$table_Computers <- renderDataTable ({ 
        MOSTable("Computers and Technology") 
    })
        
    output$table_Medical <- renderDataTable ({ 
        MOSTable("Medical") 
    })
        
    output$table_Construction <- renderDataTable ({ 
        MOSTable("Construction and Engineering") 
    })
        
    output$table_Transportation <- renderDataTable ({ 
        MOSTable("Transportation") 
    })
    
    output$table_Aviation <- renderDataTable ({ 
         MOSTable("Aviation") 
    })
 
  output$IndividualReport <- renderDataTable (
      df2()  %>% 
        filter(Fit_Score>0) %>% 
        select(Fit_Score, MOS, Title, AIT_wks, Bonus, Clearance, Special_Rqmts, ASVAB,Compo, Description) %>% 
        datatable(extensions = c('Buttons'),
        rownames= FALSE,
        options = list(dom = 'Bfrtip',
                        columnDefs=list(list(targets=0:8, class="dt-center")),
                        buttons = c('copy', 'csv', 'excel', 'pdf')) ) %>%
        formatStyle("ASVAB", color = styleEqual(levels = c ("Met", "Flag GT-Score", "Flag ST-Score", "Flag MM-Score", "Flag EL-Score"), values = c("green", "red", "red","red","red"))  )
            )
output$plot1 <- renderPlot({
    df2() %>% filter(rank<=25) %>% 
      ggplot(aes(x=reorder(MOS, Fit_Score, FUN=max), y=Fit_Score, color=ASVAB)) +
      geom_col(fill="skyblue") +
    scale_shape_manual(values = c(4, 1)) +
      geom_point(aes(shape=Bonus), size=3, color="black")+
    geom_hline(yintercept = .70, linetype="dashed", color="green") +
      theme(legend.title= element_text(color="black", size=10), legend.position = "right") +
    geom_text (aes (label=Title, hjust=1.1, vjust=.5), color="black", size = 3) +
    geom_text (aes (y=.70, x=0, label=("good fit line"), hjust=1, vjust=-.5), color="black", size = 3, angle=0) +
      coord_flip() +
    ylim(0,1) +
      xlab("") +
      ylab("Fit Score") +
      scale_color_manual("ASVAB Minimums", values=c("Met"="lightblue", "Flag GT-Score"="red", "Flag ST-Score"="red", "Flag EL-Score"="blue", "Flag MM-Score"="green")) +
      ggtitle("Fit Results: Top 25 Specialties")
})

df3 <-  reactive({ df2() %>% filter(Fit_Score>input$cutoff) %>%  
    select(MOS, Title, Category) %>% 
    rbind(df_b) 
     })

output$plot2 <- renderPlotly({

  fig <- plot_ly()
  fig %>% add_trace(
    type="sunburst",
    ids=df3()$MOS,
    labels=df3()$Title,
    parents=df3()$Category,
    domain=list(column=1),
    maxdepth=2,
    insidetextorientation='radial'
  )
  })

IndividualReport <- reactive ({ 
      df2() %>% filter(rank<=25) %>% 
      ggplot(aes(x=reorder(MOS, Fit_Score, FUN=max), y=Fit_Score, color=ASVAB, shape=)) +
      geom_col(fill="skyblue") +
    geom_point(aes(shape=Bonus), size=3, color="black") +
    scale_shape_manual(values = c(4, 1)) +
      geom_hline(yintercept = .70, linetype="dashed", color="green") +
      theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
      geom_text (aes (label=Title, hjust=1.1, vjust=.5), color="black", size = 3) +
      geom_text (aes (y=.70, x=0, label=("good fit line"), hjust=1, vjust=-.5), color="black", size = 3, angle=0) +
      coord_flip() +
      xlab("") +
    ylim(0,1) +
      ylab("Fit Score") +
      scale_color_manual("ASVAB Minimums", values=c("Met"="lightblue", "Flag GT-Score"="red", "Flag ST-Score"="red", "Flag EL-Score"="blue", "Flag MM-Score"="green")) +
    ggtitle("Fit Results: Top 25 Specialties")
      })

output$downloadVis<- downloadHandler(
  filename = function(){ 
    paste("Vis_Report", "pdf", sep=".")
  },
  content= function(file) {
    ggsave(file, plot=IndividualReport(), device="pdf", width = 8.5, height = 11) 
    } )
    
    switch_page <- function(i) {
      updateNavbarPage(session, "wizard", selected = paste0(i))
    }
    
    observeEvent(input$page_12, switch_page("Instructions"))
    observeEvent(input$page_21, switch_page("Introduction"))
    observeEvent(input$page_23, switch_page("Interest"))
    observeEvent(input$page_32, switch_page("Instructions"))
    observeEvent(input$page_34, switch_page("Personality"))
    observeEvent(input$page_43, switch_page("Interest"))
    observeEvent(input$page_45, switch_page("Skills"))
    observeEvent(input$page_54, switch_page("Personality"))
    observeEvent(input$page_56, switch_page("MOS_Explorer"))
    observeEvent(input$page_65, switch_page("Skills"))
    observeEvent(input$page_67, switch_page("Report"))
    observeEvent(input$page_76, switch_page("MOS_Explorer"))
    observeEvent(input$page_78, switch_page("Vis"))
    observeEvent(input$page_87, switch_page("Report"))
    observeEvent(input$page_89, switch_page("Vis2"))
    observeEvent(input$page_98, switch_page("Vis"))
    
    
    msg <- reactive({
      add_ggplot( 
      df2() %>% filter(rank<=25) %>%
        ggplot(aes(x=reorder(MOS, Fit_Score, FUN=max), y=Fit_Score, color=ASVAB)) +
        geom_col(fill="skyblue") +
        scale_shape_manual(values = c(4, 1)) +
        geom_point(aes(shape=Bonus), size=3, color="black")+
        geom_hline(yintercept = .70, linetype="dashed", color="green") +
        theme(legend.title= element_text(color="black", size=10), legend.position = "right") +
        geom_text (aes (label=Title, hjust=1.1, vjust=.5), color="black", size = 3) +
        geom_text (aes (y=.70, x=0, label=("good fit line"), hjust=1, vjust=-.5), color="black", size = 3, angle=0) +
        coord_flip() +
        ylim(0,1) +
        xlab("") +
        ylab("Fit Score") +
        scale_color_manual("ASVAB Minimums", values=c("Met"="lightblue", "Flag GT-Score"="red", "Flag ST-Score"="red", "Flag EL-Score"="blue", "Flag MM-Score"="green")) +
        ggtitle("Fit Results: Top 25 Specialties") +
        labs(caption = "Note: ‘Fit Scores' are for demonstration purposes only and based on available open and published sources."), width = 8.5, height = 11 )
        
    })  
   

   
    
    observeEvent(input$mailButton,{
      isolate({
        send.mail(from = "xx@gmail.com",
                  to = input$to,
                  subject = "MOS Fit Results Chart",
                  body = msg(),
                  smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "xx@gmail.com", passwd = pass, ssl = TRUE),
                  authenticate = TRUE,
                  html = TRUE,
                  send = TRUE)
      })
    } )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
