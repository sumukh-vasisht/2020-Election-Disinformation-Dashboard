#this is my version - not vaccine, but all tweets, all vs fake-only etc.
#devtools::install_version("ggplot2", version = "3.2.1", repos = "http://cran.us.r-project.org")
library(shiny)
library(shinymanager)
library(shinythemes)
library(metathis) 
library(shinycssloaders)

library(tidyverse)
library(data.table)

library(forcats)
library(scales)
library(stringr)
library(jsonlite)
library(fuzzyjoin) 

library(DT)
library(plotly)

credentials <- data.frame(
  user = c("election", "reopen"), # mandatory
  password = c("election2020", "reopen"), # mandatory
  start = c("2020-07-15"), # optional (all others)
  expire = c(NA, "2024-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Enter password",
  stringsAsFactors = FALSE
)

# Mutual information data for text keywords
mutual_information <- read_tsv("data/mutual_information_last_month_till_Jan_9_2022.tsv")

# Leaderboards data (with state and national joined)
# Updated 2022-01-10
#state_domain_leaderboards <- fread("all_domain_leaderboar")
state_domain_leaderboards <- fread("data/domain_leaderboard_last_year_till_Jan_9_2022_with_fake.tsv")

#state_domain_leaderboards_fake <- fread("data/domain_leaderboard_fake")

# Ingest df of top 10 URLs per state that have had titles scraped
# Updated 2021-02-16
state_url_leaderboards_lastmonth_top10_titles <- fread("data/url_leaderboard_last_month_till_Jan_9_2022_with_fake_filled_in_titles.tsv")#fread("data/url_leaderboard_file_vaccine_tracker_modified.tsv")#

#state_url_leaderboards_lastmonth_top10_titles_fake <- fread("data/url_leaderboard_fake.tsv")

# Ingest pre-processed monthly ranked leaderboards (with state and national joined) - does not seem to be getting used!
state_domain_leaderboards_monthy_all <- fread("data/domain_leaderboard_last_year_till_Jan_9_2022_with_fake.tsv")

# State abbreviations and names
state_abbr_and_names <- fread("data/state_abbr_and_names.csv")

# define all colormaps as a list of named vectors
COLORMAPS = list(
  race = c("African-American" = "lightblue",
           "Asian" = "#FFE523",
           "Caucasian" = "#b1ab99",
           "Hispanic"= "#635e4e"
           ),
  income =  c("<$15,000" = "#863605",
              "$15,000-$30,000" = "lightblue",
              "$30,000-$45,000" = "#FFE523",
              "$45,000-$60,000" = "#b1ab99",
              "$60,000-$75,000" = "#635e4e",
              "$75,000-$90,000" = "#59a2e9",
              "$90,000+" = "#228B22"),
  age =  c("18-29" = "lightblue",
           "30-49"= "#FFE523",
           "50-64"="#b1ab99",
           ">65"= "#635e4e"),
  party =  c("Democrat" = "#59a2e9",
             "Independent" = "#228B22",
             "Republican" = "#863605")
)
# from the colormap, we can extract just the levels associated with the variable
FACTOR_LEVELS = lapply(COLORMAPS, names)

reverse_legend_labels <- function(plotly_plot) {
  # https://stackoverflow.com/questions/59611914/reverse-the-legend-order-when-using-ggplotly
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}

# this function creates one of the domain plots
# `kind` should be a demographic variable ("age", etc.) that corresponds to
# one defined in COLORMAPS
build_plot <- function(input, kind, amount_use=100, amount_show=20, proportions=T) {
    # to avoid recursive indexing, pre-define the focal demographic group
    # and sort the factor levels
    shiny::validate(shiny::need(input[[quo_name(kind)]], glue::glue("Choose {quo_name(kind)}")))
    target <- input[[quo_name(kind)]]
    l <- FACTOR_LEVELS[[kind]]
    l <- l[rev(order(match(l, input[[quo_name(kind)]])))]

    # we're going to implement both the filled and the stacked bar charts here
    if (proportions) {
        bar_type <- "fill"
        plt_var <- sym("share_perc")
    } else {
        bar_type <- "stack"
        plt_var <- sym("shares")
    }

    # construct the input data
    #
    # `amount_use` and `amount_show`: By default, we use the top 100 domains (by
    # national popularity), even though we only display 20.
    d <- state_domain_leaderboards %>%
        filter(state == input$state_domain) %>%
        filter(newstype == input$newstype_domain) %>%
        arrange(desc(total)) %>%
        head(n=amount_use) %>%
        select(domain, total, all_of(l)) %>%
        gather(kind, value, -domain, -total) %>%
        filter(!is.na(value)) %>%
        mutate(kind = factor(kind, levels=l, ordered = T)) %>%
        mutate(share_perc = (value / total),
                shares = value)

        # the !!s are a bit nasty, but they just mean "treat the string as a variable name"
        p <- ggplot(d, aes(fill=kind,
                    y=!! plt_var,
                    label=value,
                    x=fct_reorder2(domain,
                                    kind==target,
                                    !!ifelse(proportions,
                                            sym("share_perc"),
                                            sym("shares")),
                                    .desc = F))) +
        geom_bar(position=bar_type, stat="identity") +
        coord_flip(xlim=c(min(amount_use, length(unique(d$domain)))-amount_show, min(amount_use, length(unique(d$domain)))))+
        scale_fill_manual(values=COLORMAPS[[kind]]) +
        theme_minimal() +
        labs(
            x = "",
            y = "",
            title = ""
        ) +
        scale_y_continuous(labels = comma) +
        guides(fill = guide_legend(reverse=T, title=str_to_title(quo_name(kind))))
    # Align the legend so that the top-to-bottom ordering is the same
    # as the left-to-right display
    reverse_legend_labels(ggplotly(p, tooltip=c(quo_name(kind), "shares")))
}



# Shiny app
ui <- navbarPage("USA 2020 Election Disinformation Dashboard", 
                 id = 'menu',
                 tabPanel("How to use this tool",
                          shinyjs::useShinyjs(),
                          fluidPage(
                            #verbatimTextOutput("auth_output"),
                            meta() %>%
                              meta_social(
                                title = "Russia-Ukraine Conflict Disinformation Monitor",
                                description = "Explore disinformation circulating among over 1 million twitter users in the United states. From Northeastern University's Lazer Lab and Network Science Institute.",
                                url = "https://storybench.shinyapps.io/covid-tweets",
                                image = "https://storybench.org/statelogos/covid-shiny-preview.jpg",
                                image_alt = "Covid-19 tweets project from Northeastern University",
                                og_type = "website",
                                og_site_name = "Covid-19 tweets",
                                og_author = c("Aleszu Bajak", "Lazer Lab"),
                                twitter_creator = "@aleszubajak",
                                twitter_card_type = "summary"
                              ),
                            
                              list(tags$script(HTML("var header = $('.navbar > .container-fluid');
        header.append('<div style=\"float:right\"><ahref=\"https://www.networkscienceinstitute.org\"><img src=\"https://uploads-ssl.webflow.com/5c9104426f6f88af009ef3ad/5d83de8fdb4091605831e95d_NU_NetworkScienceInstitute_RGB-01-p-800.png\" alt=\"netsci\" style=\"float:right;width:200px;height:46px;padding-top:0px;\"> </a></div>');
            console.log(header)")
                              ),
                              tags$style(".shiny-text-output {font-size:18px;
                                           color:black;
                                           padding-bottom: 10px;
                                           display:block; }"),
                              tags$head(includeHTML(("tracker.html"))),
                              tags$style(HTML(".navbar-default .navbar-brand {color: #000000;}"))),
                              br(),
                              hr(),
                            fluidRow(
                              column(2),
                              column(8,
                              h3("Background"),
                              #verbatimTextOutput("auth_output"),
                              div(
                                "Falsities about the pandemic, vaccines, and mitigation strategies have undermined the public response to the COVID-19 pandemic. Moreover, the pandemic has accelerated the industrialization of disinformation, the wholesale production and dissemination of false and misleading information. Today we find ourselves playing catch up as disinformation producers churn out new content to persuade the public of falsities that often put the public at risk. The fact is we cannot keep up with all the disinformation coming out."
                              ),
                              h3("How you can use this data"),
                              div(
                                "This dashboard is intended to help public health agencies, policy makers, journalists, and other health communicators focus on addressing the most important pieces of disinformation: those stories, domains, and keywords that are driving the conversation. You can use our dashboard to:",
                                tags$ol(
                                  tags$li(
                                    "Understand what disinformation is popular in your area. You can do this by identifying the top disinformation links being shared in your state, comparing that to disinformation shared nationally and in other states, and comparing it to high-quality information being shared in your area."
                                    ), 
                                  tags$li(
                                    "Inform other public health research. Using this dashboard, you can identify the common misconceptions about the pandemic in your area. You can then create surveys to better understand who truly holds these views and what messages may change their mind."
                                    ), 
                                  tags$li(
                                    "Shape your communication and coverage strategy: Combatting disinformation is a little different from combatting misinformation. It's not about fact checking articles or addressing false stories. It's about changing people's underlying worldview and confronting false narratives. Rather than play whack-a-mole with misinformation, our dashboard can help you understand the fundamental narratives that are driving disinformation."
                                    )
                                )
                              ),
                              
                              h3("Give us Feedback"),
                              div(
                                "We are working diligently to make this dashboard as helpful as possible to public health officials, policy makers, and journalists. To do that, we need to hear from you. What do you want to see in this data? What other data would you like us to include? Set up a conversation today by emailing Jason Radford (j.radford@northeastern.edu) to discuss how to use the data and what we can do to help you."
                              ),
                              
                              h3("Request a custom report or dashboard"),
                              div(
                                "If you have a specific question related to disinformation in your area, we can create a custom dashboard you can use for monitoring or a one-time report (depending on data availability and suitability). Please email Jason Radford (j.radford@northeastern.edu) to discuss the custom report or dashboard."
                              ),
                            ),
                             
                              column(2)
                            ),
                            fluidRow( br(),
                                      br(),
                                      br(),
                                      br()
                            )#,
                            #verbatimTextOutput("auth_output")
                          )
                 ),
                        tabPanel("Top Stories", value = "toplinks",
                                     fluidPage(
                                       tags$div(class="tagline",
                                                tags$p("The Covid-19 tweets project at Northeastern University aims to understand how users across the United States are sharing pandemic-related information", 
                                                       align="right", style = "float:right; width: 400px; font-size: 8pt; font-style: italic;")),
                                       br(),
                                       hr(),
                              fluidRow(
                                  column(2,align="center",
                                         textOutput("selectedstate2"),
                                         uiOutput("statelogo2"),
                                         br(),
                                         selectInput("state",
                                                            "Choose a state",
                                                            choices = c(state_abbr_and_names$state),
                                                            selected = "National"),
                                         selectInput("newstype",
                                                     "Choose Type of Source",
                                                     choices = c("All", "Disinformation"),
                                                     selected = "Disinformation"),
                                         #selectInput("month",
                                         #            "Choose a month",
                                         #            choices = c("January 2021"),
                                         #            selected = "January 2021")
                                         ),
                                  column(10, 
                                         #h4(paste("Top links shared from November 1, 2020 to", format(Sys.time(), "%B %d, %Y")), align = "center"),
                                         h4(paste("Top stories shared"), align = "center"),
                                         br(),
                                         DTOutput("output_top10_table","100%"), align="center"),
                                         br(),
                                         br()
                              ),
                              # fluidRow(
                              #   column(2),
                              #     column(10,
                              #            h4(textOutput("input_state_title2")), #"Top links distinctive to this state", align = "center"),
                              #            br(),
                              #            DTOutput("output_top10_distinctive_table","100%"), align="center")
                              #         ),
                              # fluidRow( br(),
                              #           br(),
                              #           br(),
                              #           br()
                              #)       
                          )
                 ),

                 tabPanel("Top keywords",
                          fluidPage(
                            tags$div(class="tagline",
                                     tags$p("The Covid-19 tweets project at Northeastern University aims to understand how users across the United States are sharing pandemic-related information",
                                            align="right", style = "float:right; width: 400px; font-size: 8pt; font-style: italic;")),
                            br(),
                            hr(),
                            fluidRow(
                              column(2),
                              column(10,
                                     h4("Distinctive disinformation keywords", align = "center"),
                                     br(),
                                     align="center",
                                     DTOutput("topfakekeywords","100%")
                              )
                            ),
                            br(),
                            br(),
                            fluidRow(column(2), 
                                     column(9),
                                     column(1)
                            ), 
                            fluidRow( br(),
                                      br(),
                                      br(),
                                      br()
                            )   
                          )
                 ), 
                 
                 tabPanel("Top websites", 
                          fluidPage(
                            tags$div(class="tagline",
                                     tags$p("The Covid-19 tweets project at Northeastern University aims to understand how users across the United States are sharing pandemic-related information", 
                                            align="right", style = "float:right; width: 400px; font-size: 8pt; font-style: italic;")),
                            br(),
                            hr(),
                            fluidRow(
                              column(2,
                                     align="center",
                                     textOutput("selectedstate3"),
                                     uiOutput("statelogo3"),
                                     br(),
                                     selectInput("state_domain",
                                                 "Choose a state",
                                                 choices = c(state_abbr_and_names$state),
                                                 selected = "National"),
                                     selectInput("newstype_domain",
                                                 "Choose Type of Source",
                                                 choices = c("All", "Disinformation"),
                                                 selected = "Disinformation"),
                              ),
                              column(10, 
                                     h4("Top websites shared from 10/01/2020 to 01/31/2021", align = "center"),
                                     br(),
                                     DTOutput("top_domains","100%"), align="center"),
                              # column(5, 
                              #        h4("Popularity of top domains by month", align = "center"),
                              #        br(),
                              #        plotlyOutput("top_domains_rank","100%"), align="center")
                            ),
                            
                            # Demographics
                            
                            hr(),
                            h4("Who is sharing those top websites?", align="center"),
                            br(),
                            fluidRow(
                              # RACE FILTER
                              column(1),
                              column(10, align="center",
                                     selectInput("race",
                                                 "Sort by race",
                                                 choices = c(FACTOR_LEVELS$race),
                                                 selected="Caucasian")
                              ),
                              column(1)
                            ),
                            br(),
                            
                            # RACE PLOTS
                            
                            fluidRow(
                              column(6,
                                     "Number of Shares",
                                     br(),
                                     plotlyOutput("domains_race_raw","100%"), align="center"),
                              
                              column(6, 
                                     "Percentage of shares",
                                     br(),
                                     plotlyOutput("domains_race","100%"), align="center")
                            ),
                            br(),
                            
                            # PARTY
                            
                            fluidRow(
                              # PARTY FILTER
                              column(1),
                              column(10, align="center",
                                     selectInput("party",
                                                 "Sort by party",
                                                 choices = c(FACTOR_LEVELS$party),
                                                 selected="Republican") 
                              ),
                              column(1)
                            ),
                            br(),
                            
                            # PARTY PLOTS
                            
                            fluidRow(
                              column(6,
                                     "Number of Shares",
                                     br(),
                                     plotlyOutput("domains_party_raw","100%"), align="center"),
                              
                              column(6, 
                                     "Percentage of shares",
                                     br(),
                                     plotlyOutput("domains_party","100%"), align="center")
                            ),
                            br(),
                            
                            # AGE  
                            
                            fluidRow(
                              # AGE FILTER 
                              column(1),
                              column(10, align="center",
                                     selectInput("age",
                                                 "Sort by age group",
                                                 choices = c(FACTOR_LEVELS$age), selected="50-64"),
                                     
                              ),
                              column(1)
                            ),
                            br(),
                            
                            # AGE PLOTS
                            
                            fluidRow(
                              column(6,
                                     "Number of Shares",
                                     br(),
                                     plotlyOutput("domains_age_raw","100%"), align="center"),
                              
                              column(6, 
                                     "Percentage of shares",
                                     br(),
                                     plotlyOutput("domains_age","100%"), align="center")
                            ),
                            br(),
                            
                            
                            # INCOME
                            
                            fluidRow(
                              # INCOME FILTER 
                              column(1),
                              column(10, align="center",
                                     selectInput("income",
                                                 "Sort by median neighborhood income",
                                                 choices = c(FACTOR_LEVELS$income),
                                                 selected="$30,000-$45,000") 
                              ),
                              column(1)
                            ),
                            br(),
                            
                            # INCOME PLOTS
                            
                            fluidRow(
                              column(6,
                                     "Number of Shares",
                                     br(),
                                     plotlyOutput("domains_income_raw","100%"), align="center"),
                              
                              column(6, 
                                     "Percentage of shares",
                                     br(),
                                     plotlyOutput("domains_income","100%"), align="center")
                            ),
                            br(),
                            br()
                          )
                 ),                 
                 
                 tabPanel("Learn More", 
                          fluidPage(
                              column(2),
                              column(9,
                                     h3("Authors"),
                                     div("This app was built by ",
                                         tags$a(href="https://jsradford.github.io/home/", "Jason Radford", target="_blank"), 
                                         " Director of Northeastern's Social Design Lab and",
                                         tags$a(href="https://pranav-goel.github.io/", "Pranav Goel", target="_blank"), 
                                         " Ph.D. student in Computer Science at the University of Maryland with help from Stefan McCabe, Hong Qu, and the Lazer Lab, on top of original code by ", 
                                     tags$a(href="http://aleszu.com/", "Aleszu Bajak", target="_blank"), 
                                     "."),
                                     br(),
              
                                     div("The project is based on data from a collaboration at the ",
                                         tags$a(href="https://lazerlab.net/", "Lazer Lab", target="_blank"),
                                         " at Northeastern University's Network Science Institute in Boston, MA. For additional information and press requests, contact David Lazer at d.lazer@neu.edu, Katherine Ognyanova at katya.ognyanova@rutgers.edu, and Matthew A. Baum at matthew_baum@hks.harvard.edu."),
                                     br(),
                                    
                                     h3("The Data"),
                                     div("This work is based on the process described in Grinberg et al 2019 (1).  We monitor the accounts of 1.6 million public accounts on Twitter linked to identified registered voters in America."),
                                     br(),
                                     div("We retained only pandemic tweets by filtering using a broad list of keywords, phrases and hashtags related to the pandemic. The keyword list contains words directly related to the pandemic. A tweet was included in the sample if it contained at least one item from our list; it could be contained in the tweet text, quoted text, hashtag or any part of the URL string -- this does not include the content from the linked web page. For more details on tweet selection, see Gallagher et al (2)."),
                                     br(),
                                     div("For each pandemic-related tweet, we collected the URLs shared by our panel and classified them as originating from a web domain that we classified as either fake, not fake or unknown. The fake news classifications were based on Grinberg et al (3), which has a three-tier classification system for fake news domains: orange, red and black. Here we classify \"black\" and \"red\" domains as fake, where \"black\" domains are \"a set of websites taken from preexisting lists of fake news sources,\" and \"red\" domains \"spread falsehoods that clearly reflect a flawed editorial process.\" We do not include domains classified as \"orange\" because the authors \"were less certain that the falsehoods stemmed from a systematically flawed process. "),
                                     br(),                                   
                                     div("We then extracted all the shared URLs, domains and keywords from these tweets."),
                                     br(),
                                     h3("What is disinformation and how is it different from misinformation?"),
                                     div("Disinformation is information that has been intentionally created to mislead the audience. It may be false information presented as truth or true information presented in a misleading way. In either case, disinformation is engineered to create a false image of reality. This contrasts with misinformation which is information that is factually incorrect. Often people create misinformation to hide the truth. But, at the same time, people make mistakes and even the most rigorous journalists get things wrong. Focusing on misinformation means focusing on what people get right and wrong. Focusing on disinformation means focusing on the underlying false image of reality being proffered."),
                                     
                                     br(),
                                    h3("How do we identify Disinformation"),
                                    div("We count as disinformation any news story produced by fake news websites - websites that are designed to create misrepresentations of reality either by fabricating news or echoing news that fits the misleading facts about world the website projects as reality. We rely on a tiered system for fake news combining data from existing lists of fake news sources and our own rating system for whether or not sources published false storied due to issues in their editorial process (as opposed to mere accidents)."),
                                    br(),
                                    div("When a user in our panel shares a link from a fake news website, we treat the content of that tweet as disinformation and we give the user a disinformation risk score. The more they share content from these sites, the higher their score."),
                                    br(),
                                    div("The disinformation links and people sharing them contrast with those from valid news sources where reporting, fact-checking, and editing standards prevent (though do not guarantee) misinformation and intentional misrepresentations. The dashboard allows you to see what is being discussed by those who share disinformation and compare it to the articles, domains, and keywords being discussed by those who do not share material from these sources."),
                                    br(),
                                    div("There is no perfect distinction, however. Disinformation and misinformation can enter mainstream news systems and we inevitably miss these cases. For example,",
                                    tags$a(href="https://www.usatoday.com/story/news/factcheck/2021/09/15/fact-check-oklahoma-hospitals-not-backed-up-ivermectin-cases/8271014002/", "the story", target="_blank"), 
                                    "that Ivermectin overdoses were preventing people from treatment in Oklahoma hospitals. Just because a story circulates among well edited sources does not mean it is a true representation of the facts."),
                                    br(),
                                     h3("Top Stories"),
                                     div("For the top stories in each state and nationally, we identify the title for the news articles. We do this by scraping the HTML code from the URL link and extracting the contents of the \"title\" meta tags. We were able to do this for 93% of the top URLs."),
                                     br(),
                                    h3("Top Keywords"),
                                    div("In the \"Disinformation keywords\" tab, we highlight the 15 keywords that are shared most often in fake news tweets, as a proportion of total keyword occurrences. We only include keywords that have been shared more than 10 times in disinformation tweets. The mutual information score ranges from 0 to 1 where 1 would mean every time you see a given keyword, it would be in a tweet sharing disinformation and '0' would mean the keyword was never used in a disinformation tweet."),
                                    br(),
                                     h3("Top Websites"),
                                     div("We present the most shared domains in each state and nationally. We breakdown who is sharing each domain by age, race, neighborhood income, and political affiliation using data from Targetsmart."),
                                     br(),
                                     h4("Download the data"),
                                     div("Data and figures are available to download via the CSV option on tables and the camera icon on interactive plots. To download entire table, select the maximum number of entries. Multiple CSVs may need to be downloaded for keywords."),
                                     br(),
                                    h3("Sources"),
                                     
                                      div("1. Gallagher, R. J., Doroshenko, L., Shugars, S., Lazer, D., & Welles, B. F. (2020). ",
                                     tags$a(href="http://arxiv.org/abs/2009.07255", "Sustained Online Amplification of COVID-19 Elites in the United States.", target="_blank")),
                                     br(),
                                     div("2. Grinberg, N., Joseph, K., Friedland, L., Swire-Thompson, B., & Lazer, D. (2019). ",
                                         tags$a(href="https://science.sciencemag.org/content/363/6425/374.abstract", "Fake news on Twitter during the 2016 U.S. presidential election.", target="_blank"),
                                         "Science, 363(6425), 374–378."),
                                     br()
                          ) 
                        )
                 ),
                 tabPanel("Others",
                          fluidPage(
                            tags$div(class="tagline",
                                     tags$p("The Covid-19 tweets project at Northeastern University aims to understand how users across the United States are sharing pandemic-related information", 
                                            align="right", style = "float:right; width: 400px; font-size: 8pt; font-style: italic;")),
                            br(),
                            hr(),
                            column(2,align="center",
                                   br(),
                                   selectInput("state_others",
                                               "Choose a state",
                                               choices = c(state_abbr_and_names$state),
                                               selected = "National"),
                                   selectInput("filter_others",
                                               "Choose a filter",
                                               choices = c("African-American", "Caucasian", "Hispanic", "Asian", "18-29",
                                                           "30-49", "50-64", ">65", "Female", "Male", "Democrat", "Independent", 
                                                           "Republican", "$15,000-$30,000", "$30,000-$45,000", "$45,000-$60,000", "$60,000-$75,000", "$75,000-$90,000",
                                                           "$90,000+", "<$15,000", "No Filter"),
                                               selected = "African-American"),
                                   selectInput("newstype_others",
                                               "Choose Type of News Source",
                                               choices = c("All", "Disinformation"),
                                               selected = "Disinformation"),
                            ),
                            column(10, 
                                   h4(paste("Top stories shared"), align = "center"),
                                   br(),
                                   DTOutput("output_top10_table_others","100%"), align="center"),
                            br(),
                            br()
                          )
                  )
)

ui <- secure_app(ui, choose_language = TRUE)

server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })

    observe(
        {
          updateSelectInput(session,
                                  "state",
                                  "Choose a state",
                                  selected = "National",
                                  choices = c(state_abbr_and_names$state))
          updateSelectInput(session,
                                  "newstype",
                                  "Choose Type of Source",
                                  choices = c("All", "Disinformation"),
                                  selected = "Disinformation")
          updateSelectInput(session,
                            "state_domain",
                            "Choose a state",
                            selected = "National",
                            choices = c(state_abbr_and_names$state))
          updateSelectInput(session,
                            "newstype_domain",
                            "Choose Type of Source",
                            choices = c("All", "Disinformation"),
                            selected = "Disinformation")

        }
    )
    
    shinyjs::addClass(id = "menus", class = "navbar-right")
    
    output$how_to_covid_tweets <- renderUI({
      tags$img(src = "https://storybench.org/statelogos/how-to-covid-tweets.gif")
    })

    c_url <- reactive({
      shiny::validate(
            shiny::need(input$state,"Choose a state")
        )
        paste0("https://storybench.org/statelogos/", input$state, ".svg") 
    })
    
    c_url_domain <- reactive({
      shiny::validate(
        shiny::need(input$state_domain,"Choose a state")
      )
      paste0("https://storybench.org/statelogos/", input$state_domain, ".svg") 
    })
    
    output$statelogo1 <- renderUI({
        tags$img(src = c_url())
    })
    
    output$selectedstate1 <- renderText({ 
        statename <- state_abbr_and_names %>%
            filter(state == input$state) %>%
            select(name) %>%
            as.character()
        statename
    })
    
    output$statelogo2 <- renderUI({
        tags$img(src = c_url())
    })
    
    output$selectedstate2 <- renderText({ 
        statename <- state_abbr_and_names %>%
            filter(state == input$state) %>%
            select(name) %>%
            as.character() 
        statename
    })
    
    output$statelogo3 <- renderUI({
        tags$img(src = c_url_domain())
    })
    
    output$selectedstate3 <- renderText({ 
      statename <- state_abbr_and_names %>%
        filter(state == input$state_domain) %>%
        select(name) %>%
        as.character() 
      statename
    })
    
    output$statelogo4 <- renderUI({
      tags$img(src = c_url())
    })
    
    output$selectedstate4 <- renderText({ 
      statename <- state_abbr_and_names %>%
        filter(state == input$state) %>%
        select(name) %>%
        as.character() 
      statename
    })
    
    output$statelogo5 <- renderUI({
      tags$img(src = "https://storybench.org/statelogos/National.svg")
    })
    
    output$selectedstate5 <- renderText({ 
      statename <- state_abbr_and_names %>%
        filter(state == "National") %>%
        select(name) %>%
        as.character() 
      statename
    })
    
    output$input_state_title <- renderText({
        statename <- state_abbr_and_names %>%
          filter(state == input$state) %>%
          select(name) %>%
          as.character() 
        
        state_title <- paste("Top", statename) 
        state_title
    })
    
    output$input_state_title2 <- renderText({
      statename <- state_abbr_and_names %>%
        filter(state == input$state) %>%
        select(name) %>%
        as.character() 
      state_title <- paste("Top 50 links distinctive to", statename, "from November 1, 2020 to", format(Sys.time(), "%B %d, %Y")) 
      state_title
    })
    
    output$googletrends <- renderUI({
      tags$a(href = paste0("https://trends.google.com/trends/explore?q=%22", input$keyword3,"%22&geo=US"), "Search Google Trends for keyword interest", target="_blank")
    })

    output$input_state_keywordtitle <- renderText({
      statename <- state_abbr_and_names %>%
        filter(state == input$state) %>%
        select(name) %>%
        as.character() 
      state_title <- paste("Most popular", statename, "keywords from November 1, 2020 to", format(Sys.time(), "%B %d, %Y")) 
      state_title
    })
    
    # OTHERS TAB
    
    output$output_top10_table_others <- DT::renderDataTable({
      shiny::validate(
        shiny::need(input$filter_others,"Choose a filter")
      )
      shiny::validate(
        shiny::need(input$newstype_others,"Choose Type of News Source")
      )
      
      fil = input$filter_others
      print(fil)
      
      state_url_leaderboards_titles_input_others <- state_url_leaderboards_lastmonth_top10_titles %>% 
        filter(newstype == input$newstype_others) %>% 
        filter(state == input$state_others)
      
      if(fil == "No Filter")
      {
        output_top10_table_others <- state_url_leaderboards_titles_input_others %>% 
          select(title, total, parsed_url, domain) %>%
          mutate(title = paste0("<a href='", parsed_url,"' target='_blank'>", title,"</a>")) %>%
          select(title, total, domain) %>%
          arrange(desc(total))
      }
      
      else
      {
        state_url_leaderboards_titles_input_others <- state_url_leaderboards_titles_input_others[state_url_leaderboards_titles_input_others[[fil]] != 0]
        
        output_top10_table_others <- state_url_leaderboards_titles_input_others %>% 
          select(title, parsed_url, domain, input$filter_others) %>%
          mutate(title = paste0("<a href='", parsed_url,"' target='_blank'>", title,"</a>")) %>%
          select(title, input$filter_others, domain) %>%
          arrange(desc(state_url_leaderboards_titles_input_others[[fil]]))
        
        colnames(output_top10_table_others) <- c('title','total','domain')
      }
      
      output_top10_table_others
    }, 
    extensions = 'Buttons',
    options = list(dom = 'Bftp',
                   pageLength = 15,
                   buttons = list('copy', list(extend = 'csv', filename= 'covid-tweets-top10-urls'))
    ),
    escape = FALSE)
    
    # TOP LINKS
    
    output$output_top10_table <- DT::renderDataTable({
        shiny::validate(
            shiny::need(input$state,"Choose a state")
        )
        shiny::validate(
           shiny::need(input$newstype,"Choose Type of Shares")
        )
      
    state_url_leaderboards_titles_input <- state_url_leaderboards_lastmonth_top10_titles %>%
        filter(state == input$state) %>%
        filter(newstype == input$newstype)
    
    #state_url_leaderboards_titles_input <- state_url_leaderboards_titles_input 
    output_top10_table <- state_url_leaderboards_titles_input %>% 
        select(title, total, parsed_url, domain) %>%
        mutate(title = paste0("<a href='", parsed_url,"' target='_blank'>", title,"</a>")) %>%
        select(title, total, domain) %>%
        arrange(desc(total))
    output_top10_table
    }, 
    extensions = 'Buttons',
    options = list(dom = 'Bftp',
                   pageLength = 15,
                   buttons = list('copy', list(extend = 'csv', filename= 'covid-tweets-top10-urls'))
                   ),
    escape = FALSE)

    
    # Top domains table
    
    output$top_domains <- DT::renderDataTable({
        
      shiny::validate(
            shiny::need(input$state_domain,"Choose a state")
        )
      shiny::validate(
            shiny::need(input$newstype_domain,"Choose Type of Shares")
      )
        
        state_month <- state_domain_leaderboards %>%
            filter(state== input$state_domain) %>%
            filter(newstype == input$newstype_domain)    # CHANGE STATE 2 INPUT and NEWSTYPE 2 INPUT

        state_month_topdomains <- state_month %>%
          select(domain, total) %>% 
          group_by(domain) %>%
          summarise(count = sum(total), .groups = "drop") %>%
          arrange(desc(count)) %>%
          slice_max(count, n=1000)
        
       state_month_topdomains_tbl <- state_month_topdomains %>%
            select(domain, count) %>% # unique_users
            datatable(extensions = 'Buttons', options = list(dom = 'Btpl',
                                                             buttons = list('copy', list(extend = 'csv', filename= 'covid-tweets-top-domains'))
                                                             ),
                colnames = c("rank", "domain", "total shares")) %>% #, "unique users"
            formatStyle(
                'count',
                background = styleColorBar(range(state_month_topdomains$count), 'lightblue'))
       state_month_topdomains_tbl
        
    })
    

    output$domains_race_raw <- renderPlotly(build_plot(input, sym("race"), proportions = F))
    output$domains_race <- renderPlotly(build_plot(input, sym("race"), proportions = T))

    output$domains_party_raw <- renderPlotly(build_plot(input, sym("party"), proportions = F))
    output$domains_party <- renderPlotly(build_plot(input, sym("party"), proportions = T))

    output$domains_age_raw <- renderPlotly(build_plot(input, sym("age"), proportions = F))
    output$domains_age <- renderPlotly(build_plot(input, sym("age"), proportions = T))

    output$domains_income_raw <- renderPlotly(build_plot(input, sym("income"), proportions = F))
    output$domains_income <- renderPlotly(build_plot(input, sym("income"), proportions = T))
    
    
    
    # KEYWORD SEARCH
    
    output$mainbarchart <- renderPlotly({
      
      shiny::validate(
        need(input$keywords != "", "Please write in a keyword")
      )
      
      search_term <- strsplit(input$keywords, ",")[[1]] %>% str_trim()
      search_term_df <- as.data.frame(search_term)
      search_term_df$search_term <- as.character(search_term_df$search_term)
      
      inputkeywordsdf_covid <- keywords_melted %>%
        fuzzy_inner_join(search_term_df, by=c("keyword" = "search_term"), match_fun= str_detect) %>%
        filter(state == input$state) %>%
        group_by(search_term, date) %>%
        summarise(daily_total = sum(value), .groups = 'drop') %>%
        mutate(term = search_term)
      
      inputkeywordsdf_covid$date <- as.Date(inputkeywordsdf_covid$date)
      
      shiny::validate(
        need(dim(inputkeywordsdf_covid)[1] != 0, "Please try another keyword. Hint: Try some of the keywords at the bottom of this page.")
      )
      
      p <- ggplot(inputkeywordsdf_covid, aes(date, daily_total, color=term)) + # or 'value'
        geom_line() + theme_minimal() + ylab("daily total") + xlab("") +
        labs(title=paste("Popularity of Covid-19 keywords on Twitter from November 1, 2021 to", format(Sys.time(), "%B %d, %Y"))) +
        theme(legend.position = "bottom") 
      
      p_interactive <- ggplotly(p, dynamicTicks = TRUE) %>%
        rangeslider() %>%
        layout(hovermode = "x",
               legend =list(orientation = "h"))
      p_interactive
    })
    
    # Export keywords search
    
    output$exportkeywordsearch <- DT::renderDataTable({
      
      shiny::validate(
        need(input$keywords != "", "Please write in a keyword")
      )
      
      search_term <- strsplit(input$keywords, ",")[[1]] %>% str_trim()
      search_term_df <- as.data.frame(search_term)
      search_term_df$search_term <- as.character(search_term_df$search_term)
      
      inputkeywordsdf_covid <- keywords_melted %>%
        fuzzy_inner_join(search_term_df, by=c("keyword" = "search_term"), match_fun= str_detect) %>%
        filter(state == input$state) %>%
        group_by(search_term, date) %>%
        summarise(daily_total = sum(value), .groups = 'drop') %>%
        mutate(term = search_term)
      
      inputkeywordsdf_covid$date <- as.Date(inputkeywordsdf_covid$date)
      
      shiny::validate(
        need(dim(inputkeywordsdf_covid)[1] != 0, "Please try another keyword. Hint: Try some of the keywords at the bottom of this page.")
      )
      
      inputkeywordsdf_covid_tbl <- inputkeywordsdf_covid %>%
        select(search_term, date, daily_total) %>%
        datatable(extensions = 'Buttons', options = list(dom = 'Blftp',
                                                         buttons = list('copy', list(extend = 'csv', filename= 'covid-tweets-your-keywords'))
        ))
      
      inputkeywordsdf_covid_tbl
      
    })
    
    
    output$topfakekeywords <- DT::renderDataTable({
      mutual_information %>%
        datatable(extensions = 'Buttons',
                  options = list(dom = 'Blftp',
                                 pageLength = 50,
                                 buttons = list('copy',
                                                list(extend = 'csv',
                                                     filename= 'covid-tweets-fakenews-keywords'))),
                  colnames = c("Token",
                               "Mutual Information",
                               "Number of non-fake-news sharers",
                               "Number of fake-news sharers",
                               "% of non-fake-news sharers",
                               "% of fake-news-sharers",
                               "Ratio")) %>%
        formatPercentage(c("ratio_untreated", "ratio_treated"), 2)
    })
    
    
}

shinyApp(ui = ui, server = server)





