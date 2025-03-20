##** BASE R ****

View(diamonds)

filtered_diamonds <- subset(diamonds, price > 15000 & cut == "Premium")
View(filtered_diamonds)

filtered_diamonds <- subset(diamonds, carat >= 1.5 & carat <= 2.0 & clarity == "IF")
View(filtered_diamonds)


filtered_diamonds <- subset(diamonds, depth >= 60 & depth <= 62 & table >= 55 & table <= 58)
View(filtered_diamonds)


price_ortalama <- mean(diamonds$price)
filtered_diamonds <- subset(diamonds, cut == "Ideal" & price > price_ortalama)
View(filtered_diamonds)


filtered_diamonds <- subset(diamonds, color == "D")
d_top5_diamonds <- head(filtered_diamonds[order(-filtered_diamonds$carat), ], 5)
View(d_top5_diamonds)


filtered_diamonds <- subset(diamonds, clarity == "SI1" & price >= 3000 & price <= 4000)
View(filtered_diamonds)



siralama_diamonds <- diamonds[order(-diamonds$price), ]
top5_diamonds <- subset(siralama_diamonds, select = c(cut, color, carat))[1:5, ]
View(top5_diamonds)


ortalama_carat <- mean(diamonds$carat)
filtered_diamonds <- subset(diamonds, carat < ortalama_carat)
min_diamond <- filtered_diamonds[order(filtered_diamonds$price), ][1, ]
print(min_diamond)

ortalama_depth <- mean(diamonds$depth)
filtered_diamonds <- subset(diamonds, cut == "Fair" & depth > ortalama_depth & color == "G")
View(filtered_diamonds)



pahali_price <- quantile(diamonds$price, probs = 0.99)
top_1_percent_diamonds <- subset(diamonds, price >= pahali_price)
View(top_1_percent_diamonds)


##########################################################################
#**DPLYR **




library(ggplot2)
library(tidyr)
library(dplyr)
View(diamonds)

length(which(is.na(diamonds)))#bu veri setinde hiç na yok mük

filtrelendi14 <- diamonds %>% 
  filter(price >= 15000 & cut == "Premium" ) 

filtrelendi14

filtrelendi15 <- diamonds %>% 
  filter(carat > 1.5 & carat < 2.0 & clarity == "IF")

filtrelendi15

filtrelendi16 <-diamonds %>%
  filter(depth > 60 & depth < 62 & table > 55 & table < 58)

filtrelendi16

filtrelendi17 <- diamonds %>% 
  filter(cut == "Ideal" & price > mean(price))

filtrelendi17

filtrelendi18 <- diamonds %>% 
  filter(color == "D" ) %>%
  arrange(desc(carat)) %>% 
  slice_head(n = 5)

filtrelendi18


filtrelendi19 <- diamonds %>% 
  filter(clarity == "SI1" & price > 3000 & price < 4000)

filtrelendi19

filtrelendi20 <- diamonds %>% 
  arrange(desc(price)) %>%
  slice_head(n = 5)

filtrelendi20

filtrelendi21 <- diamonds %>% 
  filter(carat < mean(carat)) %>% 
  filter(price == min(price))

filtrelendi21

filtrelendi22 <- diamonds%>% 
  filter(cut == "Fair" & depth > mean(depth) & color == "G")

filtrelendi22

filtrelendi23 <- diamonds %>% 
  arrange(desc(price)) %>%
  slice_head(prop = 0.01)

filtrelendi23



















#########################################################################
##**GRAFİK ****







library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggthemes)  # Tema desteği için

# 📌 Örnek veri seti
set.seed(936)
sample_diamonds <- diamonds %>% sample_n(100)

# 📌 Pastel renk üreten fonksiyon (Rastgele)
generate_pastel_colors <- function(n) {
  hues <- runif(n, min = 0, max = 360)  # Farklı renk tonları oluştur
  saturation <- 0.5  # Pastel renk doygunluğu
  value <- 0.9  # Açık renk tonu
  
  rgb_vals <- hcl(hues, saturation * 100, value * 100)  # HCL renk uzayında pastel tonlar üret
  return(rgb_vals)
}

# 📌 Shiny Kullanıcı Arayüzü (UI)
ui <- fluidPage(
  
  # 📌 Pastel renkli arkaplan
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(to right, #FFDEE9, #B5FFFC); /* Pastel geçişli arkaplan */
        font-family: 'Times New Roman', serif;
      }
      .shiny-input-container {
        color: #333;
      }
      .well {
        background-color: rgba(255, 255, 255, 0.8); /* Hafif şeffaf beyaz kutular */
        border-radius: 10px;
      }
    "))
  ),
  
  titlePanel("Renk Değişimi ile Dinamik Grafik"),
  
  sidebarLayout(
    sidebarPanel(
      # 📌 Kullanıcının fiyat aralığını seçmesi için slider
      sliderInput("price_range", "Fiyat Aralığı:", 
                  min = min(diamonds$price), 
                  max = max(diamonds$price), 
                  value = c(5000, 15000)),
      
      # 📌 Kullanıcının kesim kalitesini seçmesi için dropdown
      selectInput("cut_choice", "Kesim Kalitesi:", 
                  choices = unique(diamonds$cut), 
                  selected = "Ideal")
    ),
    
    mainPanel(
      plotlyOutput("scatterPlot")
    )
  )
)

# 📌 Shiny Sunucu Fonksiyonu (Server)
server <- function(input, output, session) {
  
  # 📌 Kullanıcının seçimine göre veriyi filtrele
  filtered_data <- reactive({
    sample_diamonds %>%
      filter(price >= input$price_range[1], price <= input$price_range[2]) %>%
      filter(cut == input$cut_choice)
  })
  
  output$scatterPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = table, y = price, 
                                     text = paste("Karat:", carat, "<br>Fiyat:", price))) +
      geom_point(aes(color = generate_pastel_colors(nrow(filtered_data()))), 
                 size = 4, alpha = 0.8) +  
      scale_color_identity() +  # 📌 Renkleri doğrudan kullan
      theme_minimal() +  # Hafif grid çizgileri içeren çerçeveli grafik
      theme(
        panel.background = element_rect(fill = "#E0F7FA", color = NA),  # 📌 Açık Mavi Arka Plan
        plot.background = element_rect(fill = "#E0F7FA", color = NA),  # 📌 Grafik çevresi de açık mavi
        text = element_text(family = "Times New Roman", face = "bold", color = "#333"),  # 📌 Yazı tipi değiştirildi
        plot.title = element_text(size = 18, face = "bold", color = "#333"),  # 📌 Başlık fontu
        axis.title = element_text(size = 14, face = "bold", color = "#666"),  # 📌 Eksen başlıkları
        axis.text = element_text(size = 12, color = "#444"),  # 📌 Eksen üzerindeki yazılar
        panel.grid.major = element_line(color = "gray90"),  # 📌 Grid çizgileri daha belirgin
        panel.grid.minor = element_line(color = "gray95"),  
        panel.border = element_rect(color = "black", fill = NA, size = 1.5)  # 📌 Kenarlık ekledim
      ) +
      labs(title = "Renk Değişimi ile Dinamik Grafik",
           x = "Yuzey Genisligi",
           y = "Fiyat")
    
    ggplotly(p, tooltip = "text")
  })
}

# 📌 Uygulamayı Başlat
shinyApp(ui = ui, server = server)

























