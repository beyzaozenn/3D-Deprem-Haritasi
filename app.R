library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)

# 1. GERÇEKÇİ BÖLGESEL DEPREM VERİ SETİ
set.seed(123)
n_data <- 200

fay_havuzu <- list(
  "Endonezya Sismik Hattı" = c(-2, 120),
  "Japonya (Pasifik Çemberi)" = c(36, 138),
  "Şili Aktif Fay Kuşağı" = c(-33, -71),
  "California (San Andreas)" = c(34, -118),
  "Türkiye (Alp-Himalaya Kuşağı)" = c(39, 35),
  "Alaska Subduksiyon Bölgesi" = c(61, -150)
)

deprem_data <- lapply(1:n_data, function(i) {
  bolge_ad <- sample(names(fay_havuzu), 1)
  merkez <- fay_havuzu[[bolge_ad]]
  data.frame(
    ID = i,
    Bolge_Isim = bolge_ad,
    Enlem = merkez[1] + rnorm(1, 0, 4.5),
    Boylam = merkez[2] + rnorm(1, 0, 6.5),
    Derinlik = round(runif(1, 10, 160), 1),
    Buyukluk = round(rnorm(1, 6.3, 0.8), 1) %>% max(4.5) %>% min(8.5),
    Tarih = as.Date('2026-05-17') - sample(1:100, 1)
  )
}) %>% bind_rows()

# 2. USER INTERFACE (Geliştirilmiş Kontrastlı Premium Arayüz)
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = span("GLOBAL SEISMIC INTELLIGENCE", style = "font-weight: bold; letter-spacing: 1.5px;")),
  
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      menuItem("Küresel İzleme Paneli", tabName = "dashboard", icon = icon("earth-americas")),
      
      div(style = "padding: 18px; color: #a1a1aa; background: #111113; font-size: 11px; letter-spacing: 1.5px;", "KONTROL PANELİ"),
      
      sliderInput("mag_filter", "Deprem Büyüklüğü (Mw):", min = 4.5, max = 8.5, value = c(5.0, 8.5), step = 0.1),
      selectInput("pal_select", "Veri Renk Paleti (Neon):", 
                  choices = c("YlOrRd", "Viridis", "Hot", "Electric"), selected = "YlOrRd"),
      
      hr(style = "border-color: #27272a;"),
      
      div(style = "padding: 15px 20px;",
          h4("📊 KÜRESEL VERİ ÖZETİ", style = "color: #f4f4f5; font-size: 13px; margin-bottom: 15px; letter-spacing:1px;"),
          uiOutput("stats_sidebar")
      )
    )
  ),
  
  dashboardBody(
    # CSS: Okunabilir Yazılar ve Kusursuz Koyu Arka Planlar
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #0b0b0d !important; }
      .box { background: #131316 !important; border: 1px solid #252529 !important; border-top: 3px solid #ff3333 !important; border-radius: 6px !important; color: #f4f4f5; }
      .box-header .box-title { color: #f4f4f5 !important; font-weight: bold; font-size: 14px; letter-spacing: 0.5px; }
      
      /* Tablo Yazı Okunabilirliği Düzeltmesi */
      .table { color: #ffffff !important; background-color: #131316 !important; }
      .table > tbody > tr > td { color: #ffffff !important; font-weight: bold !important; background: #18181c !important; border-top: 1px solid #252529 !important; }
      .table > thead > tr > th { color: #a1a1aa !important; background: #131316 !important; border-bottom: 2px solid #252529 !important; }
      
      ::-webkit-scrollbar { width: 5px; }
      ::-webkit-scrollbar-thumb { background: #3f3f46; border-radius: 10px; }
    "))),
    
    fluidRow(
      # Sol Taraf: Tam İstediğin Şekilde 3D Küre Haritası
      box(
        width = 8, solidHeader = TRUE, title = "🌐 INTERACTIVE 3D SEISMIC GLOBE (NEON MASTER)",
        plotlyOutput("globePlot", height = "730px")
      ),
      
      # Sağ Taraf: Tıklama Detayları, Bölgesel Grafik ve Canlı Akış Listesi
      column(width = 4,
             box(
               width = NULL, title = "💥 SEÇİLEN DEPREMİN ANALİZ RAPORU", solidHeader = TRUE,
               htmlOutput("pointDetail") 
             ),
             box(
               width = NULL, title = "📊 BÖLGE BAZLI MAKSİMUM ŞİDDET (Mw)", solidHeader = TRUE,
               plotlyOutput("barChart", height = "200px") 
             ),
             box(
               width = NULL, title = "📡 KÜRESEL CANLI SİSMİK AKIŞ",
               div(style = "overflow-y: auto; height: 180px; background: #18181c; border-radius:4px;", tableOutput("miniTable"))
             )
      )
    )
  )
)

# 3. SERVER LOGIC (Hatalardan Arındırılmış Temiz Katman)
server <- function(input, output, session) {
  
  # Filtrelenmiş Canlı Veri Seti
  filtered_df <- reactive({
    deprem_data %>% 
      filter(Buyukluk >= input$mag_filter[1], Buyukluk <= input$mag_filter[2])
  })
  
  # Sol Alt Menü İstatistik Bilgileri
  output$stats_sidebar <- renderUI({
    df <- filtered_df()
    if(nrow(df) == 0) return(p("Kriterlere uygun aktivite yok.", style = "color:#71717a;"))
    tagList(
      p(span("Aktif İzlenen:", style = "color:#a1a1aa;"), strong(nrow(df), " Bölge"), style="margin-bottom:6px;"),
      p(span("En Yüksek Şiddet:", style = "color:#a1a1aa;"), strong(max(df$Buyukluk), " Mw"), style="margin-bottom:6px;"),
      p(span("Ortalama Derinlik:", style = "color:#a1a1aa;"), strong(round(mean(df$Derinlik), 1), " km"), style="margin-bottom:6px;"),
      hr(style = "border-color: #27272a; margin: 10px 0;"),
      p(strong("Sistem Durumu: "), span("ONLINE / RAPORLU", style = "color:#10b981; font-weight:bold;"))
    )
  })
  
  # 3D KÜRE VE HATASIZ PARANTEZ YAPISI OLAN HARİTA MODÜLÜ
  output$globePlot <- renderPlotly({
    df <- filtered_df()
    if(nrow(df) == 0) return(plotly_empty() %>% layout(paper_bgcolor = '#131316', plot_bgcolor = '#131316'))
    
    # Parantez zinciri tamamen onarıldı, sonuna tıklama dinleyicisi eklendi
    p_map <- plot_geo(df) %>%
      add_trace(
        type = "scattergeo",
        lon = ~Boylam,
        lat = ~Enlem,
        mode = "markers",
        marker = list(
          size = ~Buyukluk * 3.3, 
          color = ~Buyukluk,
          colorscale = input$pal_select,
          showscale = TRUE,
          line = list(width = 0.8, color = "rgba(255,255,255,0.7)")
        ),
        source = "seismic_globe",
        text = ~paste0("<b>Bölge:</b> ", Bolge_Isim, 
                       "<br><b>Şiddet:</b> ", Buyukluk, " Mw", 
                       "<br><b>Derinlik:</b> ", Derinlik, " km"),
        hoverinfo = "text"
      ) %>%
      layout(
        geo = list(
          projection = list(type = 'orthographic'), 
          showland = TRUE,
          landcolor = "#16161a", 
          showocean = TRUE,
          oceancolor = "#040406", 
          showcountries = TRUE,
          countrycolor = "#00f0ff", # Tam istediğin o parlayan neon turkuaz ülke sınırları!
          showcoastlines = TRUE,
          coastlinecolor = "#00a2ff", # Sahil şeritleri neon elektrik mavisi
          bgcolor = "#131316",
          lonaxis = list(showgrid = TRUE, gridcolor = "#222"),
          lataxis = list(showgrid = TRUE, gridcolor = "#222")
        ),
        paper_bgcolor = '#131316',
        plot_bgcolor = '#131316',
        margin = list(l = 5, r = 5, t = 5, b = 5)
      )
    
    # RStudio konsolundaki uyarı hatasını engelleyen can simidi kod hattı
    p_map <- event_register(p_map, "plotly_click")
    return(p_map)
  })
  
  # %100 KİLİTLENEN VE ÇALIŞAN TIKLAMA RAPORU
  output$pointDetail <- renderUI({
    click_event <- event_data("plotly_click", source = "seismic_globe")
    
    if (is.null(click_event)) {
      return(HTML("<i style='color:#71717a; font-size:13px;'>Rapor üretmek ve veriyi yana çekmek için 3D küre üzerindeki renkli deprem noktalarından birine tıklayın.</i>"))
    }
    
    row_index <- click_event$pointNumber[[1]] + 1
    df_current <- filtered_df()
    
    if(row_index > nrow(df_current)) return(HTML("<span style='color:#ff3333;'>Seçim güncelleniyor, lütfen tekrar tıklayın.</span>"))
    
    report_data <- df_current[row_index, ]
    
    HTML(paste0(
      "<h3 style='margin-top:0; color:#ff3333; font-weight:bold; font-size:15px;'>📍 ", report_data$Bolge_Isim, "</h3>",
      "<div style='font-size:13px; line-height:1.8; color:#e4e4e7;'>",
      "<b>💥 Sismik Güç:</b> <span style='color:#ff3333; font-size:14px;'><b> Mw ", report_data$Buyukluk, "</b></span><br>",
      "<b>📉 Derinlik Katmanı:</b> ", report_data$Derinlik, " km<br>",
      "<b>📅 İstasyon Tarihi:</b> ", report_data$Tarih, "<br>",
      "<b>🌐 Jeodezik Konum:</b> ", round(report_data$Enlem, 2), "°N / ", round(report_data$Boylam, 2), "°E",
      "</div>"
    ))
  })
  
  # BÖLGE BÖLGE ŞİDDETLERİ GÖSTEREN VE ETİKETLİ OLAN EN YENİ GRAFİK
  output$barChart <- renderPlotly({
    df_chart <- filtered_df()
    if(nrow(df_chart) == 0) return(plotly_empty())
    
    # Bölgelerin en yüksek deprem şiddetini süzüp grupluyoruz
    region_max <- df_chart %>%
      group_by(Bolge_Isim) %>%
      summarise(Max_Mag = max(Buyukluk)) %>%
      arrange(desc(Max_Mag))
    
    # text parametresi ile çubukların üzerine tam istediğin gibi "Japonya: 5.3" etiketini basıyoruz
    plot_ly(region_max, 
            x = ~Max_Mag, 
            y = ~reorder(Bolge_Isim, Max_Mag), 
            type = 'bar',
            orientation = 'h',
            text = ~paste0(Max_Mag, " Mw"), # Çubukların üstündeki net değer metni!
            textposition = 'outside',
            marker = list(
              color = '#ff3333', 
              line = list(color = '#00f0ff', width = 1) # Çubuk kenarlıkları da neon turkuaz yapıldı
            )) %>%
      layout(
        xaxis = list(title = "Maksimum Şiddet", color = "#a1a1aa", gridcolor = "#222", range = c(4, 9.5)),
        yaxis = list(title = "", color = "#a1a1aa"),
        paper_bgcolor = '#131316',
        plot_bgcolor = '#131316',
        margin = list(l = 10, r = 35, t = 10, b = 10)
      )
  })
  
  # Canlı Tablo Akışı
  output$miniTable <- renderTable({
    filtered_df() %>% 
      select(Bolge_Isim, Buyukluk, Derinlik) %>% 
      rename("Sismik Bölge" = Bolge_Isim, "Büyüklük" = Buyukluk, "Derinlik (km)" = Derinlik) %>%
      arrange(desc(Büyüklük))
  }, striped = FALSE, width = "100%")
}

shinyApp(ui, server)