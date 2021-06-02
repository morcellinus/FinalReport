library(shiny)
library(tidyverse)
library(ggmap)

# Define UI ---

load('GNmap.rda')
load('YDPmap.rda')
gangnam_map <- data.table::fread('gangnam_map.csv')
cross_GN2 <- data.table::fread('cross_GN2.csv')
floor_signs_GN <- data.table::fread('floor_signs_GN.csv')
dying_case_GN <- data.table::fread('dying_case_GN.csv')
grid <- data.table::fread('grid.csv')
grid2 <- data.table::fread('grid2.csv')
grid3 <- data.table::fread('grid3.csv')
ydp_map <- data.table::fread('ydp_map.csv')
floor_signs_YDP <- data.table::fread('floor_signs_YDP.csv')
cross_YDP2 <- data.table::fread('cross_YDP2.csv')
grid4 <- data.table::fread('grid4.csv')
grid5 <- data.table::fread('grid5.csv')
grid6 <- data.table::fread('grid6.csv')
yellow_carpets_GN <- data.table::fread('yellow_carpets_GN.csv')
yellow_carpets_GN <- yellow_carpets_GN[,-'V1']
ele_location_GN <- data.table::fread('ele_location_GN.csv')
yellow_carpets_YDP <- data.table::fread('yellow_carpets_YDP.csv')
yellow_carpets_YDP <- yellow_carpets_YDP[,-'V1']
ele_location_YDP <- data.table::fread('ele_location_YDP.csv')
YDP_carpet_1 <- c(left = grid6$vl2[2], bottom = grid6$hl[5], right = grid6$vl2[3], top = grid6$hl[6])
YDP_carpet_2 <- c(left = grid6$vl2[3], bottom = grid6$hl[4], right = grid6$vl2[4], top = grid6$hl[5])
YDP_carpet_3 <- c(left = grid6$vl2[2], bottom = grid6$hl[3], right = grid6$vl2[3], top = grid6$hl[4])
YDP_carpet_4 <- c(left = grid6$vl2[4], bottom = grid6$hl[3], right = grid6$vl2[5], top = grid6$hl[4])
YDP_carpet_5 <- c(left = grid6$vl2[3], bottom = grid6$hl[2], right = grid6$vl2[4], top = grid6$hl[3])
GN_carpet_1 <- c(left = grid3$vl2[2], bottom = grid3$hl[6], right = grid3$vl2[3], top = grid3$hl[7])
GN_carpet_2 <- c(left = grid3$vl2[2], bottom = grid3$hl[5], right = grid3$vl2[3], top = grid3$hl[6])
GN_carpet_3 <- c(left = grid3$vl2[3], bottom = grid3$hl[5], right = grid3$vl2[4], top = grid3$hl[6])
GN_carpet_4 <- c(left = grid3$vl2[3], bottom = grid3$hl[4], right = grid3$vl2[4], top = grid3$hl[5])
GN_carpet_5 <- c(left = grid3$vl2[2], bottom = grid3$hl[3], right = grid3$vl2[3], top = grid3$hl[4])
GN_carpet_6 <- c(left = grid3$vl2[4], bottom = grid3$hl[3], right = grid3$vl2[5], top = grid3$hl[4])
YDP_cross_1 <- c(left = grid6$vl2[2], bottom = grid6$hl[4], right = grid6$vl2[3], top = grid6$hl[5])
YDP_cross_2 <- c(left = grid6$vl2[3], bottom = grid6$hl[4], right = grid6$vl2[4], top = grid6$hl[5])
YDP_cross_3 <- c(left = grid6$vl2[4], bottom = grid6$hl[4], right = grid6$vl2[5], top = grid6$hl[5])
YDP_cross_4 <- c(left = grid6$vl2[3], bottom = grid6$hl[3], right = grid6$vl2[4], top = grid6$hl[4])
YDP_cross_5 <- c(left = grid6$vl2[2], bottom = grid6$hl[1], right = grid6$vl2[3], top = grid6$hl[2])
GN_cross_1 <- c(left = grid3$vl2[2], bottom = grid3$hl[5], right = grid3$vl2[3], top = grid3$hl[6])
GN_cross_2 <- c(left = grid3$vl2[3], bottom = grid3$hl[5], right = grid3$vl2[4], top = grid3$hl[6])
GN_cross_3 <- c(left = grid3$vl2[2], bottom = grid3$hl[4], right = grid3$vl2[3], top = grid3$hl[5])
GN_cross_4 <- c(left = grid3$vl2[3], bottom = grid3$hl[4], right = grid3$vl2[4], top = grid3$hl[5])
GN_cross_5 <- c(left = grid3$vl2[2], bottom = grid3$hl[3], right = grid3$vl2[3], top = grid3$hl[4])
GN_cross_6 <- c(left = grid3$vl2[3], bottom = grid3$hl[3], right = grid3$vl2[4], top = grid3$hl[4])
GN_cross_7 <- c(left = grid3$vl2[4], bottom = grid3$hl[3], right = grid3$vl2[5], top = grid3$hl[4])


ui <- fluidPage(
  titlePanel(strong('LED floor signs & Yellow carpets')),
  
  sidebarLayout(
    sidebarPanel(
      h2(strong('Option Bar')),
      
      br(),
      
      p(strong(span('[강남구/영등포구]', style = 'color:blue')),'에서 자치구를 선택해 각 자치구별로', strong('시설물의 추가 설치가 필요한 구역'),'을 확인하고'),
      p(strong(span('[추천입지]', style = 'color:blue')),'에서 각 옵션을 선택하여',strong('자세한 위치'),'를 지도상에서 확인할 수 있습니다.'),
      p(strong(span('[주의1]', style = 'color:red')),strong('한 번에 하나의 selector'),'에서만 선택이 가능합니다.'),
      p('즉, 다른 selector의 플랏을 보고 싶다면 보고 있던 selector를 <선택안함>으로 변경해주세요.'),
      p(strong(span('[주의2]', style = 'color:red')), '플랏의 크기가 크니 반드시', strong('전체화면'), '으로 확인하시기 바랍니다.'),
      
      br(),
      
      p('아래에 있는',span('체크박스', style = 'color:purple'), '를 이용해 지도위에서 횡단보도와 초등학교 위치를 제거할 수 있습니다.'),
     
      br(),
      
      p(span('[강남구/영등포구]', style = 'color:blue'),'의 초록점은 횡단보도, 파란점은 기존에 설치된 바닥신호등, 주황색 점은 초등학교입니다.
        초등학교보다 더 큰 남색 점은 기존에 설치된 옐로카펫입니다.'),
        
      p(strong('격자 안의 숫자'),'가 시설물 추가 설치가 필요한 구역의 숫자입니다. 각 구역을 확대하려면 아래 selector를 조작해보세요.'),
      
      
      selectInput('location',
                  label = strong('현재 강남구/영등포구'),
                  choices = c('선택안함',
                              '강남구_바닥신호등',
                              '영등포구_바닥신호등',
                              '강남구_옐로카펫',
                              '영등포구_옐로카펫'),
                  selected = '선택안함'
                  ),
      
      p(strong('자치구별 실제 추천 위치'),'를 아래 selector를 조작하며 확인해보세요.'),
      p('노란 동그라미는 추천된 바닥신호등 위치, 연두색 동그라미는 추천된 옐로카펫의 위치입니다.'),
      
      selectInput('GN_signs_section',
                  label = strong('강남구 바닥신호등 추천입지'),
                  choices = c('선택안함','강남구_바닥신호등_구역1', '강남구_바닥신호등_구역2', '강남구_바닥신호등_구역3', '강남구_바닥신호등_구역4', 
                              '강남구_바닥신호등_구역5', '강남구_바닥신호등_구역6', '강남구_바닥신호등_구역7'
                              ),
                  selected = '선택안함'
                  ),
      
      selectInput('YDP_signs_section',
                  label = strong('영등포구 바닥신호등 추천입지'),
                  choices = c('선택안함','영등포구_바닥신호등_구역1', '영등포구_바닥신호등_구역2', '영등포구_바닥신호등_구역3', '영등포구_바닥신호등_구역4', 
                              '영등포구_바닥신호등_구역5'
                              ),
                  
                  selected = '선택안함'
                  ),
      
      selectInput('GN_carpet_section',
                  label = strong('강남구 옐로카펫 추천입지'),
                  choices = c('선택안함','강남구_옐로카펫_구역1', '강남구_옐로카펫_구역2', '강남구_옐로카펫_구역3', '강남구_옐로카펫_구역4', 
                              '강남구_옐로카펫_구역5', '강남구_옐로카펫_구역6'
                              ),
                  selected = '선택안함'
                  ),
      
      selectInput('YDP_carpet_section',
                  label = strong('영등포구 옐로카펫 추천입지'),
                  choices = c('선택안함','영등포구_옐로카펫_구역1', '영등포구_옐로카펫_구역2', '영등포구_옐로카펫_구역3', '영등포구_옐로카펫_구역4', 
                              '영등포구_옐로카펫_구역5'
                              ),
                  selected = '선택안함'
                  ),
      
      p(strong(span('[추천 후 강남구/영등포구]', style = 'color:blue')),'에서 각 옵션을 선택하여',strong('추천 입지가 반영된 지역'),'을 지도상에서 확인할 수 있습니다.'),
      
      selectInput('location2',
                  label = strong('추천 후 강남구/영등포구'),
                  choices = c('선택안함',
                              '강남구_바닥신호등',
                              '영등포구_바닥신호등',
                              '강남구_옐로카펫',
                              '영등포구_옐로카펫'),
                  selected = '선택안함'),
      
      checkboxInput('checkbox',
                    strong('횡단보도/초등학교 위치 노출'),
                    value = TRUE)
    ),
  mainPanel(
    textOutput('selected_option'),
    plotOutput('map', width = "100%", height = "1000px")
  ))
)
# Define server logic ---

server <- function(input, output){
    location_input <- reactive({
      if(input$location == '강남구_바닥신호등' & input$checkbox){
        return(ggmap(GNmap) +
          geom_polygon(data = gangnam_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 3, alpha = 0.5)+
          geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 4)+
          geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 8)+
          geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 4) +
          geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 3)+
          geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 4, color = 'black')+
          geom_jitter()+
          geom_line(data = grid, aes(y = hl, x = vl, group = index), size = 1.2) +
          geom_line(data = grid2, aes(y = hl2, x = vl2, group = index), size = 1.2)+
          geom_point(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[5]+grid3$hl[6])/2), color = 'salmon', size = 20, alpha = 0.1) +
          geom_text(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[5]+grid3$hl[6])/2), label = '1', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
          geom_point(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[5]+grid3$hl[6])/2), color = 'salmon', size = 20, alpha = 0.1) +
          geom_text(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[5]+grid3$hl[6])/2), label = '2', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
          geom_point(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[4]+grid3$hl[5])/2), color = 'salmon', size = 20, alpha = 0.1) +
          geom_text(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[4]+grid3$hl[5])/2), label = '3', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
          geom_point(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[4]+grid3$hl[5])/2), color = 'salmon', size = 20, alpha = 0.1) +
          geom_text(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[4]+grid3$hl[5])/2), label = '4', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
          geom_point(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[3]+grid3$hl[4])/2), color = 'salmon', size = 20, alpha = 0.1) +
          geom_text(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[3]+grid3$hl[4])/2), label = '5', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
          geom_point(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[3]+grid3$hl[4])/2), color = 'salmon', size = 20, alpha = 0.1) +
          geom_text(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[3]+grid3$hl[4])/2), label = '6', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
          geom_point(data = grid3, aes(x = (grid3$vl2[4]+grid3$vl2[5])/2, y = (grid3$hl[3]+grid3$hl[4])/2), color = 'salmon', size = 20, alpha = 0.1) +
          geom_text(data = grid3, aes(x = (grid3$vl2[4]+grid3$vl2[5])/2, y = (grid3$hl[3]+grid3$hl[4])/2), label = '7', color = 'ivory', size = 20, alpha = 0.5, shape = 15)
        )}
      if (input$location == '강남구_바닥신호등' & input$checkbox == FALSE){
        return(ggmap(GNmap) +
                 geom_polygon(data = gangnam_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 3, alpha = 0.5)+
                 geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 8)+
                 geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 4) +
                 geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 3)+
                 geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 4, color = 'black')+
                 geom_jitter()+
                 geom_line(data = grid, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid2, aes(y = hl2, x = vl2, group = index), size = 1.2)+
                 geom_point(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[5]+grid3$hl[6])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[5]+grid3$hl[6])/2), label = '1', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[5]+grid3$hl[6])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[5]+grid3$hl[6])/2), label = '2', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[4]+grid3$hl[5])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[4]+grid3$hl[5])/2), label = '3', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[4]+grid3$hl[5])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[4]+grid3$hl[5])/2), label = '4', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[3]+grid3$hl[4])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[3]+grid3$hl[4])/2), label = '5', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[3]+grid3$hl[4])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[3]+grid3$hl[4])/2), label = '6', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[4]+grid3$vl2[5])/2, y = (grid3$hl[3]+grid3$hl[4])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[4]+grid3$vl2[5])/2, y = (grid3$hl[3]+grid3$hl[4])/2), label = '7', color = 'ivory', size = 20, alpha = 0.5, shape = 15)
        )}
      if (input$location == '영등포구_바닥신호등' & input$checkbox){
        return(ggmap(YDPmap) + 
                  geom_polygon(data = ydp_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 2, alpha = 0.5)+
                  geom_point(data = floor_signs_YDP,
                             aes(x = lon,y = lat), color = 'blue', size = 8)+
                  geom_point(data = cross_YDP2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 4) +
                  geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 6) +
                  geom_line(data = grid4, aes(y = hl, x = vl, group = index), size = 1.2) +
                  geom_line(data = grid5, aes(y = hl2, x = vl2, group = index), size = 1.2) +
                  geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 5, color = 'black')+
                  geom_point(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[4]+grid6$hl[5])/2), color = 'salmon', size = 28, alpha = 0.1) +
                  geom_text(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[4]+grid6$hl[5])/2), label = '1', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                  geom_point(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[4]+grid6$hl[5])/2), color = 'salmon', size = 28, alpha = 0.1) +
                  geom_text(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[4]+grid6$hl[5])/2), label = '2', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                  geom_point(data = grid6, aes(x = (grid6$vl2[4]+grid6$vl2[5])/2, y = (grid6$hl[4]+grid6$hl[5])/2), color = 'salmon', size = 28, alpha = 0.1) +
                  geom_text(data = grid6, aes(x = (grid6$vl2[4]+grid6$vl2[5])/2, y = (grid6$hl[4]+grid6$hl[5])/2), label = '3', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                  geom_point(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[3]+grid6$hl[4])/2), color = 'salmon', size = 28, alpha = 0.1) +
                  geom_text(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[3]+grid6$hl[4])/2), label = '4', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                  geom_point(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[1]+grid6$hl[2])/2), color = 'salmon', size = 28, alpha = 0.1) +
                  geom_text(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[1]+grid6$hl[2])/2), label = '5', color = 'ivory', size = 20, alpha = 0.5, shape = 15)
        )}
      if (input$location == '영등포구_바닥신호등' & input$checkbox == FALSE){
        return(ggmap(YDPmap) + 
                 geom_polygon(data = ydp_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 2, alpha = 0.5)+
                 geom_point(data = floor_signs_YDP,
                            aes(x = lon,y = lat), color = 'blue', size = 8)+
                 geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 6) +
                 geom_line(data = grid4, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid5, aes(y = hl2, x = vl2, group = index), size = 1.2) +
                 geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 5, color = 'black')+
                 geom_point(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[4]+grid6$hl[5])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[4]+grid6$hl[5])/2), label = '1', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[4]+grid6$hl[5])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[4]+grid6$hl[5])/2), label = '2', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid6, aes(x = (grid6$vl2[4]+grid6$vl2[5])/2, y = (grid6$hl[4]+grid6$hl[5])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[4]+grid6$vl2[5])/2, y = (grid6$hl[4]+grid6$hl[5])/2), label = '3', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[3]+grid6$hl[4])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[3]+grid6$hl[4])/2), label = '4', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[1]+grid6$hl[2])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[1]+grid6$hl[2])/2), label = '5', color = 'ivory', size = 20, alpha = 0.5, shape = 15)
        )}
      if (input$location == '강남구_옐로카펫' & input$checkbox){
        return(ggmap(GNmap) +
                 geom_polygon(data = gangnam_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 3, alpha = 0.5)+
                 geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 11) +
                 geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'salmon', size = 8) +
                 geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'white', size = 6) +
                 geom_text(data = ele_location_GN, aes(x = lon, y =lat), label = ele_location_GN$index, size = 5, color = 'black')+
                 geom_line(data = grid, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid2, aes(y = hl2, x = vl2, group = index), size = 1.2)+
                 geom_point(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[6]+grid3$hl[7])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[6]+grid3$hl[7])/2), label = '1', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[5]+grid3$hl[6])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[5]+grid3$hl[6])/2), label = '2', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[5]+grid3$hl[6])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[5]+grid3$hl[6])/2), label = '3', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[4]+grid3$hl[5])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[4]+grid3$hl[5])/2), label = '4', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[3]+grid3$hl[4])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[3]+grid3$hl[4])/2), label = '5', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[4]+grid3$vl2[5])/2, y = (grid3$hl[3]+grid3$hl[4])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[4]+grid3$vl2[5])/2, y = (grid3$hl[3]+grid3$hl[4])/2), label = '6', color = 'ivory', size = 20, alpha = 0.5, shape = 15)
        )}
      if (input$location == '강남구_옐로카펫' & input$checkbox == FALSE){
        return(ggmap(GNmap) +
                 geom_polygon(data = gangnam_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 3, alpha = 0.5)+
                 geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 11) +
                 geom_line(data = grid, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid2, aes(y = hl2, x = vl2, group = index), size = 1.2)+
                 geom_point(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[6]+grid3$hl[7])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[6]+grid3$hl[7])/2), label = '1', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[5]+grid3$hl[6])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[5]+grid3$hl[6])/2), label = '2', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[5]+grid3$hl[6])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[5]+grid3$hl[6])/2), label = '3', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[4]+grid3$hl[5])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[3]+grid3$vl2[4])/2, y = (grid3$hl[4]+grid3$hl[5])/2), label = '4', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[3]+grid3$hl[4])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[2]+grid3$vl2[3])/2, y = (grid3$hl[3]+grid3$hl[4])/2), label = '5', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid3, aes(x = (grid3$vl2[4]+grid3$vl2[5])/2, y = (grid3$hl[3]+grid3$hl[4])/2), color = 'salmon', size = 20, alpha = 0.1) +
                 geom_text(data = grid3, aes(x = (grid3$vl2[4]+grid3$vl2[5])/2, y = (grid3$hl[3]+grid3$hl[4])/2), label = '6', color = 'ivory', size = 20, alpha = 0.5, shape = 15)
        )}
      if (input$location == '영등포구_옐로카펫' & input$checkbox){
        return(ggmap(YDPmap) +
                 geom_polygon(data = ydp_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 2, alpha = 0.5)+
                 geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size =11) +
                 geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'salmon', size = 8) +
                 geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'white', size = 6) +
                 geom_text(data = ele_location_YDP, aes(x = lon, y =lat), label = ele_location_YDP$index, size = 5, color = 'black') +
                 geom_line(data = grid4, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid5, aes(y = hl2, x = vl2, group = index), size = 1.2)+
                 geom_point(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[5]+grid6$hl[6])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[5]+grid6$hl[6])/2), label = '1', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[4]+grid6$hl[5])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[4]+grid6$hl[5])/2), label = '2', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[3]+grid6$hl[4])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[3]+grid6$hl[4])/2), label = '3', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid6, aes(x = (grid6$vl2[4]+grid6$vl2[5])/2, y = (grid6$hl[3]+grid6$hl[4])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[4]+grid6$vl2[5])/2, y = (grid6$hl[3]+grid6$hl[4])/2), label = '4', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[2]+grid6$hl[3])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[2]+grid6$hl[3])/2), label = '5', color = 'ivory', size = 20, alpha = 0.5, shape = 15)
        )}
      if (input$location == '영등포구_옐로카펫' & input$checkbox == FALSE){
        return(ggmap(YDPmap) +
                 geom_polygon(data = ydp_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 2, alpha = 0.5)+
                 geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size =11) +
                 geom_line(data = grid4, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid5, aes(y = hl2, x = vl2, group = index), size = 1.2)+
                 geom_point(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[5]+grid6$hl[6])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[5]+grid6$hl[6])/2), label = '1', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[4]+grid6$hl[5])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[4]+grid6$hl[5])/2), label = '2', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[3]+grid6$hl[4])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[2]+grid6$vl2[3])/2, y = (grid6$hl[3]+grid6$hl[4])/2), label = '3', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid6, aes(x = (grid6$vl2[4]+grid6$vl2[5])/2, y = (grid6$hl[3]+grid6$hl[4])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[4]+grid6$vl2[5])/2, y = (grid6$hl[3]+grid6$hl[4])/2), label = '4', color = 'ivory', size = 20, alpha = 0.5, shape = 15) +
                 geom_point(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[2]+grid6$hl[3])/2), color = 'salmon', size = 28, alpha = 0.1) +
                 geom_text(data = grid6, aes(x = (grid6$vl2[3]+grid6$vl2[4])/2, y = (grid6$hl[2]+grid6$hl[3])/2), label = '5', color = 'ivory', size = 20, alpha = 0.5, shape = 15)
        )}
      if(input$GN_signs_section == '강남구_바닥신호등_구역1' & input$checkbox){
        return(
          get_stamenmap(GN_cross_1, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 8)+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'white', size = 4) +
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 4)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.035357317681, y = 37.515407676250874), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.035357317681, y = 37.515407676250874), color = 'white', size = 9) +
            geom_text(aes(x = 127.035357317681, y = 37.515407676250874), label = '서울세관사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_signs_section == '강남구_바닥신호등_구역1' & input$checkbox == FALSE){
        return(
          get_stamenmap(GN_cross_1, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 4)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.035357317681, y = 37.515407676250874), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.035357317681, y = 37.515407676250874), color = 'white', size = 9) +
            geom_text(aes(x = 127.035357317681, y = 37.515407676250874), label = '서울세관사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_signs_section == '강남구_바닥신호등_구역2' & input$checkbox){
        return(
          get_stamenmap(GN_cross_2, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 8)+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'white', size = 4) +
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 8)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.0600895109839, y = 37.514512568419995), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.0600895109839, y = 37.514512568419995), color = 'white', size = 9) +
            geom_text(aes(x = 127.0600895109839, y = 37.514512568419995), label = '코엑스사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_signs_section == '강남구_바닥신호등_구역2' & input$checkbox == FALSE){
        return(
          get_stamenmap(GN_cross_2, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 8)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.0600895109839, y = 37.514512568419995), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.0600895109839, y = 37.514512568419995), color = 'white', size = 9) +
            geom_text(aes(x = 127.0600895109839, y = 37.514512568419995), label = '코엑스사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_signs_section == '강남구_바닥신호등_구역3' & input$checkbox){
        return(
          get_stamenmap(GN_cross_3, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 8)+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'white', size = 4) +
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 8)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.04272134041682, y = 37.50266294016471), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.04272134041682, y = 37.50266294016471), color = 'white', size = 9) +
            geom_text(aes(x = 127.04272134041682, y = 37.50266294016471), label = '르네상스호텔사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_signs_section == '강남구_바닥신호등_구역3' & input$checkbox == FALSE){
        return(
          get_stamenmap(GN_cross_3, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 8)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.04272134041682, y = 37.50266294016471), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.04272134041682, y = 37.50266294016471), color = 'white', size = 9) +
            geom_text(aes(x = 127.04272134041682, y = 37.50266294016471), label = '르네상스호텔사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_signs_section == '강남구_바닥신호등_구역4' & input$checkbox){
        return(
          get_stamenmap(GN_cross_4, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 8)+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'white', size = 4) +
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 8)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.05863693253951, y = 37.50305791512272), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.05863693253951, y = 37.50305791512272), color = 'white', size = 9) +
            geom_text(aes(x = 127.05863693253951, y = 37.50305791512272), label = '대치사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_signs_section == '강남구_바닥신호등_구역4' & input$checkbox == FALSE){
        return(
          get_stamenmap(GN_cross_4, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 8)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.05863693253951, y = 37.50305791512272), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.05863693253951, y = 37.50305791512272), color = 'white', size = 9) +
            geom_text(aes(x = 127.05863693253951, y = 37.50305791512272), label = '대치사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_signs_section == '강남구_바닥신호등_구역5' & input$checkbox){
        return(
          get_stamenmap(GN_cross_5, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 8)+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'white', size = 4) +
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 8)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.03923330526129, y = 37.49581649679672), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.03923330526129, y = 37.49581649679672), color = 'white', size = 9) +
            geom_text(aes(x = 127.03923330526129, y = 37.49581649679672), label = '구역삼세무서사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_signs_section == '강남구_바닥신호등_구역5' & input$checkbox == FALSE){
        return(
          get_stamenmap(GN_cross_5, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 8)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.03923330526129, y = 37.49581649679672), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.03923330526129, y = 37.49581649679672), color = 'white', size = 9) +
            geom_text(aes(x = 127.03923330526129, y = 37.49581649679672), label = '구역삼세무서사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_signs_section == '강남구_바닥신호등_구역6' & input$checkbox){
        return(
          get_stamenmap(GN_cross_6, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 8)+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'white', size = 4) +
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 8)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.05973612711864, y = 37.49288690054449), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.05973612711864, y = 37.49288690054449), color = 'white', size = 9) +
            geom_text(aes(x = 127.05973612711864, y = 37.49288690054449), label = '남부순환로삼거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_signs_section == '강남구_바닥신호등_구역6' & input$checkbox == FALSE){
        return(
          get_stamenmap(GN_cross_6, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 8)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.05973612711864, y = 37.49288690054449), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.05973612711864, y = 37.49288690054449), color = 'white', size = 9) +
            geom_text(aes(x = 127.05973612711864, y = 37.49288690054449), label = '남부순환로삼거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_signs_section == '강남구_바닥신호등_구역7' & input$checkbox){
        return(
          get_stamenmap(GN_cross_7, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 8)+
            geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'white', size = 4) +
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 8)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.08073363302394, y = 37.49098810948115), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.08073363302394, y = 37.49098810948115), color = 'white', size = 9) +
            geom_text(aes(x = 127.08073363302394, y = 37.49098810948115), label = '영희초교사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_signs_section == '강남구_바닥신호등_구역7' & input$checkbox == FALSE){
        return(
          get_stamenmap(GN_cross_7, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 7)+
            geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 8)+
            geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 7, color = 'black') +
            geom_point(aes(x = 127.08073363302394, y = 37.49098810948115), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 127.08073363302394, y = 37.49098810948115), color = 'white', size = 9) +
            geom_text(aes(x = 127.08073363302394, y = 37.49098810948115), label = '영희초교사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_signs_section == '영등포구_바닥신호등_구역1' & input$checkbox){
        return(
          get_stamenmap(YDP_cross_1, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_YDP,
                       aes(x = lon,y = lat), color = 'blue', size = 11)+
            geom_point(data = cross_YDP2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 8) +
            geom_point(data = cross_YDP2, aes(x = 경도, y = 위도), color = 'white', size = 4) +
            geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 7) +
            geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 6, color = 'black') +
            geom_point(aes(x = 126.89558861109023, y = 37.52137811309407), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 126.89558861109023, y = 37.52137811309407), color = 'white', size = 9) +
            geom_text(aes(x = 126.89558861109023, y = 37.52137811309407), label = '영등포유통상가사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_signs_section == '영등포구_바닥신호등_구역1' & input$checkbox == FALSE){
        return(
          get_stamenmap(YDP_cross_1, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_YDP,
                       aes(x = lon,y = lat), color = 'blue', size = 11) +
            geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 7) +
            geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 6, color = 'black') +
            geom_point(aes(x = 126.89558861109023, y = 37.52137811309407), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 126.89558861109023, y = 37.52137811309407), color = 'white', size = 9) +
            geom_text(aes(x = 126.89558861109023, y = 37.52137811309407), label = '영등포유통상가사거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_signs_section == '영등포구_바닥신호등_구역2' & input$checkbox){
        return(
          get_stamenmap(YDP_cross_2, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_YDP,
                       aes(x = lon,y = lat), color = 'blue', size = 11)+
            geom_point(data = cross_YDP2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 8) +
            geom_point(data = cross_YDP2, aes(x = 경도, y = 위도), color = 'white', size = 4) +
            geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 7) +
            geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 6, color = 'black') +
            geom_point(aes(x = 126.90402503764408, y = 37.528845142265446), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 126.90402503764408, y = 37.528845142265446), color = 'white', size = 9) +
            geom_text(aes(x = 126.90402503764408, y = 37.528845142265446), label = '당산동4가', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_signs_section == '영등포구_바닥신호등_구역2' & input$checkbox == FALSE){
        return(
          get_stamenmap(YDP_cross_2, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_YDP,
                       aes(x = lon,y = lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 7) +
            geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 6, color = 'black') +
            geom_point(aes(x = 126.90402503764408, y = 37.528845142265446), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 126.90402503764408, y = 37.528845142265446), color = 'white', size = 9) +
            geom_text(aes(x = 126.90402503764408, y = 37.528845142265446), label = '당산동4가', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_signs_section == '영등포구_바닥신호등_구역3' & input$checkbox){
        return(
          get_stamenmap(YDP_cross_3, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_YDP,
                       aes(x = lon,y = lat), color = 'blue', size = 11)+
            geom_point(data = cross_YDP2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 8) +
            geom_point(data = cross_YDP2, aes(x = 경도, y = 위도), color = 'white', size = 4) +
            geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 7) +
            geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 6, color = 'black') +
            geom_point(aes(x = 126.92442615007039, y = 37.52842208139561), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 126.92442615007039, y = 37.52842208139561), color = 'white', size = 9) +
            geom_text(aes(x = 126.92442615007039, y = 37.52842208139561), label = '여의도공원앞', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_signs_section == '영등포구_바닥신호등_구역3' & input$checkbox == FALSE){
        return(
          get_stamenmap(YDP_cross_3, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_YDP,
                       aes(x = lon,y = lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 7) +
            geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 6, color = 'black') +
            geom_point(aes(x = 126.92442615007039, y = 37.52842208139561), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 126.92442615007039, y = 37.52842208139561), color = 'white', size = 9) +
            geom_text(aes(x = 126.92442615007039, y = 37.52842208139561), label = '여의도공원앞', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_signs_section == '영등포구_바닥신호등_구역4' & input$checkbox){
        return(
          get_stamenmap(YDP_cross_4, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_YDP,
                       aes(x = lon,y = lat), color = 'blue', size = 11)+
            geom_point(data = cross_YDP2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 8) +
            geom_point(data = cross_YDP2, aes(x = 경도, y = 위도), color = 'white', size = 4) +
            geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 7) +
            geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 6, color = 'black') +
            geom_point(aes(x = 126.90894906626924, y = 37.512991243797764), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 126.90894906626924, y = 37.512991243797764), color = 'white', size = 9) +
            geom_text(aes(x = 126.90894906626924, y = 37.512991243797764), label = '영신로삼거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_signs_section == '영등포구_바닥신호등_구역4' & input$checkbox == FALSE){
        return(
          get_stamenmap(YDP_cross_4, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_YDP,
                       aes(x = lon,y = lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 7) +
            geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 6, color = 'black') +
            geom_point(aes(x = 126.90894906626924, y = 37.512991243797764), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 126.90894906626924, y = 37.512991243797764), color = 'white', size = 9) +
            geom_text(aes(x = 126.90894906626924, y = 37.512991243797764), label = '영신로삼거리', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_signs_section == '영등포구_바닥신호등_구역5' & input$checkbox){
        return(
          get_stamenmap(YDP_cross_5, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_YDP,
                       aes(x = lon,y = lat), color = 'blue', size = 11)+
            geom_point(data = cross_YDP2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 8) +
            geom_point(data = cross_YDP2, aes(x = 경도, y = 위도), color = 'white', size = 4) +
            geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 7) +
            geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 6, color = 'black') +
            geom_point(aes(x = 126.89706247494674, y = 37.492991688400686), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 126.89706247494674, y = 37.492991688400686), color = 'white', size = 9) +
            geom_text(aes(x = 126.89706247494674, y = 37.492991688400686), label = '대림역7호선', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_signs_section == '영등포구_바닥신호등_구역5' & input$checkbox == FALSE){
        return(
          get_stamenmap(YDP_cross_5, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = floor_signs_YDP,
                       aes(x = lon,y = lat), color = 'blue', size = 11)+
            geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 7) +
            geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 6, color = 'black') +
            geom_point(aes(x = 126.89706247494674, y = 37.492991688400686), color = 'goldenrod1', size = 13) +
            geom_point(aes(x = 126.89706247494674, y = 37.492991688400686), color = 'white', size = 9) +
            geom_text(aes(x = 126.89706247494674, y = 37.492991688400686), label = '대림역7호선', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_carpet_section == '강남구_옐로카펫_구역1' & input$checkbox){
        return(
          get_stamenmap(GN_carpet_1, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'salmon', size = 8) +
            geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'white', size = 6) +
            geom_text(data = ele_location_GN, aes(x = lon, y =lat), label = ele_location_GN$index, size = 5, color = 'black') +
            geom_point(aes(x = 127.0311435404175, y = 37.53181541805076), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.0311435404175, y = 37.53181541805076), color = 'white', size = 9) +
            geom_text(aes(x = 127.0311435404175, y = 37.53181541805076), label = '압구정초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 127.04518435576267, y = 37.52834560769805), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.04518435576267, y = 37.52834560769805), color = 'white', size = 9) +
            geom_text(aes(x = 127.04518435576267, y = 37.52834560769805), label = '청담초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_carpet_section == '강남구_옐로카펫_구역1' & input$checkbox == FALSE){
        return(
          get_stamenmap(GN_carpet_1, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(aes(x = 127.0311435404175, y = 37.53181541805076), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.0311435404175, y = 37.53181541805076), color = 'white', size = 9) +
            geom_text(aes(x = 127.0311435404175, y = 37.53181541805076), label = '압구정초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 127.04518435576267, y = 37.52834560769805), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.04518435576267, y = 37.52834560769805), color = 'white', size = 9) +
            geom_text(aes(x = 127.04518435576267, y = 37.52834560769805), label = '청담초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_carpet_section == '강남구_옐로카펫_구역2' & input$checkbox){
        return(
          get_stamenmap(GN_carpet_2, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'salmon', size = 8) +
            geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'white', size = 6) +
            geom_text(data = ele_location_GN, aes(x = lon, y =lat), label = ele_location_GN$index, size = 5, color = 'black') +
            geom_point(aes(x = 127.04506052692615, y = 37.519882922177885), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.04506052692615, y = 37.519882922177885), color = 'white', size = 9) +
            geom_text(aes(x = 127.04506052692615, y = 37.519882922177885), label = '언북초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 127.03985873960927, y = 37.51210844672435), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.03985873960927, y = 37.51210844672435), color = 'white', size = 9) +
            geom_text(aes(x = 127.03985873960927, y = 37.51210844672435), label = '학동초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_carpet_section == '강남구_옐로카펫_구역2' & input$checkbox == FALSE){
        return(
          get_stamenmap(GN_carpet_2, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(aes(x = 127.04506052692615, y = 37.519882922177885), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.04506052692615, y = 37.519882922177885), color = 'white', size = 9) +
            geom_text(aes(x = 127.04506052692615, y = 37.519882922177885), label = '언북초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 127.03985873960927, y = 37.51210844672435), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.03985873960927, y = 37.51210844672435), color = 'white', size = 9) +
            geom_text(aes(x = 127.03985873960927, y = 37.51210844672435), label = '학동초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_carpet_section == '강남구_옐로카펫_구역3' & input$checkbox){
        return(
          get_stamenmap(GN_carpet_3, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'salmon', size = 8) +
            geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'white', size = 6) +
            geom_text(data = ele_location_GN, aes(x = lon, y =lat), label = ele_location_GN$index, size = 5, color = 'black') +
            geom_point(aes(x = 127.06099209808988, y = 37.520255631003394), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.06099209808988, y = 37.520255631003394), color = 'white', size = 9) +
            geom_text(aes(x = 127.06099209808988, y = 37.520255631003394), label = '봉은초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_carpet_section == '강남구_옐로카펫_구역3' & input$checkbox == FALSE){
        return(
          get_stamenmap(GN_carpet_3, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(aes(x = 127.06099209808988, y = 37.520255631003394), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.06099209808988, y = 37.520255631003394), color = 'white', size = 9) +
            geom_text(aes(x = 127.06099209808988, y = 37.520255631003394), label = '봉은초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_carpet_section == '강남구_옐로카펫_구역4' & input$checkbox){
        return(
          get_stamenmap(GN_carpet_4, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'salmon', size = 8) +
            geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'white', size = 6) +
            geom_text(data = ele_location_GN, aes(x = lon, y =lat), label = ele_location_GN$index, size = 5, color = 'black') +
            geom_point(aes(x = 127.06333595636745, y = 37.50334436928764), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.06333595636745, y = 37.50334436928764), color = 'white', size = 9) +
            geom_text(aes(x = 127.06333595636745, y = 37.50334436928764), label = '대현초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 127.04903976925294, y = 37.50130610045472), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.04903976925294, y = 37.50130610045472), color = 'white', size = 9) +
            geom_text(aes(x = 127.04903976925294, y = 37.50130610045472), label = '도성초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_carpet_section == '강남구_옐로카펫_구역4' & input$checkbox == FALSE){
        return(
          get_stamenmap(GN_carpet_4, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(aes(x = 127.06333595636745, y = 37.50334436928764), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.06333595636745, y = 37.50334436928764), color = 'white', size = 9) +
            geom_text(aes(x = 127.06333595636745, y = 37.50334436928764), label = '대현초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 127.04903976925294, y = 37.50130610045472), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.04903976925294, y = 37.50130610045472), color = 'white', size = 9) +
            geom_text(aes(x = 127.04903976925294, y = 37.50130610045472), label = '도성초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_carpet_section == '강남구_옐로카펫_구역5' & input$checkbox){
        return(
          get_stamenmap(GN_carpet_5, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'salmon', size = 8) +
            geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'white', size = 6) +
            geom_text(data = ele_location_GN, aes(x = lon, y =lat), label = ele_location_GN$index, size = 5, color = 'black') +
            geom_point(aes(x = 127.0327933134344, y = 37.492879843882854), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.0327933134344, y = 37.492879843882854), color = 'white', size = 9) +
            geom_text(aes(x = 127.0327933134344, y = 37.492879843882854), label = '역삼초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_carpet_section == '강남구_옐로카펫_구역5' & input$checkbox == FALSE){
        return(
          get_stamenmap(GN_carpet_5, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(aes(x = 127.0327933134344, y = 37.492879843882854), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.0327933134344, y = 37.492879843882854), color = 'white', size = 9) +
            geom_text(aes(x = 127.0327933134344, y = 37.492879843882854), label = '역삼초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_carpet_section == '강남구_옐로카펫_구역6' & input$checkbox){
        return(
          get_stamenmap(GN_carpet_6, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'salmon', size = 8) +
            geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'white', size = 6) +
            geom_text(data = ele_location_GN, aes(x = lon, y =lat), label = ele_location_GN$index, size = 5, color = 'black') +
            geom_point(aes(x = 127.0760192517693, y = 37.48991673550949), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.0760192517693, y = 37.48991673550949), color = 'white', size = 9) +
            geom_text(aes(x = 127.0760192517693, y = 37.48991673550949), label = '일원초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 127.06932385390749, y = 37.49043709826502), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.06932385390749, y = 37.49043709826502), color = 'white', size = 9) +
            geom_text(aes(x = 127.06932385390749, y = 37.49043709826502), label = '양전초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$GN_carpet_section == '강남구_옐로카펫_구역6' & input$checkbox == FALSE){
        return(
          get_stamenmap(GN_carpet_6, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(aes(x = 127.0760192517693, y = 37.48991673550949), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.0760192517693, y = 37.48991673550949), color = 'white', size = 9) +
            geom_text(aes(x = 127.0760192517693, y = 37.48991673550949), label = '일원초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 127.06932385390749, y = 37.49043709826502), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 127.06932385390749, y = 37.49043709826502), color = 'white', size = 9) +
            geom_text(aes(x = 127.06932385390749, y = 37.49043709826502), label = '양전초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_carpet_section == '영등포구_옐로카펫_구역1' & input$checkbox){
        return(
          get_stamenmap(YDP_carpet_1, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'salmon', size = 8) +
            geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'white', size = 6) +
            geom_text(data = ele_location_YDP, aes(x = lon, y =lat), label = ele_location_YDP$index, size = 5, color = 'black') +
            geom_point(aes(x = 126.8961719041273, y = 37.53943309077306), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.8961719041273, y = 37.53943309077306), color = 'white', size = 9) +
            geom_text(aes(x = 126.8961719041273, y = 37.53943309077306), label = '당산초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 126.89265722692637, y = 37.53273800018154), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.89265722692637, y = 37.53273800018154), color = 'white', size = 9) +
            geom_text(aes(x = 126.89265722692637, y = 37.53273800018154), label = '선유초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_carpet_section == '영등포구_옐로카펫_구역1' & input$checkbox == FALSE){
        return(
          get_stamenmap(YDP_carpet_1, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(aes(x = 126.8961719041273, y = 37.53943309077306), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.8961719041273, y = 37.53943309077306), color = 'white', size = 9) +
            geom_text(aes(x = 126.8961719041273, y = 37.53943309077306), label = '당산초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 126.89265722692637, y = 37.53273800018154), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.89265722692637, y = 37.53273800018154), color = 'white', size = 9) +
            geom_text(aes(x = 126.89265722692637, y = 37.53273800018154), label = '선유초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_carpet_section == '영등포구_옐로카펫_구역2' & input$checkbox){
        return(
          get_stamenmap(YDP_carpet_2, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'salmon', size = 8) +
            geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'white', size = 6) +
            geom_text(data = ele_location_YDP, aes(x = lon, y =lat), label = ele_location_YDP$index, size = 5, color = 'black') +
            geom_point(aes(x = 126.90721088241837, y = 37.52681354423924), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.90721088241837, y = 37.52681354423924), color = 'white', size = 9) +
            geom_text(aes(x = 126.90721088241837, y = 37.52681354423924), label = '영동초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 126.90380891158105, y = 37.52351270687672), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.90380891158105, y = 37.52351270687672), color = 'white', size = 9) +
            geom_text(aes(x = 126.90380891158105, y = 37.52351270687672), label = '영중초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_carpet_section == '영등포구_옐로카펫_구역2' & input$checkbox == FALSE){
        return(
          get_stamenmap(YDP_carpet_2, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(aes(x = 126.90721088241837, y = 37.52681354423924), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.90721088241837, y = 37.52681354423924), color = 'white', size = 9) +
            geom_text(aes(x = 126.90721088241837, y = 37.52681354423924), label = '영동초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 126.90380891158105, y = 37.52351270687672), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.90380891158105, y = 37.52351270687672), color = 'white', size = 9) +
            geom_text(aes(x = 126.90380891158105, y = 37.52351270687672), label = '영중초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_carpet_section == '영등포구_옐로카펫_구역3' & input$checkbox){
        return(
          get_stamenmap(YDP_carpet_3, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'salmon', size = 8) +
            geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'white', size = 6) +
            geom_text(data = ele_location_YDP, aes(x = lon, y =lat), label = ele_location_YDP$index, size = 5, color = 'black') +
            geom_point(aes(x = 126.89325689808979, y = 37.51876260119002), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.89325689808979, y = 37.51876260119002), color = 'white', size = 9) +
            geom_text(aes(x = 126.89325689808979, y = 37.51876260119002), label = '문래초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 126.89837094711648, y = 37.51306328597632), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.89837094711648, y = 37.51306328597632), color = 'white', size = 9) +
            geom_text(aes(x = 126.89837094711648, y = 37.51306328597632), label = '영등포초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_carpet_section == '영등포구_옐로카펫_구역3' & input$checkbox == FALSE){
        return(
          get_stamenmap(YDP_carpet_3, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(aes(x = 126.89325689808979, y = 37.51876260119002), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.89325689808979, y = 37.51876260119002), color = 'white', size = 9) +
            geom_text(aes(x = 126.89325689808979, y = 37.51876260119002), label = '문래초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 126.89837094711648, y = 37.51306328597632), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.89837094711648, y = 37.51306328597632), color = 'white', size = 9) +
            geom_text(aes(x = 126.89837094711648, y = 37.51306328597632), label = '영등포초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_carpet_section == '영등포구_옐로카펫_구역4' & input$checkbox){
        return(
          get_stamenmap(YDP_carpet_4, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'salmon', size = 8) +
            geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'white', size = 6) +
            geom_text(data = ele_location_YDP, aes(x = lon, y =lat), label = ele_location_YDP$index, size = 5, color = 'black') +
            geom_point(aes(x = 126.92271699623565, y = 37.51987334120885), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.92271699623565, y = 37.51987334120885), color = 'white', size = 9) +
            geom_text(aes(x = 126.92271699623565, y = 37.51987334120885), label = '윤중초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 126.9181006827444, y = 37.510003445445406), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.9181006827444, y = 37.510003445445406), color = 'white', size = 9) +
            geom_text(aes(x = 126.9181006827444, y = 37.510003445445406), label = '영신초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_carpet_section == '영등포구_옐로카펫_구역4' & input$checkbox == FALSE){
        return(
          get_stamenmap(YDP_carpet_4, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(aes(x = 126.92271699623565, y = 37.51987334120885), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.92271699623565, y = 37.51987334120885), color = 'white', size = 9) +
            geom_text(aes(x = 126.92271699623565, y = 37.51987334120885), label = '윤중초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 126.9181006827444, y = 37.510003445445406), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.9181006827444, y = 37.510003445445406), color = 'white', size = 9) +
            geom_text(aes(x = 126.9181006827444, y = 37.510003445445406), label = '영신초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_carpet_section == '영등포구_옐로카펫_구역5' & input$checkbox){
        return(
          get_stamenmap(YDP_carpet_5, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'salmon', size = 8) +
            geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'white', size = 6) +
            geom_text(data = ele_location_YDP, aes(x = lon, y =lat), label = ele_location_YDP$index, size = 5, color = 'black') +
            geom_point(aes(x = 126.9133792827441, y = 37.49823775501695), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.9133792827441, y = 37.49823775501695), color = 'white', size = 9) +
            geom_text(aes(x = 126.9133792827441, y = 37.49823775501695), label = '대길초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 126.90595498459844, y = 37.502574874743054), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.90595498459844, y = 37.502574874743054), color = 'white', size = 9) +
            geom_text(aes(x = 126.90595498459844, y = 37.502574874743054), label = '대영초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$YDP_carpet_section == '영등포구_옐로카펫_구역5' & input$checkbox == FALSE){
        return(
          get_stamenmap(YDP_carpet_5, zoom = 16, type = 'toner-lite') %>%
            ggmap()+
            geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size = 13) +
            geom_point(aes(x = 126.9133792827441, y = 37.49823775501695), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.9133792827441, y = 37.49823775501695), color = 'white', size = 9) +
            geom_text(aes(x = 126.9133792827441, y = 37.49823775501695), label = '대길초등학교', color = 'black', size = 7, fontface = 'bold') +
            geom_point(aes(x = 126.90595498459844, y = 37.502574874743054), color = 'seagreen1', size = 15) +
            geom_point(aes(x = 126.90595498459844, y = 37.502574874743054), color = 'white', size = 9) +
            geom_text(aes(x = 126.90595498459844, y = 37.502574874743054), label = '대영초등학교', color = 'black', size = 7, fontface = 'bold')
        )
      }
      if(input$location2 == '강남구_바닥신호등' & input$checkbox){
        return(ggmap(GNmap) +
                 geom_polygon(data = gangnam_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 3, alpha = 0.5)+
                 geom_point(data = cross_GN2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 4)+
                 geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 8)+
                 geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 4) +
                 geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 3)+
                 geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 4, color = 'black')+
                 geom_jitter()+
                 geom_line(data = grid, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid2, aes(y = hl2, x = vl2, group = index), size = 1.2)+
                 geom_point(aes(x = 127.035357317681, y = 37.515607676250874), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.035357317681, y = 37.515407676250874), color = 'white', size = 4) +
                 geom_point(aes(x = 127.0600895109839, y = 37.514512568419995), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.0600895109839, y = 37.514512568419995), color = 'white', size = 4) +
                 geom_point(aes(x = 127.04272134041682, y = 37.50266294016471), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.04272134041682, y = 37.50266294016471), color = 'white', size = 4) +
                 geom_point(aes(x = 127.05863693253951, y = 37.50305791512272), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.05863693253951, y = 37.50305791512272), color = 'white', size = 4) +
                 geom_point(aes(x = 127.03923330526129, y = 37.49581649679672), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.03923330526129, y = 37.49581649679672), color = 'white', size = 4) +
                 geom_point(aes(x = 127.05973612711864, y = 37.49288690054449), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.05973612711864, y = 37.49288690054449), color = 'white', size = 4) +
                 geom_point(aes(x = 127.08073363302394, y = 37.49098810948115), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.08073363302394, y = 37.49098810948115), color = 'white', size = 4)
        )}
      if(input$location2 == '강남구_바닥신호등' & input$checkbox == FALSE){
        return(ggmap(GNmap) +
                 geom_polygon(data = gangnam_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 3, alpha = 0.5)+
                 geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'blue', size = 8)+
                 geom_point(data = floor_signs_GN, aes(x = lon, y= lat), color = 'white', size = 4) +
                 geom_point(data = dying_case_GN, aes(x = 경도, y = 위도), color = 'red', size = 3)+
                 geom_text(data = floor_signs_GN, aes(x = lon, y =lat), label = floor_signs_GN$idx, size = 4, color = 'black')+
                 geom_jitter()+
                 geom_line(data = grid, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid2, aes(y = hl2, x = vl2, group = index), size = 1.2)+
                 geom_point(aes(x = 127.035357317681, y = 37.515607676250874), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.035357317681, y = 37.515407676250874), color = 'white', size = 4) +
                 geom_point(aes(x = 127.0600895109839, y = 37.514512568419995), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.0600895109839, y = 37.514512568419995), color = 'white', size = 4) +
                 geom_point(aes(x = 127.04272134041682, y = 37.50266294016471), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.04272134041682, y = 37.50266294016471), color = 'white', size = 4) +
                 geom_point(aes(x = 127.05863693253951, y = 37.50305791512272), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.05863693253951, y = 37.50305791512272), color = 'white', size = 4) +
                 geom_point(aes(x = 127.03923330526129, y = 37.49581649679672), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.03923330526129, y = 37.49581649679672), color = 'white', size = 4) +
                 geom_point(aes(x = 127.05973612711864, y = 37.49288690054449), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.05973612711864, y = 37.49288690054449), color = 'white', size = 4) +
                 geom_point(aes(x = 127.08073363302394, y = 37.49098810948115), color = 'goldenrod1', size = 8) +
                 geom_point(aes(x = 127.08073363302394, y = 37.49098810948115), color = 'white', size = 4)
        )}
      if (input$location2 == '영등포구_바닥신호등' & input$checkbox){
        return(ggmap(YDPmap) + 
                 geom_polygon(data = ydp_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 2, alpha = 0.5)+
                 geom_point(data = floor_signs_YDP,
                            aes(x = lon,y = lat), color = 'blue', size = 8)+
                 geom_point(data = cross_YDP2, aes(x = 경도, y = 위도), color = 'darkgreen', size = 4) +
                 geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 6) +
                 geom_line(data = grid4, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid5, aes(y = hl2, x = vl2, group = index), size = 1.2) +
                 geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 5, color = 'black')+
                 geom_point(aes(x = 126.89558861109023, y = 37.52137811309407), color = 'goldenrod1', size = 9) +
                 geom_point(aes(x = 126.89558861109023, y = 37.52137811309407), color = 'white', size = 6) +
                 geom_point(aes(x = 126.90402503764408, y = 37.528845142265446), color = 'goldenrod1', size = 9) +
                 geom_point(aes(x = 126.90402503764408, y = 37.528845142265446), color = 'white', size = 6) +
                 geom_point(aes(x = 126.92442615007039, y = 37.52842208139561), color = 'goldenrod1', size = 9) +
                 geom_point(aes(x = 126.92442615007039, y = 37.52842208139561), color = 'white', size = 6) +
                 geom_point(aes(x = 126.90894906626924, y = 37.512991243797764), color = 'goldenrod1', size = 9) +
                 geom_point(aes(x = 126.90894906626924, y = 37.512991243797764), color = 'white', size = 6) +
                 geom_point(aes(x = 126.89706247494674, y = 37.492991688400686), color = 'goldenrod1', size = 9) +
                 geom_point(aes(x = 126.89706247494674, y = 37.492991688400686), color = 'white', size = 6)
        )}
      if (input$location2 == '영등포구_바닥신호등' & input$checkbox == FALSE){
        return(ggmap(YDPmap) + 
                 geom_polygon(data = ydp_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 2, alpha = 0.5)+
                 geom_point(data = floor_signs_YDP,
                            aes(x = lon,y = lat), color = 'blue', size = 8)+
                 geom_point(data = floor_signs_YDP, aes(x = lon, y = lat), color = 'white', size = 6) +
                 geom_line(data = grid4, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid5, aes(y = hl2, x = vl2, group = index), size = 1.2) +
                 geom_text(data = floor_signs_YDP, aes(x = lon, y =lat), label = floor_signs_YDP$idx, size = 5, color = 'black')+
                 geom_point(aes(x = 126.89558861109023, y = 37.52137811309407), color = 'goldenrod1', size = 9) +
                 geom_point(aes(x = 126.89558861109023, y = 37.52137811309407), color = 'white', size = 6) +
                 geom_point(aes(x = 126.90402503764408, y = 37.528845142265446), color = 'goldenrod1', size = 9) +
                 geom_point(aes(x = 126.90402503764408, y = 37.528845142265446), color = 'white', size = 6) +
                 geom_point(aes(x = 126.92442615007039, y = 37.52842208139561), color = 'goldenrod1', size = 9) +
                 geom_point(aes(x = 126.92442615007039, y = 37.52842208139561), color = 'white', size = 6) +
                 geom_point(aes(x = 126.90894906626924, y = 37.512991243797764), color = 'goldenrod1', size = 9) +
                 geom_point(aes(x = 126.90894906626924, y = 37.512991243797764), color = 'white', size = 6) +
                 geom_point(aes(x = 126.89706247494674, y = 37.492991688400686), color = 'goldenrod1', size = 9) +
                 geom_point(aes(x = 126.89706247494674, y = 37.492991688400686), color = 'white', size = 6)
        )}
      if (input$location2 == '강남구_옐로카펫' & input$checkbox){
        return(ggmap(GNmap) +
                 geom_polygon(data = gangnam_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 3, alpha = 0.5)+
                 geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 11) +
                 geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'salmon', size = 8) +
                 geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'white', size = 6) +
                 geom_text(data = ele_location_GN, aes(x = lon, y =lat), label = ele_location_GN$index, size = 5, color = 'black')+
                 geom_line(data = grid, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid2, aes(y = hl2, x = vl2, group = index), size = 1.2)+
                 geom_point(aes(x = 127.0311435404175, y = 37.53181541805076), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.0311435404175, y = 37.53181541805076), color = 'white', size = 9) +
                 geom_point(aes(x = 127.04518435576267, y = 37.52834560769805), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.04518435576267, y = 37.52834560769805), color = 'white', size = 9) +
                 geom_point(aes(x = 127.04506052692615, y = 37.519882922177885), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.04506052692615, y = 37.519882922177885), color = 'white', size = 9) +
                 geom_point(aes(x = 127.03985873960927, y = 37.51210844672435), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.03985873960927, y = 37.51210844672435), color = 'white', size = 9) +
                 geom_point(aes(x = 127.06099209808988, y = 37.520255631003394), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.06099209808988, y = 37.520255631003394), color = 'white', size = 9) +
                 geom_point(aes(x = 127.06333595636745, y = 37.50334436928764), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.06333595636745, y = 37.50334436928764), color = 'white', size = 9) +
                 geom_point(aes(x = 127.04903976925294, y = 37.50130610045472), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.04903976925294, y = 37.50130610045472), color = 'white', size = 9) +
                 geom_point(aes(x = 127.0327933134344, y = 37.492879843882854), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.0327933134344, y = 37.492879843882854), color = 'white', size = 9) +
                 geom_point(aes(x = 127.0760192517693, y = 37.48991673550949), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.0760192517693, y = 37.48991673550949), color = 'white', size = 9) +
                 geom_point(aes(x = 127.06932385390749, y = 37.49043709826502), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.06932385390749, y = 37.49043709826502), color = 'white', size = 9)
        )}
      if (input$location2 == '강남구_옐로카펫' & input$checkbox){
        return(ggmap(GNmap) +
                 geom_polygon(data = gangnam_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 3, alpha = 0.5)+
                 geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 11) +
                 geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'salmon', size = 8) +
                 geom_point(data = ele_location_GN, aes(x = lon, y = lat), color = 'white', size = 6) +
                 geom_text(data = ele_location_GN, aes(x = lon, y =lat), label = ele_location_GN$index, size = 5, color = 'black')+
                 geom_line(data = grid, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid2, aes(y = hl2, x = vl2, group = index), size = 1.2)+
                 geom_point(aes(x = 127.0311435404175, y = 37.53181541805076), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.0311435404175, y = 37.53181541805076), color = 'white', size = 9) +
                 geom_point(aes(x = 127.04518435576267, y = 37.52834560769805), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.04518435576267, y = 37.52834560769805), color = 'white', size = 9) +
                 geom_point(aes(x = 127.04506052692615, y = 37.519882922177885), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.04506052692615, y = 37.519882922177885), color = 'white', size = 9) +
                 geom_point(aes(x = 127.03985873960927, y = 37.51210844672435), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.03985873960927, y = 37.51210844672435), color = 'white', size = 9) +
                 geom_point(aes(x = 127.06099209808988, y = 37.520255631003394), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.06099209808988, y = 37.520255631003394), color = 'white', size = 9) +
                 geom_point(aes(x = 127.06333595636745, y = 37.50334436928764), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.06333595636745, y = 37.50334436928764), color = 'white', size = 9) +
                 geom_point(aes(x = 127.04903976925294, y = 37.50130610045472), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.04903976925294, y = 37.50130610045472), color = 'white', size = 9) +
                 geom_point(aes(x = 127.0327933134344, y = 37.492879843882854), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.0327933134344, y = 37.492879843882854), color = 'white', size = 9) +
                 geom_point(aes(x = 127.0760192517693, y = 37.48991673550949), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.0760192517693, y = 37.48991673550949), color = 'white', size = 9) +
                 geom_point(aes(x = 127.06932385390749, y = 37.49043709826502), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.06932385390749, y = 37.49043709826502), color = 'white', size = 9)
        )}
      if (input$location2 == '강남구_옐로카펫' & input$checkbox == FALSE){
        return(ggmap(GNmap) +
                 geom_polygon(data = gangnam_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 3, alpha = 0.5)+
                 geom_point(data = yellow_carpets_GN, aes(x = lon, y = lat), color = 'darkblue', size = 11) +
                 geom_line(data = grid, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid2, aes(y = hl2, x = vl2, group = index), size = 1.2)+
                 geom_point(aes(x = 127.0311435404175, y = 37.53181541805076), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.0311435404175, y = 37.53181541805076), color = 'white', size = 9) +
                 geom_point(aes(x = 127.04518435576267, y = 37.52834560769805), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.04518435576267, y = 37.52834560769805), color = 'white', size = 9) +
                 geom_point(aes(x = 127.04506052692615, y = 37.519882922177885), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.04506052692615, y = 37.519882922177885), color = 'white', size = 9) +
                 geom_point(aes(x = 127.03985873960927, y = 37.51210844672435), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.03985873960927, y = 37.51210844672435), color = 'white', size = 9) +
                 geom_point(aes(x = 127.06099209808988, y = 37.520255631003394), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.06099209808988, y = 37.520255631003394), color = 'white', size = 9) +
                 geom_point(aes(x = 127.06333595636745, y = 37.50334436928764), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.06333595636745, y = 37.50334436928764), color = 'white', size = 9) +
                 geom_point(aes(x = 127.04903976925294, y = 37.50130610045472), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.04903976925294, y = 37.50130610045472), color = 'white', size = 9) +
                 geom_point(aes(x = 127.0327933134344, y = 37.492879843882854), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.0327933134344, y = 37.492879843882854), color = 'white', size = 9) +
                 geom_point(aes(x = 127.0760192517693, y = 37.48991673550949), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.0760192517693, y = 37.48991673550949), color = 'white', size = 9) +
                 geom_point(aes(x = 127.06932385390749, y = 37.49043709826502), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 127.06932385390749, y = 37.49043709826502), color = 'white', size = 9)
        )}
      if (input$location2 == '영등포구_옐로카펫' & input$checkbox){
        return(ggmap(YDPmap) +
                 geom_polygon(data = ydp_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 2, alpha = 0.5)+
                 geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size =11) +
                 geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'salmon', size = 8) +
                 geom_point(data = ele_location_YDP, aes(x = lon, y = lat), color = 'white', size = 6) +
                 geom_text(data = ele_location_YDP, aes(x = lon, y =lat), label = ele_location_YDP$index, size = 5, color = 'black') +
                 geom_line(data = grid4, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid5, aes(y = hl2, x = vl2, group = index), size = 1.2)+
                 geom_point(aes(x = 126.8961719041273, y = 37.53943309077306), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.8961719041273, y = 37.53943309077306), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.89265722692637, y = 37.53273800018154), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.89265722692637, y = 37.53273800018154), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.90721088241837, y = 37.52681354423924), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.90721088241837, y = 37.52681354423924), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.90380891158105, y = 37.52351270687672), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.90380891158105, y = 37.52351270687672), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.89325689808979, y = 37.51876260119002), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.89325689808979, y = 37.51876260119002), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.89837094711648, y = 37.51306328597632), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.89837094711648, y = 37.51306328597632), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.92271699623565, y = 37.51987334120885), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.92271699623565, y = 37.51987334120885), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.9181006827444, y = 37.510003445445406), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.9181006827444, y = 37.510003445445406), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.9133792827441, y = 37.49823775501695), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.9133792827441, y = 37.49823775501695), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.90595498459844, y = 37.502574874743054), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.90595498459844, y = 37.502574874743054), color = 'white', size = 9)
        )}
      if (input$location2 == '영등포구_옐로카펫' & input$checkbox == FALSE){
        return(ggmap(YDPmap) +
                 geom_polygon(data = ydp_map, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = 2, alpha = 0.5)+
                 geom_point(data = yellow_carpets_YDP, aes(x = lon, y = lat), color = 'darkblue', size =11) +
                 geom_line(data = grid4, aes(y = hl, x = vl, group = index), size = 1.2) +
                 geom_line(data = grid5, aes(y = hl2, x = vl2, group = index), size = 1.2)+
                 geom_point(aes(x = 126.8961719041273, y = 37.53943309077306), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.8961719041273, y = 37.53943309077306), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.89265722692637, y = 37.53273800018154), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.89265722692637, y = 37.53273800018154), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.90721088241837, y = 37.52681354423924), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.90721088241837, y = 37.52681354423924), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.90380891158105, y = 37.52351270687672), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.90380891158105, y = 37.52351270687672), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.89325689808979, y = 37.51876260119002), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.89325689808979, y = 37.51876260119002), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.89837094711648, y = 37.51306328597632), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.89837094711648, y = 37.51306328597632), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.92271699623565, y = 37.51987334120885), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.92271699623565, y = 37.51987334120885), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.9181006827444, y = 37.510003445445406), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.9181006827444, y = 37.510003445445406), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.9133792827441, y = 37.49823775501695), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.9133792827441, y = 37.49823775501695), color = 'white', size = 9) +
                 
                 geom_point(aes(x = 126.90595498459844, y = 37.502574874743054), color = 'seagreen1', size = 15) +
                 geom_point(aes(x = 126.90595498459844, y = 37.502574874743054), color = 'white', size = 9)
        )}
  })
  output$map <- renderPlot({
    location_input()
  })

}

# Run the app ---

shinyApp(ui = ui, server = server)
