# Load packages
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)

# Read file
setwd("C:/Users/Andre.Canal/OneDrive - MEGAWORK/personal-mega/03_code/01_Planejamento_Comercial")
mPlanejamento <- data.frame(read.csv("revenue_with_type.csv", header = TRUE, sep = ';'))
head(mPlanejamento)
colnames(mPlanejamento)

# Choose fields to include
list_of_fields <- c(mPlanejamento[1]		# Budget
                    , mPlanejamento[2]    	# Sales order
                    , mPlanejamento[3]    	# Date
                    , mPlanejamento[4]    	# Flow type
                    , mPlanejamento[5]      # revenue
                    , mPlanejamento[6]      # revenue_norm
)

# Create data frame
df <- data.frame(list_of_fields)
df$date <- as.Date(df$date, "%d/%m/%Y")
df$revenue_norm <- as.numeric(df$revenue_norm)


df <- df %>%
  group_by(date, type) %>%
  summarize(revenue = sum(revenue_norm)) %>%
  mutate(lab = scales::label_number_si(accuracy = 0.1)(revenue))


# Plot
fig <- plot_ly() %>%
  add_trace(df, x=df$date, y=df$revenue, color = df$type,
            type = 'scatter', mode = 'lines+markers',
            fill = 'tozeroy', fillcolor = df$type,
            colors = c('dodgerblue1', 'darkslategray'),
            symbol = df$type, symbols = c('circle', 'diamond'),
            hoveron = 'points', hoverinfo = 'text',
            text = ~paste('Data: ', df$date, '\nReceita: ', df$lab, '\nType: ', df$type)) %>%
  layout(
    xaxis = list(
      type = 'date', tickformat = "%B <br>%Y"
    ))

fig
