# This script uses only one field of revenue, and the type
#(economic or financial) is indicated by an independent field

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


# Plot using GGPLOT2
gg_fig <- ggplot(df, aes(x = date)) +
  
  # Area plot with point markers
  geom_area(data = df, aes(y = revenue, color = type, fill = type), alpha = 0.6, linewidth = 0.7, position = 'identity') +
  geom_point(data = df, aes(y = revenue, shape = type, color = type), alpha = 0.7, size = 2, position = 'identity') +
  
  # Colors
  scale_color_manual(values = c("financial" = "#e6714b", "economic" = "#666d74"), guide = "none") +
  scale_fill_manual(name = "revenue", values = c("financial" = "dodgerblue1", "economic" = "darkslategrey"), labels = c("financial" = "financial", "economic" = "economic")) +
  scale_shape_manual(values=c("financial" = 18, "economic" = 20), labels = c("financial" = "financial", "economic" = "economic"), guide = "none") +
  
  # Theme
  theme_minimal() +
  
  ## Visual and text elements
  theme(axis.text.x = element_text(angle=60, hjust=1),
        text = element_text(size=9, family="72-Web", color = 'gray30'),
        legend.title = element_text(color="gray30", size=9),
        legend.position = "bottom",
        
        ### Grids and backgrounds
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)) +
  
  ### Axis
  scale_y_continuous(name = "", labels = label_number(suffix = ' m', scale = 1e-6)) +
  scale_x_date(name = "", date_breaks = "1 month", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.01)))

gg_fig
pp_fig <- ggplotly(a)
pp_fig

# Plot using plotly
fig <- plot_ly() %>%
  add_trace(df, x=df$date, y=df$revenue, color = df$type,
            type = 'scatter', mode = 'lines+markers',
            fill = 'tozeroy', fillcolor = df$type,
            colors = c('dodgerblue1', 'darkslategray'),
            symbol = df$type, symbols = c('circle', 'diamond'),
            hoveron = 'points', hoverinfo = 'text',
            text = ~paste('Date: ', df$date, '\nRevenue: ', df$lab, '\nType: ', df$type)) %>%
  layout(
    xaxis = list(
      type = 'date', tickformat = "%B <br>%Y"
    ))

fig


