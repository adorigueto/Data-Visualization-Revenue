# This script uses two different revenue fields, one for
#financial revenue and the other to economic revenue

# Load packages
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)

# Read file
setwd("C:/Users/Andre.Canal/OneDrive - MEGAWORK/personal-mega/03_code/01")
mPlanejamento <- data.frame(read.csv("revenue.csv", header = TRUE, sep = ';'))
head(mPlanejamento)
colnames(mPlanejamento)

# Choose fields to include
list_of_fields <- c(mPlanejamento[1]       # Orçamento
                    #, mPlanejamento[2]    # Canal de Distribuição
                    #, mPlanejamento[3]    # Cliente
                    #, mPlanejamento[4]    # Tipo de Venda
                    #, mPlanejamento[5]    # Pedido
                    #, mPlanejamento[6]    # Material
                    , mPlanejamento[7]     # Data Embarque
                    #, mPlanejamento[8]    # Data Expedição
                    #, mPlanejamento[9]    # Data Antecipação
                    #, mPlanejamento[10]   # Data Pgto. Saldo
                    #, mPlanejamento[11]   # Moeda
                    #, mPlanejamento[12]   # quantidade
                    #, mPlanejamento[13]   # preco_unit
                    #, mPlanejamento[14]   # preco_unit_sa
                    , mPlanejamento[15]    # receita
                    #, mPlanejamento[16]   # perctr
                    #, mPlanejamento[17]   # libor
)


# Create data frame
df <- data.frame(list_of_fields)
df["receita2"] <- receita/2 + receita * 0.2 + 1000
df
# Transform as Date
df$Data.Embarque <- as.Date(str_c(substring(df$Data.Embarque, 1, 1),
                                  substr(df$Data.Embarque, 4, 6),
                                  substr(df$Data.Embarque, 9, 12)),
                            "%d%b%Y")

df <- df %>%  
  # Group by
  group_by(#Orçamento
           #, Canal.de.Distribuição
           #, Cliente
           #, Tipo.de.Venda
           #, Pedido
           #, Material
            Data.Embarque
           #, Data.Expedição
           #, Data.Antecipação
           #, Data.Pgto..Saldo
           #, Moeda
           #, quantidade
           #, preco_unit
           #, preco_unit_sa
           #, receita
           #, perctr
           #, libor
           ) %>% 


  
  
  # Summarize
  summarize(receita = sum(receita), receita2 = sum(receita2))

# Order by
df <- df[order(df$Data.Embarque),]

# Plot simple
plot(df$receita2~df$Data.Embarque, type='l')

# Plot with ggplot2
ggplot() +
  
  # Area plot
  geom_area(data = df, aes(x=Data.Embarque, y=receita), alpha = 0.4, color = "", size = 1, fill = 'orange') +
  geom_area(data = df, aes(x=Data.Embarque, y=receita2), alpha = 1, color = "darkgreen", size = 1, fill = 'grey') +
  # Point markers
  geom_point(data = df, aes(x=Data.Embarque, y=receita2), color = "darkgreen", size = 3) +
  geom_point(data = df, aes(x=Data.Embarque, y=receita), color = "darkorange", size = 3) +
  
  # Minimalist theme
  theme_minimal() +
  
  # Scale and adjust y and x axis
  theme(axis.text.x=element_text(angle=20, hjust=1)) +
  scale_y_continuous(name = "Receita", labels = label_number(suffix = ' m', scale = 1e-6)) +
  scale_x_date(name = "Data de Embarque", date_breaks = "2 month", date_labels = "%b %Y")
  