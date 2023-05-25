setwd('C:\\Users\\lizhvania\\Working\\Patriarchate\\R')

# Libraries 
# საჭირო ბიბლიოთეკები/პაკეტები
library(tidyverse)
library(ggalluvial)
library(networkD3)
library(htmlwidgets)
library(htmltools)


# Import data
# მონაცემების შემოტანა
# To create the sankey networks the data needs to have variables: 'source', target', 'value', which is possible to assign in R, though the data used here already have the necessary structure to be used directly.


# Dataset 1: Patriarchate lands by regions | საპატრიარქოს მიწები რეგიონების მიხედვით
patr_lands_regions <- read_csv('Data\\patr_lands_by_regions.csv')

# Dataset 2: Patriarchate lands by types of land transfers | საპატრიარქოს საკუთრება მიწის გადაცემის ფორმების მიხედვით
patr_lands_transfer_types <- read_csv('Data\\patr_lands_transfer_types.csv')

# Dataset 3: Patriarchate lands by the chronology of land transfers | საპატრიარქოს საკუთრება მიწის გადაცემის ფორმების ქრონოლოგიის მიხედვით
patr_lands_transfers_chron <- read_csv('Data\\patr_lands_transfer_chronology.csv')



# CREATE SANKEY NETWORKS | "ნეთვორქ" დიაგრამების შექმნა 

# 1. Patriarchate lands by regions | საპატრიარქოს მიწები რეგიონების მიხედვით

# Create a node data frame: it lists every entities involved in the flow
nodes_1 <- data.frame(name=c(as.character(patr_lands_regions$source), as.character(patr_lands_regions$target)) %>% unique())


# With networkD3, connection must be provided using id, not using real name like in the links dataframe. So we need to reformat it.
patr_lands_regions$IDsource=match(patr_lands_regions$source, nodes_1$name)-1 
patr_lands_regions$IDtarget=match(patr_lands_regions$target, nodes_1$name)-1

# Assign groups to the nodes
nodes_1$group <- as.factor(c('region', 'region', 'region', 'region', 'region', 'region', 'region', 'region', 'region', 'region', 'region', 
                           'type_a', 'type_b', 'type_c'))

# Give a color for each group of land type:
my_colors_1 <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "type_c", "region", "type_a", "type_b", "type_c"]) .range(["#C6CDF7", "#D8B70A", "#74A089", "#899DA4", "#D3DDDC", "#D3DDDC", "#D3DDDC"])'


sankey_1 <- sankeyNetwork(Links = patr_lands_regions, 
                        Nodes = nodes_1,
                        Source = "IDsource", 
                        Target = "IDtarget",
                        Value = "value", 
                        NodeID = "name", 
                        units = "ჰა",
                        colourScale=my_colors_1, 
                        fontSize=14, 
                        fontFamily = "FiraGo SemiBold", 
                        nodeWidth=50, 
                        nodePadding=15, 
                        
                        margin = 225,  
                        
                        height=600, 
                        width=1500, 
                        iterations = 0, 
                        sinksRight=FALSE, 
                        
                        LinkGroup = 'target', 
                        NodeGroup="group"
)

sankey_1 <- htmlwidgets::prependContent(sankey_1, htmltools::tags$h1(style = "text-align: center; font-family: FiraGo ExtraBold; color:#737373; 
                                                                 font-size: 120%; padding: 0px;", 
                                                                 "საპატრიარქოს საკუთრება მიწის დანიშნულების მიხედვით"))


sankey_rendered_1 <- htmlwidgets::onRender(sankey_1, 
                                         'function(el, x) {
                                         
                                         // round the values to two decimal places for links
                                         var link = d3.selectAll(".link");
                                         var format = d3.formatLocale({"decimal": ".", "thousands": ",", "grouping": [3], "currency": ["", "\u00a0€"]}).format(",.2f");
                                      
                                         link.select("title").select("body")
                                            .html(function(d) { return "<pre>" + d.source.name + " \u2192 " + d.target.name +
                                                  "\\n" + format(d.value) + " ჰა" + "<pre>"; });
                                                  
                                         
                                         // round the values to two decimal places for nodes
                                         var node = d3.selectAll(".node")
                                         var format = d3.formatLocale({"decimal": ".", "thousands": ",", "grouping": [3], "currency": ["", "\u00a0€"]}).format(",.2f");
                                         
                                         node.select("title").select("body")
                                            .html(function(d) { return "<pre>" + d.name + " \u2192 " + 
                                                  "\\n" + format(d.value) + " ჰა" + "<pre>"; });
                                         
                                         
                                         
                                         d3.selectAll(".node text")
                                           .filter(function(d, i) { return i < 11})   //to select only regions (the first 11 elements from nodes)
                                           
                                           .attr("x", -8)
                                           .attr("text-anchor", "end")
                                           .style("font-family", "FiraGo Bold")
                                           .style("fill",  "#525252");
                                         
                                         d3.selectAll(".node text")
                                           .filter(function(d, i) { return i > 10})   //to select only types (the last 3 elements from nodes)
                                           
                                           .style("font-family", "FiraGo ExtraBold")
                                           .style("fill",  "#969696");
                                         
                                         // to remove semicolon (:) from label of nodes
                                         d3.selectAll(".node text")
                                           .text(function(d, i) { return d.name.slice(0, -1)});
                                         
                                         }')


sankey_rendered_1

# Save the html file
saveNetwork(sankey_rendered_1, "1_Patriarchate lands by regions.html")



# 2. Patriarchate lands by types of land transfers | საპატრიარქოს საკუთრება მიწის გადაცემის ფორმების მიხედვით

# Create a node data frame: it lists every entities involved in the flow
nodes_2 <- data.frame(name=c(as.character(patr_lands_transfer_types$source), as.character(patr_lands_transfer_types$target)) %>% unique())


# With networkD3, connection must be provided using id, not using real name like in the links dataframe. So we need to reformat it.
patr_lands_transfer_types$IDsource=match(patr_lands_transfer_types$source, nodes_2$name)-1 
patr_lands_transfer_types$IDtarget=match(patr_lands_transfer_types$target, nodes_2$name)-1


# set colors
ColourScal ='d3.scaleOrdinal() .range([
"#FAEFD1", 
"#FAEFD1", 
"#FAEFD1", 
"#FAEFD1",
"#FAEFD1", 
"#FAEFD1", 
"#FAEFD1", 
"#FAEFD1", 
"#FAEFD1", 
"#FAEFD1",
"#FAEFD1",


"#67000d",    // უსასყიდლოდ გადაცემა
"#a50f15",    // დროებით სარგებლობაში  
"#cb181d",    // კონსტიტუციური შეთანხმებით
"#ef3b2c",    // შეწირულება
"#fb6a4a",   // სიმბოლურ ფასად შეძენა
"#fc9272",   // კერძო პირისგან შესყიდვა
"#fcbba1",   // უცნობი სტატუსი
"#fee0d2",   // გაცვლით მიღებული
"#fff5f0"   // საბაზრო ფასად შეძენა
])'



sankey_2 <- sankeyNetwork(Links = patr_lands_transfer_types, 
                        Nodes = nodes_2,
                        Source = "IDsource", 
                        Target = "IDtarget",
                        Value = "value", 
                        NodeID = "name", 
                        units = "ჰა", 
                        colourScale=ColourScal,
                        fontSize=14, 
                        fontFamily = "FiraGo SemiBold", 
                        nodeWidth=50, 
                        nodePadding=15, 
                        
                        margin = 260,  
                        
                        height=600, 
                        width=1500, 
                        iterations = 0, 
                        sinksRight=FALSE
)

sankey_2 <- htmlwidgets::prependContent(sankey_2, htmltools::tags$h1(style = "text-align: center; font-family: FiraGo ExtraBold; color:#737373; 
                                                                 font-size: 120%; padding: 0px;", 
                                                                 "საპატრიარქოს საკუთრება მიწის გადაცემის ფორმების მიხედვით"))


sankey_rendered_2 <- htmlwidgets::onRender(sankey_2, 
                                         'function(el, x) {
                                         
                                         // round the values to two decimal places for links
                                         var link = d3.selectAll(".link");
                                         var format = d3.formatLocale({"decimal": ".", "thousands": ",", "grouping": [3], "currency": ["", "\u00a0€"]}).format(",.2f");
                                      
                                         link.select("title").select("body")
                                            .html(function(d) { return "<pre>" + d.source.name + " \u2192 " + d.target.name +
                                                  "\\n" + format(d.value) + " ჰა" + "<pre>"; });
                                                  
                                         
                                         // round the values to two decimal places for nodes
                                         var node = d3.selectAll(".node")
                                         var format = d3.formatLocale({"decimal": ".", "thousands": ",", "grouping": [3], "currency": ["", "\u00a0€"]}).format(",.2f");
                                         
                                         node.select("title").select("body")
                                            .html(function(d) { return "<pre>" + d.name + " \u2192 " + 
                                                  "\\n" + format(d.value) + " ჰა" + "<pre>"; });
                                         
                                         
                                         
                                         d3.selectAll(".node text")
                                           .filter(function(d, i) { return i < 11})   //to select only regions (the first 11 elements from nodes)
                                           
                                           .attr("x", -8)
                                           .attr("text-anchor", "end")
                                           .style("font-family", "FiraGo Bold")
                                           .style("fill",  "#525252");
                                         
                                         d3.selectAll(".node text")
                                           .filter(function(d, i) { return i > 10})   //to select only types (the last 3 elements from nodes)
                                           
                                           .style("font-family", "FiraGo ExtraBold")
                                           .style("fill",  "#969696");
                                         
                                         // to remove semicolon (:) from label of nodes
                                         d3.selectAll(".node text")
                                           .text(function(d, i) { return d.name.slice(0, -1)});
                                         
                                         }')


sankey_rendered_2

# Save the html file
saveNetwork(sankey_rendered_2, "2_Patriarchate lands by types of land transfers.html")



# 3. Patriarchate lands by the chronology of land transfers | საპატრიარქოს საკუთრება მიწის გადაცემის ფორმების ქრონოლოგიის მიხედვი

# Create a node data frame: it lists every entities involved in the flow
nodes_3 <- data.frame(name=c(as.character(patr_lands_transfers_chron$source), as.character(patr_lands_transfers_chron$target)) %>% unique())


# With networkD3, connection must be provided using id, not using real name like in the links dataframe. So we need to reformat it.
patr_lands_transfers_chron$IDsource=match(patr_lands_transfers_chron$source, nodes_3$name)-1 
patr_lands_transfers_chron$IDtarget=match(patr_lands_transfers_chron$target, nodes_3$name)-1


# set colors
ColourScal ='d3.scaleOrdinal() .range([
"#046C9A",    
"#046C9A", 
"#046C9A", 
"#046C9A",
"#046C9A", 
"#046C9A", 
"#046C9A", 
"#046C9A", 
"#046C9A",


"#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262",
"#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262", "#FDD262",
"#FDD262", "#FDD262"
])'


sankey_3 <- sankeyNetwork(Links = patr_lands_transfers_chron, 
                        Nodes = nodes_3,
                        Source = "IDsource", 
                        Target = "IDtarget",
                        Value = "value", 
                        NodeID = "name", 
                        units = "ჰა", 
                        colourScale=ColourScal, 
                        fontSize=14, 
                        fontFamily = "FiraGo SemiBold", 
                        nodeWidth=50, 
                        nodePadding=15, 
                        
                        margin = 65,  
                        
                        height=600, 
                        width=1500, 
                        iterations = 0, 
                        sinksRight=FALSE
)

sankey_3 <- htmlwidgets::prependContent(sankey_3, htmltools::tags$h1(style = "text-align: center; font-family: FiraGo ExtraBold; color:#737373; 
                                                                 font-size: 120%; padding: 0px;", 
                                                                 "საპატრიარქოს საკუთრება - მიწის გადაცემის ქრონოლოგია"))



sankey_rendered_3 <- htmlwidgets::onRender(sankey_3,  
                                         'function(el, x) {
                                         
                                         // round the values to two decimal places for links
                                         var link = d3.selectAll(".link");
                                         var format = d3.formatLocale({"decimal": ".", "thousands": ",", "grouping": [3], "currency": ["", "\u00a0€"]}).format(",.2f");
                                      
                                         link.select("title").select("body")
                                            .html(function(d) { return "<pre>" + d.source.name + " \u2192 " + d.target.name +
                                                  "\\n" + format(d.value) + " ჰა" + "<pre>"; });
                                                  
                                         
                                         // round the values to two decimal places for nodes
                                         var node = d3.selectAll(".node")
                                         var format = d3.formatLocale({"decimal": ".", "thousands": ",", "grouping": [3], "currency": ["", "\u00a0€"]}).format(",.2f");
                                         
                                         node.select("title").select("body")
                                            .html(function(d) { return "<pre>" + d.name + " \u2192 " + 
                                                  "\\n" + format(d.value) + " ჰა" + "<pre>"; });
                                         
                                         
                                         
                                         d3.selectAll(".node text")
                                           .filter(function(d, i) { return i < 9})   //to select only regions (the first 11 elements from nodes)
                                           
                                           .attr("x", -8)
                                           .attr("text-anchor", "end")
                                           .style("font-family", "FiraGo Bold")
                                           .style("fill",  "#525252");
                                         
                                         d3.selectAll(".node text")
                                           .filter(function(d, i) { return i > 8})   //to select only types (the last 3 elements from nodes)
                                           
                                           .style("font-family", "FiraGo ExtraBold")
                                           .style("fill",  "#969696");
                                         
                                         // to remove semicolon (:) from label of nodes
                                         d3.selectAll(".node text")
                                           .text(function(d, i) { return d.name.slice(0, -1)});
                                           
                                         
                                         }')


sankey_rendered_3

# Save the html file
saveNetwork(sankey_rendered_3, "3_Patriarchate lands by the chronology of land transfers.html")
