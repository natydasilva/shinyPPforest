
library(ggplot2)
library(shiny)
library(plyr)
library(plotly)
library(stringr)
library(PPtreeViz)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(randomForest)
library(pROC)
library(ggmosaic)
source("shinyplots.R")

data.sources = list.files(pattern="*.RData")
for(i in 1:length(data.sources)){
  load(data.sources[i])
}


runAppforest <- function(ppf, imp){

#################################
#             Data              #
#################################

#random forest
rf <- randomForest(Type ~ ., data = ppf$train, ntree = ppf$n.tree, importance = TRUE,
                   proximity = TRUE)
n.class <- ppf$train %>% select_(ppf$class.var) %>% unique() %>% nrow()
 
lev <- ppf$train %>% select_(ppf$class.var) %>%   sapply(levels) %>% as.factor() 


k <- 2
id <- diag(dim(ppf$train)[1])
id[lower.tri(id, diag = TRUE)] <- 1 - ppf[[9]]$proxi
rf.mds <- stats::cmdscale(as.dist(id), eig = TRUE, k = k)
colnames(rf.mds$points) <- paste("MDS", 1:k, sep = "")
nlevs <- nlevels(ppf$train[, 1])

f.helmert <- function(d)
{
  helmert <- rep(1 / sqrt(d), d)
  for (i in 1:(d - 1))
  {
    x <- rep(1 / sqrt(i * (i + 1)), i)
    x <- c(x,-i / sqrt(i * (i + 1)))
    x <- c(x, rep(0, d - i - 1))
    helmert <- rbind(helmert, x)
  }
  return(helmert)
}

#projected data
projct <- t(f.helmert(length(unique(ppf$train[ , ppf$class.var] )))[-1, ])

bestnode <- plyr::ldply(ppf[[8]][[2]], function(x) {
  bn <- abs(x$projbest.node)
  bn[bn == 0] <- NA
  dat.fr <- data.frame(node = 1:dim(x$projbest.node)[1],bn)
  })

colnames(bestnode)[-1] <- colnames(ppf$train[ , -which(colnames(ppf$train)==ppf$class.var)])
bestnode$node <- as.factor(bestnode$node)

#scale data for the parallel plot tab 1
myscale <- function(x) (x - mean(x)) / sd(x)

scale.dat <- ppf$train %>% mutate_each(funs(myscale),-matches(ppf$class.var)) 
scale.dat.melt <- scale.dat %>%  mutate(ids = 1:nrow(ppf$train)) %>% gather(var,Value,-Type,-ids)
scale.dat.melt$Variables <- as.numeric(as.factor(scale.dat.melt$var))
colnames(scale.dat.melt)[1] <- "Class"

###importance
makePairs <- function(dat){
  aux <-dat[ , -c(1, 2)]
  id <- sample(1:(ncol(aux)), 3)
  d<-aux[ , id]
  
  grid <- expand.grid(x = 1:ncol(d), y = 1:ncol(d))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(Class=dat[,1],ids=dat[,2], x = d[, xcol], y = d[, ycol], pair=paste(grid[i,], collapse = '-') )
     }))
  
  all
}

#ternary
dat3 <- data.frame(Class = ppf$train[, 1], ids = 1:nrow(rf.mds$points),
                   proj.vote = as.matrix(ppf$votes) %*% projct)
##with 3 or less classes
empt <-rep(1:nrow(dat3), 3)
dat3.empt <- dat3[empt,] %>% mutate(rep = rep(1:3,each=nrow(dat3)))
if(n.class>3){
gg1 <-  makePairs(dat3) 
}
###Tab 2 data
##Importance tree

impo.pl <-bestnode %>% 
  mutate(ids = rep(1:ppf$n.tree,each = nrow(ppf[[8]][[2]][[1]]$projbest.node) ) ) %>% 
  gather(var, value, -ids, -node)
impo.pl$Variables <- as.numeric(as.factor(impo.pl$var))
impo.pl$Abs.importance <- round(impo.pl$value,2)


#eror tree
error.tree <- data_frame(ids = 1:ppf$n.tree, trees = "trees", OOB.error.tree = ppf$oob.error.tree)

###Tab 3
#rf
dat.side <- data.frame(ids = 1:nrow(ppf$train), Type = ppf$train[, ppf$class.var], rf$votes, pred = rf$predicted )

dat.side.pl <- dat.side %>% gather(Classvote, Probability, -pred, -ids, -Type)
colnames(dat.side.pl)[2] <- "Class"

#rfpp
dat.sidepp <- data.frame( ids = 1:nrow(ppf$train), Type = ppf$train[,ppf$class.var], ppf$votes, pred = ppf$prediction.oob)
dat.sidepp.pl <- dat.sidepp %>% gather(Classvote,Probability,-pred,-ids,-Type)
colnames(dat.sidepp.pl )[2] <- "Class"

#ROC
  dat.rocpprf <- dat.sidepp.pl %>% group_by(Classvote) %>% 
  mutate(cond = Class %in% Classvote)  %>%
  ungroup() %>%  ddply(.(Classvote), function(d) {
    rocky(d$cond,d$Probability)
  })

dat.rocpprf$Classvote <- as.factor(dat.rocpprf$Classvote)


dat.rocrf <- dat.side.pl %>%group_by(Classvote) %>% 
  mutate(cond = Class %in% Classvote)  %>%
  ungroup() %>%  ddply(.(Classvote),function(d) {
    rocky(d$cond,d$Probability)
  })

dat.rocrf$Classvote <- as.factor(dat.rocrf$Classvote)

#oob error data rf
nsplit1 <- round(nrow(ppf$train)/13)
sel.tr <- round(seq(2, round(rf$ntree),nsplit1))
err <- rf$err.rate
err <- cbind(err, rf$test$err.rate)
colnames(err)[1] <- "All"

dat_rf_aux <-data.frame(err) 
colnames(dat_rf_aux) <- colnames(err)

dat_rf <- dat_rf_aux %>% mutate(trees = 1:rf$ntree) %>% gather(Class,OOB,-trees) %>%
  dplyr::filter(trees %in% sel.tr)

oob.pl <- ppf_oob_error(ppf, nsplit1 = round(nrow(ppf$train)/13) )

myColors <- c("#000000", brewer.pal(length(unique(ppf$train[,ppf$class.var])),"Dark2"))
names(myColors) <- levels(dat_rf$Class)

#imporf
aux <- data.frame(rf$importance)
imp.pl <- data.frame(nm=rownames(rf$importance),imp=aux[,"MeanDecreaseAccuracy"]) %>% arrange(imp)
imp.pl$nm<-  factor(imp.pl$nm, levels= imp.pl[order( imp.pl$imp), "nm"])

#############################
#             UI            #
#############################
ui <-   fluidPage(
  titlePanel('PPforest visualization'),
  # Some custom CSS for a smaller font for preformatted text
  tags$head(tags$style(
    HTML("
         pre, table.table {
         font-size: smaller;
         }
         ")
    )),
  
  tabsetPanel(
    tabPanel(
      "Case level",
      fluidRow(column(
        width = 11,
        plotlyOutput("parallel", height = 400)
      )),
      fluidRow(
        column(
          width = 4, align = "center", offset = 2,
          plotlyOutput("mdsplot", height = 400)
        ), column(
          width = 4,align = "center",
          plotlyOutput("sideplot", height = 400)
        )
        
      ),

    fluidRow(
      column(width = 11,
             plotlyOutput("ternaryplot", height = 400)))
  ),
    
    tabPanel(
      "Individual mode level",
      fluidRow(
        column(width = 7,
               plotlyOutput("importancetree",height = 500)), column(
                 width = 4,
                 plotOutput("plottree",height = 350)
                 , plotlyOutput('boxtreeerror',height = 150)
               )
      ),
      fluidRow(
        if(sum(ppf[[8]][[2]][[1]]$Tree.Struct[,4]!=0) == 1){
        column(width = 4,
                plotlyOutput("plotdensity",height = 400))
        }else if(sum(ppf[[8]][[2]][[1]]$Tree.Struct[,4]!=0) == 2){
          column(width = 7,
                 plotlyOutput("plotdensity",height = 400))
        }else{
          column(width = 11,
                 plotlyOutput("plotdensity",height = 400)) 
        }
  
      ),
      fluidRow(
        if(sum(ppf[[8]][[2]][[1]]$Tree.Struct[,4]!=0) == 1 ){
        column(width = 4,
               plotlyOutput("plotmosaic",height = 400))
        }else if(sum(ppf[[8]][[2]][[1]]$Tree.Struct[,4]!=0) == 2 ){
          column(width = 7,
                 plotlyOutput("plotmosaic",height = 400))
        }else{
          column(width = 11,
                 plotlyOutput("plotmosaic",height = 400))
        }
    
      )
    )
  ,
  tabPanel(
    "Ensemble level I",
    fluidRow(column(width = 12,sidebarPanel(
      selectizeInput(
        'xcol', 'Select Class', as.character(lev) , selected = lev,multiple =
          TRUE
      ),actionButton("goButton", "Go!")
    ))),
    fluidRow(
      column(width = 4,
             plotlyOutput("siderfpp",height = 400)),
      column(width = 4,
             plotlyOutput("plot_rocpp",height = 400)),
      column(width = 4,
             plotlyOutput("plot_oobpp",height = 400))

    ),
    fluidRow(
      column(width = 4,
             plotlyOutput("siderf",height = 400)),
      column(width = 4,
             plotlyOutput("plot_rocrf",height = 400)),
      column(width = 4,
             plotlyOutput("plot_oobrf",height = 400))
    ),


    fluidRow(
      column(width = 4, align = "center", offset = 2,
             plotlyOutput("plot_impopp",height = 400)),
     
      column(width = 4,
             plotlyOutput("plot_imporf",height = 400))

      ),
    fluidRow(column(width = 6,align = "center",
                    tableOutput('tablepp')),
             column(width = 6,
                    tableOutput('tablerf')))
    )
    
  )
    )


#############################
#         SERVER            #
#############################
server <- function(input, output) {
 
rv <- reactiveValues( data = data.frame(
        MDS1 = rf.mds$points[,1], MDS2 = rf.mds$points[,2],
        Class = ppf$train[, 1],ids = 1:nrow(rf.mds$points),
        fill = logical(nrow(ppf$train ) ),proj.vote =
        as.matrix(ppf$votes) %*% projct,
        vote = ppf$votes, pred = ppf$prediction.oob, scale.dat ) )

  updateRV <- function(selected) {
    fill <- logical(length(rv$data$fill))
    fill[selected] <- TRUE
    rv$data$fill <- fill
  }
  
  
  observeEvent(event_data("plotly_selected"), {
    selected <- rv$data$ids %in% event_data("plotly_selected")$key
    updateRV(selected)
  })
  
  
  observeEvent(event_data("plotly_click"), {
    k <- event_data("plotly_click")$key
    if (any(k %in% unique(rv$data$ids))) {
      selected <- rv$data$ids %in% k
    }
    
    updateRV(selected)
  })
  
  

  rv3 <- reactiveValues(bestnode = data.frame(ids = 1:(nrow(bestnode) / sum(length(
      unique( bestnode$node )))), bestnode %>% 
      dplyr::filter(node == 1)), fill = FALSE)
  
  
  updateRV3 <- function(selectedbest) {
    fill <- logical(length(rv3$bestnode$fill))
    fill[selectedbest] <- TRUE
    rv3$bestnode$fill <- fill
  }
  
  observeEvent(event_data("plotly_click", source = "dibu"), {
    k <- event_data("plotly_click", source = "dibu")$key
    if (any(k %in% unique(rv3$bestnode$ids))) {
      selectedbest <- rv3$bestnode$ids %in% k
    }
    updateRV3(selectedbest)
  })
  

############################
#           TAB1           #
############################
#proximity and vote matrix individual level comparison
  
  
##Parallel plot with standarized data
  output$parallel <- renderPlotly({
    yy <- rv$data$ids[rv$data$fill]
    
    p <- ggplot(scale.dat.melt, aes(x = Variables, y = Value, 
                                    group = ids, key = ids, colour = Class, var = var)) +
      geom_line(alpha = 0.3) + scale_x_discrete(limits = levels(as.factor(scale.dat.melt$var)), expand = c(0.01,0.01)) +
      ggtitle("Data parallel plot ") + theme(legend.position = "none", axis.text.x  = element_text(angle = 90, vjust = 0.5), aspect.ratio = 1) + 
      scale_colour_brewer(type = "qual", palette = "Dark2")
    
    if (length(yy) > 0) {
      dat <-   scale.dat.melt %>% dplyr::filter(ids %in% yy)
      p <- ggplot(scale.dat.melt, aes(x = Variables, y = Value, 
                                      group = ids, key = ids, color = Class, var = var)) +
        geom_line(alpha = 0.1) + scale_x_discrete(limits = levels(as.factor(scale.dat.melt$var)), expand = c(0.01,0.01)) + 
        ggtitle("Data parallel plot") + theme(legend.position = "none",axis.text.x  = element_text(angle = 90, vjust = 0.5), aspect.ratio = 1) + 
        scale_colour_brewer(type = "qual",palette = "Dark2")
      
      p <- p + geom_line(data = dat) 
    }
    ggplotly(p,tooltip = c("var","colour","y","key"))
  })
  
  #MDS plot
  output$mdsplot <- renderPlotly({
    yy <- rv$data$ids[rv$data$fill]
    
    p <- ggplot(data = rv$data, aes(x = MDS1, y = MDS2, 
                                    colour = Class, key = ids)) + 
      geom_point(size = I(3),alpha = .5)  + theme(legend.position = "none", legend.text = element_text(angle = 90), legend.key = element_blank(), aspect.ratio =
        1)  + labs(y = "MDS 2", x = "MDS 1", title = "Multidimensional Scaling") +
     scale_colour_brewer(type = "qual",palette = "Dark2")
    
    if (length(yy) > 0) {
      dat <- rv$data %>% dplyr::filter(ids %in% yy)
      p <- ggplot(data = rv$data, aes(x = MDS1, y = MDS2, color = Class, key = ids)) + 
        geom_point( size = I(3), alpha = .1) + theme(legend.position = "none", legend.text = element_text(angle = 90), legend.key = element_blank(), aspect.ratio =1) +
        labs(y = "MDS 2", x = "MDS 1", title = "Multidimensional Scaling")  + 
        scale_colour_brewer(type =   "qual",palette = "Dark2")
    
    p <- p + geom_point(data = dat, size =  I(3))
 
    }
    ggplotly(p,tooltip = c("colour","x","y","key")) %>% layout(dragmode = "select")
    
  })
  
  #Jittered Side-by-side probability plot
  output$sideplot <- renderPlotly({
    
    yy <- rv$data$ids[rv$data$fill]
    
   reddat <- rv$data %>% 
            select(ids, Class, starts_with("vote"), pred) 
      colnames(reddat) <- colnames( reddat) %>% str_replace("vote.","")
   
      sidepl <- reddat %>% gather(classpred, Probability, -pred, -ids, -Class)
   
  
    p <- ggplot(data = sidepl, aes(classpred, Probability, colour = Class, key = ids)) + 
      geom_jitter(height = 0, size = I(3), alpha = .5) +
      theme(legend.position = "none", axis.text.x  = element_text(angle = 45, vjust = 0.5), aspect.ratio = 1) +
      labs(x = "Class", title = "Side by side plot") + scale_colour_brewer(type = "qual", palette = "Dark2")
    
    if (length(yy) > 0) {
      dat <- sidepl %>% dplyr::filter(!ids %in% yy)
      dat_fil <- sidepl %>% dplyr::filter(ids %in% yy)
      p <- ggplot(data = dat, aes(classpred, Probability, colour = Class, key = ids)) + 
        geom_jitter(height = 0, size = I(3), alpha = .1) +
        theme(legend.position = "none",axis.text.x  = element_text(angle = 45, vjust = 0.5) , aspect.ratio = 1) +
        labs(x = "Class", title = "Side by side plot") + scale_colour_brewer(type = "qual", palette = "Dark2")
      
      p <- p + geom_jitter( height = 0, data = dat_fil, size =I(3)) 
    }
    
    ggplotly(p,tooltip = c("colour","y","key")) %>% layout(dragmode = "select")
    
    
  })
  
  
 output$ternaryplot <- renderPlotly({
    yy <- rv$data$ids[rv$data$fill]
  
    if(n.class <= 3){
    p <- ggplot(data = dat3.empt, aes(
        x = proj.vote.x, y = proj.vote.x.1, colour = Class, key = ids
      )) +  geom_blank() + facet_wrap(~rep) + geom_point(data = filter(dat3.empt, rep == 2), size = I(3), alpha = .5) + ylab("") +
      xlab("") + theme(legend.position = "none" , aspect.ratio = 1) + ggtitle("Vote matrix ternary plot") +
      scale_colour_brewer(type = "qual",palette = "Dark2")
    
    if (length(yy) > 0) {
      dat33 <- dat3.empt %>% dplyr::filter(ids %in% yy)
      p <- ggplot(data = dat3.empt, aes( x = proj.vote.x, y = proj.vote.x.1, 
                                       colour = Class,key = ids)) + 
        geom_blank() + facet_wrap(~rep) + geom_point(data = filter(dat3.empt, rep == 2), size = I(3), alpha = .1) + ylab("") + xlab("") + 
           theme(legend.position = "none", aspect.ratio = 1) + ggtitle("Vote matrix ternary plot") +
           scale_colour_brewer(type = "qual",palette = "Dark2")
      
      p <- p + geom_point(data = filter(dat33, rep==2), size = I(3)) 
     }
    }else{
      
      p <- gg1 %>% filter(pair %in% c("2-1","3-1","3-2")) %>% 
        ggplot(aes(x, y, color = Class,key = ids)) + geom_point( size = I(3), alpha = .5) + 
        facet_wrap(~pair) + labs(y = "",  x = "") + theme(legend.position = "none", aspect.ratio = 1) + ggtitle("Vote matrix ternary plot") +
        scale_colour_brewer(type = "qual",palette = "Dark2")
      
      if (length(yy) > 0) {
        dat33 <- dat3 %>% dplyr::filter(ids %in% yy)
        gg2 <-  gg1 %>% dplyr::filter(ids %in% yy)
     
     p <- gg1 %>% filter(pair %in% c("2-1","3-1","3-2")) %>% 
       ggplot(aes(x, y, color = Class,key = ids))+geom_point( size = I(3), alpha = .1) + 
       facet_wrap(~pair) + ylab("") + xlab("") + theme(legend.position = "none", aspect.ratio=1) + ggtitle("Vote matrix ternary plot") +
       scale_colour_brewer(type = "qual",palette = "Dark2")
     
     p <- p + geom_point(data = gg2 %>% filter(pair %in% c("2-1","3-1","3-2")), 
                          size = I(3)) 
       }
    }
    ggplotly(p,tooltip = c("colour","x","y","key")) %>% layout(dragmode = "select")
    
    
    })
  
  
################################
#             Tab 2            # 
################################  
  #Importance
  output$importancetree <- renderPlotly({
    if(length(unique(impo.pl$node))>3){
      impo.pl <- impo.pl %>% filter(node==unique(impo.pl$node)[1:3])
    }
    p <- ggplot(filter(impo.pl,!ids %in% 1), aes( x = Variables, y = Abs.importance, group = ids,
                                                  key = ids, var = var)) +
           geom_jitter(height = 0, size = I(2), alpha = 0.3) + facet_grid(node ~ .) + 
           scale_x_discrete(limits = levels(as.factor(impo.pl$var) ) ) + ggtitle("Importance variable for each tree") +
           theme(legend.position = "none", axis.text.x  = element_text(angle = 90, vjust = 0.5 ) )
    
    p <- p + geom_jitter( data = filter(impo.pl, ids %in% 1), color="red",height = 0) +
                facet_grid(node ~ .) + scale_x_discrete(limits = levels(as.factor(impo.pl$var) ) ) + 
                ggtitle("Importance variable for each tree") +
                theme(legend.position = "none", axis.text.x  = element_text(angle=90, vjust=0.5), aspect.ratio = 1)
    
    yy1 <- as.numeric(rv3$bestnode$ids[rv3$bestnode$fill])
    yy2 <- yy1[!is.na(yy1)]
    
    if (length(yy2) > 0) {
      dat <-   impo.pl %>% dplyr::filter(!ids %in% yy2)
      dat2 <-   impo.pl %>% dplyr::filter(ids %in% yy2)
      p <- ggplot(dat, aes(x = Variables , y = Abs.importance, key = ids,var.=var)) +
              geom_jitter(height = 0, size = I(2),alpha = 0.3) + facet_grid(node ~ .) + 
              scale_x_discrete(limits = levels(as.factor(impo.pl$var) ) ) + ggtitle("Importance variable for each tree") +
              theme(legend.position = "none", axis.text.x  = element_text(angle = 90, vjust = 0.5), aspect.ratio = 1 )
      
      p <-  p  + facet_grid(node ~ .) + geom_jitter(height = 0, data = dat2,  color = "red" )
      
    }
     ggplotly(p,tooltip = c("var","y","key"),source = "dibu") 
  })
  
  #Density
  output$plotdensity <- renderPlotly({
    yy1 <- as.numeric(rv3$bestnode$ids[rv3$bestnode$fill])
    yy2 <- yy1[!is.na(yy1)]
    
    if (length(yy2) > 0) {
  
    PPtree_dens(ppf, yy2) %>% layout(dragmode = "select")
    }else{
     PPtree_dens(ppf, 1) %>% layout(dragmode = "select")
      
    }
    
  })
  
  #Tree structure with PPtreeViz
  output$plottree <- renderPlot({
    yy1 <- as.numeric(rv3$bestnode$ids[rv3$bestnode$fill])
    yy2 <- yy1[!is.na(yy1)]
    
    if (length(yy2) > 0) {
      plot(ppf[[8]][[2]][[yy2]])
    }else{
      plot(ppf[[8]][[2]][[1]])
      
    }
  })
  
  #Boxplot error tree
  output$boxtreeerror <- renderPlotly({
    yy1 <- as.numeric(rv3$bestnode$ids[rv3$bestnode$fill])
    yy2 <- yy1[!is.na(yy1)]
    
    if (length(yy2) > 0) {
      error <- round(ppf$oob.error.tree[yy2],3)
      p <-
        ggplot(error.tree, aes(
          x = trees, y = OOB.error.tree,fill = trees,key = ids
        )) + geom_boxplot() + scale_fill_manual(values = "#ffffff") +
        guides(fill = FALSE) + labs(x = "", y = "OOB error trees") +
        coord_flip() +   geom_point(
          aes(y = error),alpha = 0.1,size = I(2),color = I("red")
        )
      
      ggplotly(p,tooltip = "y") %>% layout(dragmode = "select")
    }else{
      error <- round(ppf$oob.error.tree[1], 3)
      p <- ggplot(error.tree, aes(x = trees, y = OOB.error.tree,fill = trees)) + geom_boxplot() + 
        scale_fill_manual(values = "#ffffff") +
        guides(fill = FALSE) +labs(x = "", y = "OOB error trees") +
        coord_flip()  +   geom_point(
          aes(y = error), alpha = 0.1, size = I(2),color = I("red") )
      
      ggplotly(p,tooltip = "y") %>% layout(dragmode = "select")
      
    }
    
  })
  
  #Mosaic plot
  output$plotmosaic <- renderPlotly({
    yy1 <- as.numeric(rv3$bestnode$ids[rv3$bestnode$fill])
    yy2 <- yy1[!is.na(yy1)]
    
    if (length(yy2) > 0) {
      PPtree_mosaic(ppf, yy2) %>% layout(dragmode = "select")  
    }else{
      PPtree_mosaic(ppf, 1) %>% layout(dragmode = "select")
      
    }
    
  })
  
  
################################
#             Tab 3            # 
################################  
  #
  
  selectedData <- reactive({
  input$goButton
      isolate(input$xcol)
  })
  # Side by side PPRF
  output$siderfpp <- renderPlotly({
    
    if (length(selectedData()) == length(unique(ppf$train[,ppf$class.var]))) {
      p <- ggplot(data = dat.sidepp.pl, aes(x = Classvote, y = Probability, colour = Class, key = ids) ) + 
        geom_jitter(height = 0, size = I(3), alpha = .5) +
        theme(legend.position = "none", axis.text.x  = element_text(angle = 45, vjust = 0.5), aspect.ratio = 1) +
        labs(x = "Class", title = "Side by side plot PPforest") + scale_colour_brewer(type = "qual",palette =
                                                                                        "Dark2")
    }
    if (length(selectedData( ) ) != length(unique(ppf$train[,ppf$class.var] ))) {
      dat <- dat.sidepp.pl %>% dplyr::filter(!Class %in% selectedData())
      dat_fil <- dat.sidepp.pl %>% dplyr::filter(Class %in% selectedData())
      
      p <- ggplot(data = dat, aes(Classvote, Probability, colour = Class, key = ids)) + 
        geom_jitter(height = 0, size = I(3), alpha = .1) +
        theme(legend.position = "none", axis.text.x  = element_text(angle = 45, vjust = 0.5), aspect.ratio = 1) +
        labs(x = "Class", title = "Side by side plot random forest") + scale_colour_brewer(type = "qual",palette =
                                                                                             "Dark2")
      p <- p + geom_jitter( height = 0, data = dat_fil, alpha = .5, size = I(3)) 
      
    }
    ggplotly(p,tooltip = c("colour", "y", "key"))
    
  })
  
  
  # Side by side RF
  output$siderf <- renderPlotly({

    if (length(selectedData()) == length(unique(ppf$train[,ppf$class.var]))) {
      p <- ggplot(data = dat.side.pl, aes(Classvote, Probability, colour = Class, key = ids) ) + 
        geom_jitter(height = 0, size = I(3), alpha = .5) + 
        theme(legend.position = "none", axis.text.x  = element_text(angle = 45, vjust = 0.5), aspect.ratio = 1) +
        labs(x = "Class", title = "Side by side plot random forest") + scale_colour_brewer(type = "qual",palette =
                                                                          "Dark2")
      
    }
    if (length(selectedData()) != length(unique(ppf$train[,ppf$class.var]))) {
      dat <- dat.side.pl %>% dplyr::filter(!Class %in% selectedData())
      dat_fil <- dat.side.pl %>% dplyr::filter(Class %in% selectedData())
      p <-
        ggplot(data = dat, aes(Classvote, Probability, colour = Class, key = ids)) + 
        geom_jitter(height = 0, size = I(3), alpha = .1) +
        theme(legend.position = "none", axis.text.x  = element_text(angle = 45, vjust = 0.5), aspect.ratio = 1) +
        labs(x = "Class", title = "Side by side plot random forest")  + scale_colour_brewer(type = "qual",palette =
                                                                          "Dark2")
      p <-
        p + geom_jitter( height = 0,data = dat_fil, size = I(3), alpha = .5
        ) 
    }
    
    ggplotly(p,tooltip = c("colour","y","key")) 
    
    
  })
  
  # ROC RF and PP

  output$plot_rocpp <- renderPlotly({

    if (length(selectedData()) == length(unique(ppf$train[,ppf$class.var]))) {
      
      p <- ggplot(data = dat.rocpprf, aes(x = specificities, y = sensitivities,colour = Classvote) ) +
        geom_path(alpha = 0.5, size = 1.2)+ scale_x_reverse() +
        geom_abline(intercept = 1, slope = 1, color = 'grey') +
        labs(x = 'Specificity', y = 'Sensitivity', title ="ROC curve PPforest") +
        scale_colour_brewer(type = "qual",palette = "Dark2") +  theme(legend.position = "none", aspect.ratio = 1) 
    }
    
    if (length(selectedData()) != length(unique(ppf$train[,ppf$class.var]))) {
      dat <- dat.rocpprf %>% dplyr::filter(!Classvote %in% selectedData())
      dat_fil <- dat.rocpprf %>% dplyr::filter(Classvote %in% selectedData())
      
      p <- dat %>% ggplot( aes(y = sensitivities, x = specificities, colour = Classvote)) + 
        geom_path( alpha = 0.1, size = 1.2 ) + scale_x_reverse() + labs(x = 'Specificity', y = 'Sensitivity', title ="ROC curve PPforest") +
        scale_colour_brewer(type = "qual",palette = "Dark2") +  theme(legend.position = "none", aspect.ratio = 1) 
      
      p <- p +  geom_path(data = dat_fil,size = 1.2 )
      }
    ggplotly(p, tooltip = c("colour","x","y")) 
    
  })
  
  
  output$plot_rocrf <- renderPlotly({
    
    if (length(selectedData()) ==  length(unique(ppf$train[,ppf$class.var]))) {
      p <- dat.rocrf %>% ggplot(aes( y = sensitivities,x = specificities, colour = Classvote
        )) + geom_path(alpha = 0.5, size=1.2)  + scale_x_reverse() +
        geom_abline(intercept = 1, slope = 1, color = 'grey')+
        labs(x = 'Specificity', y = 'Sensitivity', title ="ROC curve random forest") +
        scale_colour_brewer(type = "qual",palette = "Dark2") +  theme(legend.position = "none", aspect.ratio = 1) 
    }
  
    if (length(selectedData()) !=  length(unique(ppf$train[,ppf$class.var]))) {
      dat <- dat.rocrf %>% dplyr::filter(!Classvote %in% selectedData())
      dat_fil <- dat.rocrf %>% dplyr::filter(Classvote %in% selectedData())
      
      p <- dat %>% ggplot(aes(y = sensitivities, x = specificities, colour = Classvote)) +
        geom_path(alpha = 0.1, size = 1.2)  + scale_x_reverse() +
        geom_abline(intercept = 1, slope = 1, color='grey')+
        labs(x = 'Specificity', y = 'Sensitivity', title ="ROC curve random forest") +
        scale_colour_brewer(type = "qual",palette = "Dark2") +  theme(legend.position = "none", aspect.ratio = 1) 
      
      p <- p +  geom_path(data = dat_fil,size = 1.2 )
      }
    ggplotly(p, tooltip = c("colour","x","y")) 
    
  })
  
  #Plot OOB error
  output$plot_oobpp <- renderPlotly({
        
 if (length(selectedData()) == length(unique(ppf$train[,ppf$class.var]))) {
        
    
        p1 <- oob.pl %>% ggplot(aes( x = tree.id, y = OOB.error , colour = Class)) + 
          geom_point(alpha = .5) + geom_line(size = I(0.5), alpha = .5) + labs(y = "OOB error rate", 
          x = "Number of trees", title = "Cumulative OOB error") + ylim(c(0,1)) +
          theme(legend.position = "none", aspect.ratio = 1) + scale_color_manual(values = myColors)
      }
      
      if (length(selectedData()) != length(unique(ppf$train[,ppf$class.var]))) {
        dat <- oob.pl %>% dplyr::filter(!Class %in% selectedData())
        dat_fil <-oob.pl %>% dplyr::filter(Class %in% selectedData())
        
        p1 <- dat %>% ggplot(aes( x = tree.id, y = OOB.error , colour = Class) ) + 
          geom_point(alpha = .1) + geom_line(size = I(0.1),alpha = .1) + 
          labs(y = "OOB error rate", x = "Number of trees", title = "Cumulative OOB error") + ylim(c(0,1)) +
          theme(legend.position = "none", aspect.ratio = 1) + scale_color_manual(values = myColors)
        
        p1 <- p1 + geom_point(data = dat_fil, alpha = .5, aes(x = tree.id, y = OOB.error, colour = Class)) + geom_line(data = dat_fil,alpha = .5)
        
      }
      
      plotly::ggplotly(p1,tooltip = c("colour","y","x"))

  })
  
  
  output$plot_oobrf <- renderPlotly({

    
    if (length(selectedData()) == length(unique(ppf$train[,ppf$class.var]))) {
      p <- dat_rf %>% mutate(Class = as.factor(Class)) %>% 
        ggplot(aes(x = trees, y = OOB,colour = Class)) +
        geom_point(alpha = .5) + geom_line(alpha = .5) +  scale_color_manual(values = myColors) +
         labs(y = "OOB error rate", x = "Number of trees", title = "Cumulative OOB error") + ylim(c(0,1)) + 
        theme(legend.position = "none", aspect.ratio = 1) + scale_x_continuous(name = "Number of trees") 
      
    }
    if (length(selectedData()) != length(unique(ppf$train[,ppf$class.var]))) {
      dat <- dat_rf %>% dplyr::filter(!Class %in% selectedData())
      dat_fil <- dat_rf %>% dplyr::filter(Class %in% selectedData())
  
      p <- dat %>% mutate(Class = as.factor(Class)) %>% 
        ggplot(aes(x = trees, y = OOB,colour = Class)) +
        geom_point(alpha = .1) + geom_line(alpha = .1) +  scale_color_manual(values = myColors) +
        labs(y = "OOB error rate", x = "Number of trees", title = "Cumulative OOB error") + ylim(c(0,1)) + theme(legend.position = "none", aspect.ratio = 1) 
        
      p <- p + geom_point(data = dat_fil, aes(x = trees, y = OOB, colour = Class), alpha = .5) + 
        geom_line(data = dat_fil, alpha = .5 )
    }
    
    ggplotly(p, tooltip = c("colour","y","x"))
    
  })
  
  
  
  #Confussion matrix tables PP and RF
  
  output$tablepp <- renderTable({
    ppf$confusion
  }, caption = "Confusion Matrix PPforest",caption.placement = getOption("xtable.caption.placement", "top"))
  
  
  output$tablerf <- renderTable({
    rf$confusion
  }, caption = "Confusion Matrix randomForest",caption.placement = getOption("xtable.caption.placement", "top"))
 
  
  output$plot_impopp <- renderPlotly({
    if(nrow(imp )>20){
    
   
    p <- ggplot(data = imp[1:20,], aes(imp2,nm) ) + geom_point() + labs(x = "Importance", 
                                                                             y = "") 
    }else{
      p <- ggplot(data = imp, aes(imp2,nm) ) +ggplot2::geom_point() + labs(x = "Importance", 
                                                                                y = "")
    }
    ggplotly(p) 
    
  })
  
  
  output$plot_imporf <- renderPlotly({
    if(nrow(imp )>20){
    p <- ggplot(data = imp.pl[1:20,], aes(imp,nm) ) + geom_point() + 
      labs(x = "Importance", y = "") 
    }else{
      p <- ggplot(data = imp.pl, aes(imp,nm) ) + geom_point() + 
      labs(x = "Importance", y = "") }
    ggplotly(p) 
    
  }) 
  
}
shinyApp(ui, server)
}



