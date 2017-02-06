#Data trans density and mosaic
dat_dens <- function(PPclassOBJ, node.id, Rule, legend = TRUE, std = TRUE, 
                        image = FALSE, diff.prop = 0.2,c1=FALSE) {
  
  searchGroup <- function(node.id, TS, gName) {
    flag <- TRUE
    sel.id <- TS[node.id, 2:3]
    LR.id <- c(TRUE, FALSE)
    sel.group <- NULL
    i <- 1
    while ((sel.id[i] != 0) && (i < length(sel.id))) {
      if (TS[sel.id[i], 2] != 0) {
        sel.id <- c(sel.id, TS[sel.id[i], 2:3])
        if (LR.id[i]) 
          LR.id <- c(LR.id, c(TRUE, TRUE))
        else LR.id <- c(LR.id, c(FALSE, FALSE))
      }
      if (TS[sel.id[i + 1], 2] != 0) {
        sel.id <- c(sel.id, TS[sel.id[i + 1], 2:3])
        if (LR.id[i + 1]) 
          LR.id <- c(LR.id, c(TRUE, TRUE))
        else LR.id <- c(LR.id, c(FALSE, FALSE))
      }
      i <- i + 2
    }
    sel.Name <- TS[sel.id[which(TS[sel.id, 2] == 0)], 3]
    selName <- sort(gName[sel.Name])
    L.list <- sort(gName[sel.Name[LR.id[which(TS[sel.id, 
                                                 2] == 0)]]])
    R.list <- sort(gName[sel.Name[!LR.id[which(TS[sel.id, 
                                                  2] == 0)]]])
    return(list(selName = selName, Llist = L.list, Rlist = R.list))
  }
  
  TS <- PPclassOBJ$Tree.Struct
  Alpha <- PPclassOBJ$projbest.node
  cut.off <- PPclassOBJ$splitCutoff.node
  origdata <- PPclassOBJ$origdata
  origclass <- PPclassOBJ$origclass
  p <- ncol(origdata)
  gName <- names(table(origclass))
  
  if (TS[node.id, 2] != 0) {
    SG.result <- searchGroup(node.id, TS, gName)
    selG <- SG.result$selName
    selL <- SG.result$Llist
    selR <- SG.result$Rlist
    sel.id <- NULL
    LR.class <- NULL
    for (i in 1:length(selG)) {
      sel.id <- c(sel.id, which(origclass == selG[i]))
      LR.class <- c(LR.class, rep(ifelse(sum(selL == selG[i]) != 
                                           0, "L", "R"), length(which(origclass == selG[i]))))
    }
    proj.data <- c(as.matrix(origdata) %*% as.matrix(Alpha[TS[node.id, 
                                                              4], ]))[sel.id]
    
  
    proj.class <- origclass[sel.id]
    plot.data <- data.frame(proj.data = proj.data, origclass = proj.class,
                            cut = cut.off[TS[node.id, 4], Rule], node.id =node.id, LR.class,Dir=as.factor(proj.data>cut.off[TS[node.id, 4], Rule]))
    colnames(plot.data)[2]<-"Class"
  
    
   plot.data
 
   }
}

# #Density plot Tab 2
# PPtree_densold <- function(ppf, tr) {
# 
# nodes <- ppf[[8]][[tr]]$Tree.Struct[ppf[[8]][[tr]]$Tree.Struct[,4]!=0,1]
# nn <- data.frame(nn = 1:length(ppf[[8]][[tr]]$Tree.Struct[,1]))
# densf <- function(x){
#   dat_dens(PPclassOBJ=ppf[[8]][[tr]],node.id=x,Rule=1)
# }
# 
# dat_pl<- apply(nn, 1, densf)  %>%  lapply(data.frame) %>%bind_rows()
# 
# 
# 
# myColors <- brewer.pal(length(unique(ppf[[8]][[tr]]$origclass)),"Dark2")
# names(myColors) <- levels(dat_pl$Class)
# 
# 
# if( length( unique(dat_pl$node.id ) ) >=3 ){
# p1 <- dat_pl %>% filter(node.id %in% unique(node.id )[nodes] ) %>% 
#   ggplot( aes(x = proj.data, group = Class,fill = Class)) + geom_density(alpha = .5) +
#         facet_grid( ~node.id, scales = 'free') + scale_fill_manual(values= myColors) +
#         theme(legend.position="none", aspect.ratio = 1) + geom_vline(aes(xintercept = cut), 
#              linetype = "dashed",  color = 2 ) + xlab("")  
# 
# }else{
#   p1 <-  ggplot(dat_pl, aes(x = proj.data, group = Class,fill = Class) )+ 
#     geom_density(alpha = .5) +
#     facet_grid(~node.id, scales = "free_y")   +  scale_fill_manual(values= myColors) +
#     theme(legend.position="none",aspect.ratio = 1) + 
#     geom_vline( aes(xintercept = cut), linetype = 'dashed', color = 2) + 
#     xlab("")  
#   
#   
#   
# }
# 
# ggplotly(p1,tooltip=c("fill","x"))
# 
# }

PPtree_dens <- function(ppf, tr, nodes = NULL) {
  if(is.null(nodes)){
    nodes <- ppf[[8]][[tr]]$Tree.Struct[ppf[[8]][[tr]]$Tree.Struct[,4]!=0,1][1:3]
  }
  nn <- data.frame(nn = nodes)
  densf <- function(x) {
    dat_dens(PPclassOBJ = ppf[["output.trees"]][[tr]],
             node.id = x,
             Rule = 1)
    
  }
  
  dat_pl <- apply(nn, 1, densf)  %>%  lapply(data.frame) %>% bind_rows()
  
  myColors <- brewer.pal(dim(unique(ppf$train[ppf$class.var]))[1], "Dark2")
  names(myColors) <- levels(ppf$train[ppf$class.var][, 1])
  dat_pl$Class <- as.factor(dat_pl$Class)
  
  if(is.factor(ppf$train[ppf$class.var][, 1])){
    levels(dat_pl$Class) <-  levels(ppf$train[ppf$class.var][, 1])
    
  }else{
    levels(dat_pl$Class) <-  levels(as.factor(ppf$train[ppf$class.var][, 1]))
  }
  
  p1 <- dat_pl %>% filter(node.id %in%nodes) %>%
    ggplot( aes(  x = proj.data, group = Class, fill = Class ) ) + 
    geom_density(alpha = .5) + facet_grid(~ node.id, scales = 'free') + 
    scale_fill_manual(values = myColors) + geom_vline(aes(xintercept = cut),
                                                      linetype = "dashed",
                                                      color = 2) + xlab("")
  
  

    p1 <-  p1 + theme(legend.position = "none",aspect.ratio = 1)
    ggplotly(p1, tooltip = c("fill", "x"))

}

#Mosaic plot Tab 2
PPtree_mosaic <- function(ppf,tr, nodes = NULL){
  
  
  if(is.null(nodes)){
    nodes <- ppf[[8]][[tr]]$Tree.Struct[ppf[[8]][[tr]]$Tree.Struct[,4]!=0,1][1:3]
  }
  nn <- data.frame(nn = nodes)
  
  densf <- function(x){
    dat_dens(PPclassOBJ=ppf[[8]][[tr]],node.id=x,Rule=1)
  }

  dat_pl<- apply(nn, 1, densf)  %>%  lapply(data.frame) %>%bind_rows()
  
  levels(dat_pl$Dir)<-c("Left", "Right")
  myColors <- brewer.pal(length( unique( ppf[[8]][[tr]]$origclass ) ), "Dark2")
  names(myColors) <- levels(dat_pl$Class)
 
  dat_mosaic <- data.frame( with(dat_pl, table(Class, Dir,node.id) ) )
  
 
    p1 <- dat_mosaic %>% filter(node.id %in%nodes) %>% ggplot() + 
      geom_mosaic( aes(weight = Freq, x = product(Class,Dir) ,fill = Class))+facet_grid(~node.id)+
      scale_fill_manual(values = myColors) + theme(legend.position="none",axis.text.x  = element_text(angle=90, vjust=0.5),aspect.ratio = 1) +xlab("Class")
  
  ggplotly(p1)

}




##ROC curve tab 3
rocky <- function(response, predictor) {
  aux <- roc(response, predictor)
  sensi <- aux$sensitivities
  speci <- aux$specificities
  tresh <- aux$thresholds
  auc <- aux$auc
  data.frame(
    sensitivities = sensi, specificities = speci, auc = rep(auc, length(sensi))
  )
}

  #plot oob error
ppf_oob_error <- function(ppf, nsplit1) {
  ntree <- NULL
  value <- NULL
  variable <- NULL
  error.cum <- function(ppf, m) {
    l.train <- 1:nrow(ppf$train)
    index2 <- lapply(as.numeric(attributes(ppf$boot.samp)$names[1:m]), function(x)
               x + 1)
    # oob.obs <-
    #   plyr::ldply(index2, function(x)
    #     (!l.train %in% x))

    
    oob.obs <- index2 %>%  lapply(function(x)
      data.frame(obs=!l.train %in% x)) %>% bind_cols() %>%t()
    pred.mtree <- ppf$vote.mat[1:m,]

    
    
    oob.pred <-
      sapply(
        X = 1:nrow(ppf$train), FUN = function(i) {
          t1 <- table(pred.mtree[oob.obs[, i] == TRUE, i])
          names(t1)[which.max(t1)]
        }
      )
    
    
    oob.mat <- sapply(
      X = 1:nrow(ppf$train), FUN = function(i) {
        table(pred.mtree[oob.obs[, i] == TRUE, i])
      }
    )
    
    aux <- unlist(lapply(oob.pred, is.null))
    oob.all <-
      1 - sum(diag(table(unlist(oob.pred[!aux]), ppf$train[!aux, 1]))) / length(ppf$train[!aux, 1])
    tab.err <- table(unlist(oob.pred[!aux]), ppf$train[!aux, 1])
    oob.class <- 1 - diag(tab.err) / apply(tab.err, 2, sum)
    c(oob.all, oob.class)
  }
  
  # oob.err.sp <-
  #   plyr::mdply(data.frame(m = round(seq(
  #     2, round(ppf$n.tree),nsplit1
  #   ))), error.cum, ppf = ppf)
  # 
  # 
  
  
  
  mm <- data.frame(m = round(seq(
    2, round(ppf$n.tree),nsplit1)))
  
  errcfun <- function(x){
    error.cum(ppf,x)
  }
  
  oob.err.sp <- data.frame(mm, apply(mm, 1,errcfun)  %>% t() )

  
  
  
  names(oob.err.sp)[1:2] <- c("tree.id", "All")
  
  oob.pl <- reshape2::melt(oob.err.sp, id.vars = "tree.id")
  colnames(oob.pl)[2:3] <- c("Class", "OOB.error")
  oob.pl
}

