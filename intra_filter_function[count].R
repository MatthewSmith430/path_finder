intra_count<-function(NET,COUNTRY_LIST,sub){
  
  NET<-igraph::simplify(NET)
  V(NET)$degree<-degree(NET)
  NET<-delete.vertices(NET, 
                  V(NET)[ degree(NET) <2 ])
  H<-igraph::subgraph_isomorphisms(sub, NET)
  
  EL<-list()
  seq_list<-list()
  for (i in 1:length(H)){
    R<-H[[i]]
    seq_list[[i]]<-R
    R1<-as_ids(R)
    R2<-as.matrix(R1)%>%t()%>%
      as.data.frame()
    EL[[i]]<-R2
  }
  #EL2<-do.call(rbind.data.frame, EL)
  #colnames(EL2)<-c("1","2","3","4","5")
  
  EL2b<-purrr::map_df(EL,data.frame)
  
  ISO<-list()
  for (i in 1:length(EL)){
    VEC<-seq_list[[i]]
    subCheck<-igraph::induced_subgraph(NET, VEC)
    ISO[[i]]<-igraph::ecount(subCheck)
  }
  DFedges<-purrr::map_df(ISO,data.frame)
    #plyr::ldply(ISO,data.frame)
  colnames(DFedges)<-"number_edges"
  EL2<-bind_cols(EL2b,DFedges)|>
    filter(number_edges<5)
  
  V1c<-factor(EL2$V1%in%COUNTRY_LIST, 
              levels = c(TRUE,FALSE),
              labels = c(1, 0))%>%
    as.character%>%
    as.numeric()
  
  V2c<-factor(EL2$V2%in%COUNTRY_LIST, 
              levels = c(TRUE,FALSE),
              labels = c(1, 0))%>%
    as.character%>%
    as.numeric()
  
  V3c<-factor(EL2$V3%in%COUNTRY_LIST, 
              levels = c(TRUE,FALSE),
              labels = c(1, 0))%>%
    as.character%>%
    as.numeric()
  
  V4c<-factor(EL2$V4%in%COUNTRY_LIST, 
              levels = c(TRUE,FALSE),
              labels = c(1, 0))%>%
    as.character%>%
    as.numeric()
  EL2<-mutate(EL2,check_v1=V1c,check_v2=V2c,
              check_v3=V3c,check_v4=V4c)
  
  SUM<-EL2[,6:9]
  SS<-rowSums(SUM)
  
  EL2<-EL2%>%
    mutate(row_sum=SS)
  
  EL3<-EL2%>%
    filter(row_sum==2)%>%
    filter(number_edges==4)

  count_sg<-nrow(EL3)
  
  return(as.numeric(count_sg))
}
