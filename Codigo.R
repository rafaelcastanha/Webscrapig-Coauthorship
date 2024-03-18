{
  
  #Bibliotecas  
  
  #Instale os pacotes se necessário
  
  #install.packages('rvest')
  #install.packages('dplyr')  
  #install.packages('igraph')  
  #install.packages('visNetwork')  
  #install.packages('stringi')  
  
  library(rvest)
  library(dplyr)
  library(igraph)
  library(visNetwork)
  library(stringi)
  
  #WEB SCRAPING dos links dos volumes e números
  
  
  rl<-readline("Deseja analisar a revista Em Questão (1) ou Encontros Bibli (2)?:")
  
  rl2<-readline("Caso queira analisar um número específico cole o link aqui, caso contrário digite 1:")
  
  edges_aux<-readline("Digite o número mínimo (maior que zero) de frequência de coautoria entre dois autores:")
  
  
  #Todos volumes e numeros da revista Em Questão
  
  if (rl == "1" & rl2 == "1"){
    
    links_loop<-character()
    
    extract<-read_html("https://seer.ufrgs.br/index.php/EmQuestao/issue/archive")%>%
      html_nodes(".title") %>%
      html_attr("href")
    
    links_loop<-append(extract, links_loop)
    
    links_loop<-links_loop[!is.na(links_loop)]
    
    base<-character()
    
    for (i in links_loop){
      extract<-read_html(i)%>%
        html_nodes(".authors") %>%
        html_text2()
      
      base<-append(extract, base)
    }
    
    #Todos volumes e numeros da revista Encontros Bibli
    
  }
  
  if (rl=="2" & rl2=="1"){
    
    total<-read_html("https://periodicos.ufsc.br/index.php/eb/issue/archive")%>%
      html_nodes(".current") %>%
      html_text2()
    
    valor <- gsub("^.* (\\d+)$", "\\1", total)
    total<-as.numeric(valor[2])
    
    i<-1
    
    page1<-paste0('https://periodicos.ufsc.br/index.php/eb/issue/archive',"/",i)
    
    for (i in 1:(ceiling(total/20))){
      
      page1[i]<-paste0('https://periodicos.ufsc.br/index.php/eb/issue/archive',"/",i)
      
    }
    
    links_loop<-character()
    
    for (i in 1:(ceiling(total/20))){
      
      extract<-read_html(page1[i])%>%
        html_nodes(".title") %>%
        html_attr("href")
      
      links_loop<-append(extract, links_loop)
      
      links_loop<-links_loop[!is.na(links_loop)]
      
    }
    
    base<-character()
    
    for (i in links_loop){
      extract<-read_html(i)%>%
        html_nodes(".authors") %>%
        html_text2()
      
      base<-append(extract, base)
    }
    
    
  }
  
  #Volume ou número único da Em Questão
  
  
  if (rl=="1" & rl2!="1"){
    
    base<-read_html(rl2)%>%
      html_nodes(".authors") %>%
      html_text2()
    
  }
  
  #Volume ou número único da Encontros Bibli
  
  
  if (rl=="2" & rl2!="1"){
    
    base<-read_html(rl2)%>%
      html_nodes(".authors") %>%
      html_text2()
    
  }
  
  #Organização dos dados extraídos
  
  
  base<-sub("Comissão Editorial Em Questão", "", base)
  base<-sub("Revista Encontros Bibli", "", base)
  base<-sub("Editor Encontros Bibli", "", base)
  
  base<-stri_trans_general(base, "Latin-ASCII")
  

  ### Definição dos autores mais produtivos   
  
  autores_prod <- (unlist(strsplit(base, ", ")))   #Autores
  
  prod<-as.data.frame(table(autores_prod)) #Producao por autor
  
  prod_total<-prod[order(prod$Freq,decreasing=TRUE),]
  
  
  prod_top10<-na.omit(prod_total[1:10,]) #10 autores mais produtivos
  
  colnames(prod_top10)[1]<-"Autores"
  colnames(prod_top10)[2]<-"Frequência"
  
  
  ### Estruturação dos dados para construção da rede 
  
  b4<-strsplit(as.character(base), split = ", " , fixed = FALSE)
  
  b5<-as.data.frame(do.call(cbind, b4))
  
  b6<-stack(b5)
  
  #Matriz de coautoria
  
  mtx<-table(stack(b5))
  
  mtx[mtx>1]<-1
  
  mtx_coaut<-mtx%*%t(mtx) 
  
  diag(mtx_coaut)<-0
  
  #Rede igraph
  
  rede_coaut<-graph_from_adjacency_matrix(mtx_coaut, weighted = T, mode = "undirected")
  
  #rede visNetwork
  
  vis_coaut<-toVisNetworkData(rede_coaut)
  
  #condicional e erro para frequencia mínima
  
  if (edges_aux > max(vis_coaut$edges$weight)){
    
    stop (paste0("Não há frequência de coautoria menor ou igual a ", edges_aux))
  }
  
  
  node_coaut<-data.frame("id"=vis_coaut$nodes$id, "label"=vis_coaut$nodes$label)
  links_coaut<-as.data.frame(vis_coaut$edges) 
  colnames(links_coaut)[3]<-'width'
  links_coaut<-filter(links_coaut, width>=edges_aux)

  if (edges_aux >= 2){
    
    id_aux<-unique(as.character(c(links_coaut$from, links_coaut$to)))
    node_coaut<-data.frame("id"=id_aux, "label"=id_aux)
    links_coaut<-filter(links_coaut, width>=edges_aux)
    prod_2<-as.data.frame(table(as.character(c(links_coaut$from, links_coaut$to))))
    prod_2<-prod_2[order(prod_2$Freq,decreasing=TRUE),]
    colnames(prod_2)[1]<-'Autores'
    colnames(prod_2)[2]<-'#Coautores'
    
  }
  

  #REDE DE COAUTORIAS

  #Construção da rede de coautorias
  
  visNetwork(node_coaut, links_coaut) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visIgraphLayout(layout = "layout_with_fr")
  
}


#Salvando a rede em html

#Execute o código abaixo para salvar a rede em .html

  
visSave(graph=visNetwork(node_coaut, links_coaut) %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
          visIgraphLayout(layout = "layout_with_fr"), "rede de coautorias.html")


#Comandos a serem executados

#prod_total: produção de cada autor
#prod_top10: Produção dos 10 pesquisadores mais produtivos
#prod_2: Executará somente se a frequencia de coautoria entre dois autores for maior ou igual a 2.
#será construida uma tabela contendo todos autores que atendem a este critério e a quantidade a coautores de cada um

