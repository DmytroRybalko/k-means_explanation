# Realise k-means algorithm as recursive function
k_means <- function(n, env) {
  
  trig <- 1:(n + 1)
  
  if (length(trig) > 1) {
    
    # Calculate the Euclidean distances for the initial cluster group by the formula: sqrt((xi-xc)^2 + (yi-yc)^2)
    step1 <- bind_cols(map_dfr(env$df_xy, rep, env$n_clust),
                       map_dfr(env$df_clust, rep, each = env$len_xy)) %>% 
      mutate(Eu_dist = sqrt((.$x - .$Cx)^2 + (.$y - .$Cy)^2))
    
    # Determine which points to which clusters belong
    step1 %>% 
      select(-Cx, -Cy) %>% 
      spread(Cluster, Eu_dist) %>% 
      nest(-x, -y) %>%
      mutate(Eu_dist = map_dbl(.$data, min),
             Cluster = map_dbl(.$data, which.min) %>% factor(., ordered = T)) %>%
      unnest(data) %>% 
      arrange(match(x, env$df_xy$x)) -> step2
    
    # Get the affiliation of points to cluster's centers
    step2 %>% 
      left_join(env$df_clust, by = 'Cluster') %>% 
      select(-num_range("", 1:env$n_clust)) -> step3
    
    # Make list of tibbles where each tibble contain data from corresponding iteration 
    i <- length(env$df_list) + 1 - n
    names(env$df_list)[i] <- paste0("Iteration", i)
    env$df_list[[i]] <- step3 %>% 
      mutate(Iteration = names(env$df_list)[i]) %>% 
      rowid_to_column()
    
    # Recalculate the coordinates of new cluster's center for each cluster
    step3 %>% 
      group_by(Cluster) %>% 
      mutate(Cx = mean(x),
             Cy = mean(y)) -> step4
    
    # Allocate coordinates of centers and numbers of the new clusters for the next iteration
    step4 %>% 
      select(Cluster, Cx, Cy) %>% 
      group_by(Cluster) %>% 
      unique(.) %>% 
      arrange(Cluster) -> env$df_clust
    
    trig <- trig[-1]
    
    return(k_means(n - 1, env))
  } else { return(env$df_clust) }
}

# Make data frame for taking results of all iterations
wraper_kmeans <- function(df_xy, df_clust, iter) {
  
  wrap.env <- new.env()
  wrap.env$df_xy <- df_xy
  wrap.env$df_clust <- df_clust
  wrap.env$len_xy <- dim(df_xy)[1]
  wrap.env$n_clust <- dim(df_clust)[1]
  
  # Initialize list for storing data from k_means function
  wrap.env$df_list <- lapply(rep("", iter), paste0)
  
  k_means(iter, wrap.env)
  
  return(wrap.env$df_list)
}

# Make suitable structure to get results of k_means algorithm
get_kmeans = function(which_i, df_kmeans) {
  
  ls_kmeans <- list()
  
  df_kmeans[[which_i]] %>%
    select(Cluster, Cx, Cy) %>%
    group_by(Cluster) %>% 
    unique(.) %>% 
    arrange(Cluster) %>% 
    ungroup() -> ls_kmeans$`Cluster means`
  
  df_kmeans[[which_i]] %>% 
    count(Cluster) -> ls_kmeans$`Clustering vector`
  
  df_kmeans[[which_i]] %>%
    group_by(Cluster) %>% 
    summarise(sum(Eu_dist)) %>% 
    ungroup() -> ls_kmeans$`Euclidean distance wihtin clusters` 
  
  return(ls_kmeans)  
}

# Make dataset for geom_polygon() function
df_polygon <- function(df_kmeans) {
  
  df_kmeans %>% 
    map(~ group_by(.x, Cluster) %>%
          by(.$Cluster, FUN = function(.) {.[chull(.$x, .$y), ]}) %>% 
          map_dfr( ~ .) %>% ungroup()) %>% 
    map_dfr( ~ .)
}
