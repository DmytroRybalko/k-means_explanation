# Realise k-means algorithm as recursive function
k_means <- function(n, df_cl, env) {
  
  trig <- 1:(n + 1)
  
  if (length(trig) > 1) {
    
    # Calculate the Euclidean distances for the initial cluster group by the formula: sqrt((xi-xc)^2 + (yi-yc)^2)
    step1 <- bind_cols(map_dfr(df_xy, rep, n_cl),
                       map_dfr(df_cl, rep, each = n_xy)) %>% 
      mutate(Eu_dist = sqrt((.$x - .$Cx)^2 + (.$y - .$Cy)^2))
    
    # Determine which points to which clusters belong
    step1 %>% 
      select(-Cx, -Cy) %>% 
      spread(Cluster, Eu_dist) %>% 
      nest(-x, -y) %>%
      mutate(Eu_dist = map_dbl(.$data, min),
             Cluster = map_dbl(.$data, which.min) %>% as.character()) %>%
      unnest(data) %>% 
      arrange(match(x, df_xy$x)) -> step2
    
    # Get the affiliation of points to cluster's centers
    step2 %>%
      left_join(step1 %>% select(Cx, Cy, Eu_dist), by = 'Eu_dist') %>% 
      select(-num_range("", 1:n_cl)) -> step3
    
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
      arrange(Cluster) -> new_clusters
    
    trig <- trig[-1]
    
    return(k_means(n - 1, new_clusters, env))
  } else { return(df_cl) }
}

# Make data frame for taking results of all iterations
wraper_kmeans <- function(iter, df_cl) {
  # Initialize list for storing data from k_means function
  wrap.env <- new.env()
  wrap.env$df_list <- lapply(rep("", iter), paste0)
  
  k_means(iter, df_cl, wrap.env)
  
  return(wrap.env$df_list)
}
