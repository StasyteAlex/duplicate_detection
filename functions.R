# functions

find_matching_brand <- function(definition, brands) {
  #ok
  for(b in brands){
    if(grepl(b, definition)){
      return(b)
    }
  }
  return("unknown")
}

data_prep <- function(data, brands){
  #ok
  brands <- unique(tolower(brands))
  brands <- c(brands, "insignia", "contex", "dynex", "tcl", "mitsubishi", 
              "avue", "optoma", "venturer", "curtisyoung", "hiteker", "gpx", 
              "viore", "elite")
  
  data <- data %>% 
    mutate(definition = title) %>% 
    mutate(definition = tolower(definition), 
           definition = gsub('"', 'inch', definition), 
           definition = gsub("'", 'inch', definition), 
           definition = gsub('-inch', 'inch', definition), 
           definition = gsub(' inch', 'inch', definition), 
           definition = gsub('/ ', "", definition), 
           definition = gsub(' hz', "hz", definition), 
           definition = gsub("[()]", "", definition),
           definition = gsub("”", "inch", definition),
           definition = gsub("\\[", "", definition),
           definition = gsub("newegg.com |bestbuy.com |amazon.com |thenerds.net", 
                             "", definition),brand = tolower(brand))
  data$brand2 <- mapply(find_matching_brand, data$definition, 
                         MoreArgs = list(brands))
  data$brand[is.na(data$brand)] <- data$brand2[is.na(data$brand)]
  

  data <- data %>% select(-brand2) %>% 
    mutate(
      Refresh1 = ifelse(is.na(Refresh1),
                             str_extract(definition, "\\b\\d+hz\\b"),
                          Refresh1),
      Refresh1 = ifelse(is.na(Refresh1),
                        Refresh2,
                        Refresh1),
      Refresh1 = tolower(Refresh1),
      Refresh1 = as.numeric(str_extract(Refresh1, "\\b\\d+")),
      Refresh1 = ifelse(is.na(Refresh1), NA, paste0(Refresh1, "hz")),
           Size1 = ifelse(is.na(Size1),
                           str_extract(definition, "(\\b\\d+inch)"), 
                           Size1), 
      HDMI1 = ifelse(HDMI1 == "Yes", HDMI3, HDMI1),
      HDMI1 = ifelse(is.na(HDMI1), HDMI2, HDMI1),
      HDMI = as.numeric(str_extract(HDMI1, "\\b\\d+")), 
      HDMI = ifelse(!is.na(HDMI), paste0(HDMI, "hdmi"), NA),
      Size1 = gsub(" inches", "inch", Size1), 
      Size1 = gsub('\"', 'inch', Size1),
      Size1 = gsub("\''", 'inch', Size1), 
      Size1 = str_extract(Size1, "^[^\\s]+"), 
      Size1 = ifelse(grepl("inch", Size1), Size1, 
                     paste0(Size1, "inch")), 
      Size1 = gsub("31-1/2inch", "32inch", Size1),
      Size1 = gsub("49.9inch", "50inch", Size1),
      Size1 = gsub("64.5inch", "65inch", Size1),
      Size1 = gsub("59.06inch", "60inch", Size1),
      Size1 = gsub("60.1inch", "60inch", Size1),
      Type2 = gsub("Flat-panel LCD", "LCD Flat-Panel", Type2), 
      Resolution2 = gsub("Full HD ", "", Resolution2),
      Resolution2 = gsub("SXGA ", "", Resolution2), 
      Resolution2 = gsub("WSXGA+ ", "", Resolution2),
      Resolution2 = gsub("WSXGA", "", Resolution2),
      Resolution2 = gsub("WUXGA ", "", Resolution2),
      Resolution2 = gsub("[()]", "", Resolution2),
      Resolution2 = gsub("+", "", Resolution2),
      Weight1 = gsub("[()]", "", Weight1),
      Ratio1 = gsub(",", "", Ratio1),
      Ratio2 = gsub(",", "", Ratio2),
      Resolution1 = gsub("i", "p", Resolution1),
      Resolution1 = gsub(" x ", "x", Resolution1),
      Resolution2 = gsub(" x ", "x", Resolution2),
      #Weight1 = as.numeric(str_extract(Weight1, "\\b\\d+")), 
      #Weight2 = as.numeric(str_extract(Weight2, "\\b\\d+")), 
      Brightness1 = as.numeric(str_extract(Brightness1, "\\b\\d+")), 
      Brightness1 = ifelse(is.na(Brightness1), NA, paste0(Brightness1, "cd/m2")), 
      def = paste(brand, Size1, Resolution1, Resolution2, #Weight1, 
                  Refresh1, 
                  Ratio1, Ratio2, Brightness1, #Color1, 
                  HDMI,
                  #Weight2, 
                  Ratio3#, #Refresh2
                  ), 
      def = paste(def, str_extract(definition, "led")), 
      def = paste(def, str_extract(definition, "hdtv")), 
      def = paste(def, str_extract(definition, "lcd")), 
      def = paste(def, str_extract(definition, "dvd")), 
      def = paste(def, str_extract(definition, "4k")),
      # id = str_extract(str_extract(definition, "\\b[A-Z0-9]+\\b"), "[A-Z]{3}\\d+"), 
      # id3 = str_extract(definition, "\\b[A-Z0-9]+\\b"), 
      # #                id),
      # # id = ifelse(nchar(id) < 7, NA, id),
      definition = tolower(def), 
      # definition = paste(definition, id), 
      definition = gsub(" NA", " ", definition),
      definition = gsub(" na", " ", definition), 
      definition = tolower(definition), 
      definition = gsub(",", "", definition), 
      extra = str_extract(tolower(title), "\\b\\d+p\\b"), 
      definition = ifelse(!grepl("\\b\\d+p\\b", definition), 
                           paste(definition, extra), 
                           definition))

  
  data <- data %>% 
    mutate(brand = gsub("lg electronics", "lg", brand), 
           brand = gsub("jvc tv", "jvc", brand))
  # key_words <- unique(c(tolower(data$Size1), tolower(data$Resolution1), 
  #                       tolower(data$Resolution2), 
  #                       #data$Weight1, 
  #                       tolower(data$Refresh1), #data$UPC1, 
  #                       tolower(data$Ratio1), tolower(data$Ratio2), 
  #                       tolower(data$Brightness1), tolower(data$brand), 
  #                       tolower(data$Color1), #data$Weight2, 
  #                       tolower(data$Ratio3), 
  #                       tolower(data$Refresh2), "led", "hdtv", "lcd", "dvd"))
  
  
  # key_words <- key_words[!is.na(key_words)]
  data <- data %>% 
    select(shop, url, modelID, title, definition, Product_Name, brand)
  # results <- list()
  # results[["data"]] <- data
  # results[["key_words"]] <- key_words
  return(data)
}

finding_binary_vector <- function(data, brands){
  # change the pattern thing
  pattern <- "[a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*"

  key_words <- unique(unlist(str_extract_all(data$definition, pattern)))
  key_words <- c(tolower(unique(brands)), key_words, "led", "dvd", "hdtv", "lcd")
  
  binary_vector <- matrix(data = 0, 
                          nrow = length(key_words), 
                          ncol = length(data$Product_Name))
  binary_vector <- as_tibble(binary_vector)
  colnames(binary_vector) <- data$Product_Name
  binary_vector$key_words <- key_words
  binary_vector$key_words_nr <- 0:(length(key_words)-1)
  
  for(id in data$Product_Name){
    name <- data$definition[data$Product_Name == id]
    for(kw in key_words){
      if(grepl(kw, name, fixed = TRUE)){
        binary_vector[binary_vector$key_words == kw, id] <- 1
      }
    }
  }
  
  return(binary_vector)
  
}

hash_function <- function(word_nr, mod_value, nr_hash_functions) {

  #ok
  result <- tibble(word_nr = word_nr)
  result[, paste0("hash", 1:nr_hash_functions)] <- NA
  
  for(nr in 1:nr_hash_functions){
    a <- sample(1:mod_value, 1)
    b <- sample(1:mod_value, 1)
    result[, paste0("hash", nr)] <- (a * result[, "word_nr"] + b) %% mod_value
  }

  return(result)
}


minhash_signature <- function(binary_vector) {
  
  nr_hash_functions <- 50
  mod_value <- nr_hash_functions + 1
  while(!as.logical(isprime(mod_value))){
    mod_value <- mod_value + 1
  }
  #ok
  print("Start MinHash")
  word_nr <- binary_vector$key_words_nr
  products_nr <- grep("Name", colnames(binary_vector), value = TRUE)
  
  signature <- matrix(data = Inf, 
                      nrow = nr_hash_functions, 
                      ncol = length(products_nr)) %>% 
    as_tibble()
  colnames(signature) <- products_nr
  
  total_ite <- length(products_nr)
  count <- 1
  hash_value <- hash_function(word_nr, mod_value, nr_hash_functions)
  for(product in products_nr){
    binary_vector[binary_vector[, product] == 0, product] <- NA
    for(hash_nr in 1:nr_hash_functions){
      
      hash_value_sig <- min(hash_value[, paste0("hash", hash_nr)]*binary_vector[, product], 
                            na.rm = TRUE
                            )
      signature[hash_nr, product] <- hash_value_sig
      
    }
    progress <- count * 100 / total_ite
    cat(sprintf("\rMinHash Progress: %f%%", progress))
    flush.console()
    count <- count+1
  }
  
  
  return(signature)
}

calculate_b_bands_and_r_rows <- function(t, n){
  b <- as.integer(-(n*log(t))/pracma::lambertWp(-(n*log(t))))
  r <- floor(n/b)
  
  b <- n/r

  result <- list()
  result[["b"]] <- b
  result[["r"]] <- r
  return(result)
}


calculate_lsh <- function(data, bands){

  #ok
  num_rows <- dim(data)[1]
  b <- bands
  r <- floor(num_rows/b)
  
  # b_and_r <- calculate_b_bands_and_r_rows(t = threshold, n = num_rows)
  # # 
  # r <- b_and_r[["r"]]
  # b <- b_and_r[["b"]]
  #r <- ceiling(num_rows/b)
  
  threshold <- (1/b)^(1/r)
  print(paste0("LSH Threshold: ", round(threshold, 2)))
  print(paste0("LSH Bands: ", b))
  result <- list()

  data2 <- data
 
  for (band in 1:b) {
    band_start <- (band - 1) * r + 1
    band_end <- band * r
    # if(band_end > num_rows){
    #   band_end <- num_rows
    # }
    if(b == band){
      band_end <- num_rows
    }
    data2[band_start:band_end, "band"] <- band
    
  }

  data_band <- data2 %>%
    mutate(rr = 1:dim(data2)[1]) %>% 
    group_by(band) %>% 
    mutate(row_lenght = n()) %>% 
    ungroup()

  data_band_long <- data_band %>%
    pivot_longer(contains("Name"), names_to = "col")
  
  temp <- data_band_long %>% 
    left_join(data_band, by = c("rr", "band", "row_lenght")) %>% 
    select(-rr) %>%
    mutate(across(contains("Name"), ~ .x == value)) %>%
    select(-value) %>% 
    pivot_longer(contains("Name"), names_to = "col2") %>% 
    filter(value) 
  
  for(bb in 1:b){
    #result[[b]] 
    result[[bb]] <- temp %>% 
      filter(band == bb) %>%
      group_by(col, band, col2, row_lenght) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      filter(value == row_lenght) %>%
      filter(col != col2) %>% 
      rename(col1 = col)
    progress <- bb * 100 / b
    cat(sprintf("\rLSH Progress: %f%%", progress))
    flush.console()
    
  }

  result <- bind_rows(result) %>% 
    distinct() %>% 
    mutate(col1 = as.numeric(gsub("Name", "", col1)), 
           col2 = as.numeric(gsub("Name", "", col2))) %>% 
    mutate(col3 = ifelse(col1 < col2, col1, col2), 
           col4 = ifelse(col1 > col2, col1, col2)) %>% 
    select(col3, col4) %>% 
    distinct() %>% 
    rename(col1 = col3, 
           col2 = col4) %>% 
    mutate(col1 = paste0("Name", col1), 
           col2 = paste0("Name", col2))
  
  
  return(result)
}


calc_distance <- function(data, candidates, binary_vector){
  
  products <- grep("Name", colnames(binary_vector), value = TRUE)
  
  distances <- matrix(9999999, nrow = length(products), 
                      ncol = length(products))
  diag(distances) <- 0
  
  
  distances <- as.data.frame(distances)
  
  colnames(distances) <- products
  rownames(distances) <- products
  count <- 1
  candidates_count <- 1
  for(n1 in unique(candidates$col1)){
    for(n2 in unique(candidates$col2[candidates$col1 == n1])){
      if(same_shop(data, n1, n2) || 
         diff_brand(data, n1, n2)){
        next
      } else {
        candidates_count <- candidates_count+1
        vector1 <- binary_vector[, n1]
        vector2 <- binary_vector[, n2]
        
        dot_product <- sum(vector1 * vector2)
        
        # Calculate magnitudes
        magnitude_vector1 <- sqrt(sum(vector1^2))
        magnitude_vector2 <- sqrt(sum(vector2^2))
        
        # Calculate cosine similarity
        cosine_similarity <- dot_product / 
          (magnitude_vector1 * magnitude_vector2)
        
        # calculate cosine distance
        cosine_distance <- 1 - cosine_similarity
        
        distances[n1, n2] <- cosine_distance
        distances[n2, n1] <- cosine_distance
        
      }
    }
    progress <-  count * 100 / length(unique(candidates$col1))
    cat(sprintf("\rDistance Progress: %f%%", progress))
    flush.console()
    count <- count+1
  }
  
  print(paste0("Non infiniti values: ", candidates_count))
  return(distances)
  
  
  
  
}

same_shop <- function(data, a, b){
  result <- data$shop[data$Product_Name == a] == data$shop[data$Product_Name == b]
  return(result)
}

diff_brand <- function(data, a, b){
  brand_a <- data$brand[data$Product_Name == a]
  brand_b <- data$brand[data$Product_Name == b]
  if(brand_a != "unknown" && brand_b != "unknown" && 
     brand_a != brand_b){
    return(TRUE)
  }
  return(FALSE)
}

apply_LSH <- function(signature_matrix,
                      lsh_threshold){
  
  
  
  lsh_result <- calculate_lsh(signature_matrix, 
                              lsh_threshold) %>% 
    bind_rows() %>% distinct()
  
  return(lsh_result)
  
}

calculate_performance <- function(LSH_cand, real_dup){
  # check how to calculate measures
  check <- LSH_cand %>% mutate(predicted_dup = TRUE) %>% 
    left_join(real_dup, by = c("col1", "col2")) %>% 
    mutate(predicted_dup = ifelse(is.na(predicted_dup), 
                                  FALSE, predicted_dup), 
           dup = ifelse(is.na(dup), FALSE, dup)) %>% 
    mutate(indicator = ifelse(dup & predicted_dup, "TP", NA), 
           indicator = ifelse(!dup & predicted_dup, "FP", indicator), 
           indicator = ifelse(dup & !predicted_dup, "FN", indicator))
  
  
  found_dup <- unique(c(check$col1[check$indicator == "TP"], 
                        check$col2[check$indicator == "TP"]))
  
  dup_real <- unique(real_dup$col2, real_dup$col1)

  pq <- length(found_dup)/dim(LSH_cand)[1]
  pc <- length(found_dup)/length(dup_real)
  #browser()
  
  f1 <- 2*pq*pc/(pc+pq)
  
  LSH_cand <- LSH_cand %>% 
    mutate(pair_completeness = pc, 
           pair_quality = pq, 
           f1_star = f1, 
           number_comparisons = dim(LSH_cand)[1])
  
  return(LSH_cand)
  
}


# hashing_bands <- function(data, lsh_table){
#   
#   result <- data %>% 
#     pivot_longer(cols = starts_with("Name"), 
#                  values_to = "value", 
#                  names_to = "Product") %>% 
#     mutate(value = if_else(is.infinite(value), 3, value)) %>% 
#     group_by(value) %>% 
#     summarise(products = paste(Product, collapse = ";"), 
#               .groups = "drop") %>% 
#     ungroup() %>% 
#     mutate(value = value + 1)
#   for(v in unique(result$value)){
#     lsh_table[[v]] <- paste(result$products[result$value == v], 
#                             lsh_table[[v]], sep = ";")
#   }
#   
#   return(lsh_table)
# }

cluster_calc <- function(data, LSH_cand, binary_vector, 
                         real_dup){
  

  distances <- calc_distance(data, LSH_cand, binary_vector)
  
  f1_best <- 0
  method_best <- NA
  h_best <- NA
  for(mt in c("complete", "single")){
    hc <- hclust(as.dist(distances), method = mt)
    for(h in seq(0, 2, by = 0.01)){
      cutree_result <- cutree(hc, h = h) 
      
      a <- tibble(name = colnames(distances), 
                  cluster =  cutree_result)
      a_dup <- list()
      if(max(cutree_result) == length(data$Product_Name)){
        next
      }
      
      for(prod in unique(a$name)){
        id <- a$cluster[a$name == prod]
        col2 <- a$name[a$cluster == id]
        col2 <- setdiff(col2, prod)
        a_dup[[prod]] <- tibble(col2 = col2, 
                                col1 = prod)
      }
      
      a_dup <- bind_rows(a_dup) %>% 
        mutate(pred_dup = TRUE)
      check <- a_dup %>% 
        left_join(real_dup, by = c("col2", "col1")) %>% 
        mutate(dup = ifelse(is.na(dup), FALSE, dup)) %>% 
        mutate(indicator = ifelse(dup, "TP", "FP"))
      
      tp <- unique(c(check$col1[check$indicator == "TP"], 
                            check$col2[check$indicator == "TP"]))
      fp <- unique(c(check$col1[check$indicator == "FP"], 
                                      check$col2[check$indicator == "FP"]))
      
      tp <- length(tp)
      fp <- length(fp)

      
      fn <- real_dup %>% 
        anti_join(a_dup, by = c("col2", "col1"))
      
      fn <- unique(c(fn$col1, 
                     fn$col2))

      fn <- length(fn)
      
      recall <- tp/(tp+fn)
      precision <- tp/(tp+fp)
      
      f1 <- 2*precision*recall/(recall + precision)
      if(is.nan(f1)){
        next
      }
      if(f1_best < f1){
        f1_best <- f1
        method_best <- mt
        h_best <- h
        
      }
    }
  }
  
  results <- list()
  results[["f1"]] <- f1_best
  results[["method"]] <- method_best
  results[["h"]] <- h_best
  
  return(results)
}



cluster_calc_test <- function(data, LSH_cand, binary_vector, method, h,
                              real_dup){
  
  
  distances <- calc_distance(data, LSH_cand, binary_vector)
  hc <- hclust(as.dist(distances), method = method)
  f1_best <- 0
  #for(h in seq(0.5, 1, by = 0.01)){
    cutree_result <- cutree(hc, h = h) 
    
    a <- tibble(name = colnames(distances), 
                cluster =  cutree_result) %>% 
      group_by(cluster) %>% 
      filter(n() > 1) %>% 
      ungroup()
    a_dup <- list()
    
    for(prod in unique(a$name)){
      id <- a$cluster[a$name == prod]
      col2 <- a$name[a$cluster == id]
      col2 <- setdiff(col2, prod)
      a_dup[[prod]] <- tibble(col2 = col2, 
                              col1 = prod)
    }
    
    a_dup <- bind_rows(a_dup) %>% 
      mutate(pred_dup = TRUE)
    if(length(a_dup$pred_dup) == 0){
      next
    }
    #browser()
    check <- a_dup %>% 
      left_join(real_dup, by = c("col2", "col1")) %>% 
      mutate(dup = ifelse(is.na(dup), FALSE, dup)) %>% 
      mutate(indicator = ifelse(dup, "TP", "FP"))
    
    tp <- unique(c(check$col1[check$indicator == "TP"], 
                   check$col2[check$indicator == "TP"]))
    fp <- unique(c(check$col1[check$indicator == "FP"], 
                   check$col2[check$indicator == "FP"]))
    
    tp <- length(tp)
    fp <- length(fp)
    
    
    fn <- real_dup %>% 
      anti_join(a_dup, by = c("col2", "col1"))
    
    fn <- unique(c(fn$col1, 
                   fn$col2))
    
    fn <- length(fn)
    
    recall <- tp/(tp+fn)
    precision <- tp/(tp+fp)
    
    f1 <- 2*precision*recall/(recall + precision)
    if(is.nan(f1)){
      f1 <- NA
    }
    if(f1_best < f1){
      f1_best <- f1
      
    }
  #}
  
  return(f1)
}

