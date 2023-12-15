## computer science class

remove(list = ls())
library(jsonlite)
library(dplyr)
library(tidyverse)
library(stringr)
library("textreuse")
library(pracma)
library(stringdist)
library(matlab)
set.seed(777)

source("functions.R")

dir1 <- "intermediate/"
dir2 <- "/output/"


data <- fromJSON("TVs-all-merged 2.json", simplifyVector = FALSE)

data2 <- list()
features <- list()
brands <- c()
prod <- 1
all_product_names <- c()
all_col <- c()
#featuresMap <- list()
for (nm in 1:length(data)){
  temp <- list()
  temp_fearures <- list()
  for(i in 1:length(data[[nm]])){
    a <- data[[nm]][[i]]
    features[[paste0(nm,";",  i)]] <- bind_rows(a[["featuresMap"]])
    if(!is.null(a[["featuresMap"]][["Brand"]])){
      brand_a <- a[["featuresMap"]][["Brand"]]
      brands <- c(brands, brand_a)
      a[["brand"]] <- brand_a
    }
    for(tt in c("Brightness", "Size",  
                "Weight", "Refresh", "Color", "UPC", "Type", 
                 "HDMI", "Resolution", "Ratio")){
      if(any(grepl(tt, colnames(features[[paste0(nm,";",  i)]])))){
        col <- grep(tt, colnames(features[[paste0(nm,";",  i)]]), 
                    value = TRUE)
        all_col <- unique(c(all_col, col))
        for(l in 1:length(col)){
          value <- a[["featuresMap"]][[col[l]]]
          a[[paste0(tt, l)]] <- value
        }
      }
    }
    
    a[["featuresMap"]] <- NULL
    data2[[paste0(nm,";",  i)]] <- a %>% bind_rows() %>% 
      mutate(Product_Name = paste0("Name", prod))
    all_product_names <- c(all_product_names, paste0("Name", prod))
    prod <- prod + 1
    
  }
}
data2 <- data2 %>% bind_rows()

data2 <- data_prep(data2, brands)

data_dup <- data2 %>% 
  group_by(modelID) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  select(Product_Name, modelID)

data_non_dup <- data2 %>% 
  group_by(modelID) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  select(Product_Name, modelID)

bootstrap_samples <- 5 # Number of bootstrap samples

lsh_test_results <- list()
lsh_train_results <- list()

for(nr_sample in 1:bootstrap_samples){
  

  #sampling train and test data
  training_product <- unique(sample(data2$Product_Name, replace = TRUE))
  test_product <- setdiff(data2$Product_Name, training_product)
  
  # cleaning train data, creating binary vector and signature matrix
  data_train <- data2 %>% filter(Product_Name %in% training_product)
  binary_vector_train <- finding_binary_vector(data_train, brands)
  sig_matrix_train <- minhash_signature(binary_vector_train)
  
  # cleaning test data, creating binary vector and signature matrix
  data_test <- data2 %>% filter(Product_Name %in% test_product)
  binary_vector_test <- finding_binary_vector(data_test, brands)
  sig_matrix_test <- minhash_signature(binary_vector_test)
  
  
  # for TRAIN: creating matrix which indicates all duplicate combinations 
  tmp <- data_train %>% 
    group_by(modelID) %>% 
    filter(n() > 1) %>% 
    ungroup()
  
  dup_train <- list()
  
  for(prod in unique(tmp$Product_Name)){
    modID <- data_train$modelID[data_train$Product_Name == prod]
    col2 <- data_train$Product_Name[data_train$modelID == modID]
    col2 <- setdiff(col2, prod)
    dup_train[[prod]] <- tibble(col2 = col2, 
                                col1 = prod)
  }
  
  dup_train <- bind_rows(dup_train) %>% 
    mutate(dup = TRUE)
  
  # for TEST: creating matrix which indicates all duplicate combinations 
  tmp <- data_test %>% 
    group_by(modelID) %>% 
    filter(n() > 1) %>% 
    ungroup()
  
  dup_test <- list()
  
  for(prod in unique(tmp$Product_Name)){
    modID <- data_test$modelID[data_test$Product_Name == prod]
    col2 <- data_test$Product_Name[data_test$modelID == modID]
    col2 <- setdiff(col2, prod)
    dup_test[[prod]] <- tibble(col2 = col2, 
                                col1 = prod)
  }
  
  dup_test <- bind_rows(dup_test) %>% 
    mutate(dup = TRUE)
  
  rm(tmp)
  
  test_comb <- choose(dim(data_test)[1], 2)
  train_comb <- choose(dim(data_train)[1], 2) 
 
  bands <- 2
  
  # for different threshold t we perform LSH, once we have the candidates, we
  # calculate cosine distance and for non candidates we set distance to very 
  # large number. If I set it to Inf then hclust() does not work
  
  
  for(bands in c(unique(floor(50/1:50)), 14, 15, 20, 13, 11, 18)){
    print("TRAIN")
    train_LSH_candidates <- calculate_lsh(sig_matrix_train, 
                                          bands) %>% 
      bind_rows() %>% distinct() %>% 
      calculate_performance(real_dup = dup_train)
    
    cluster_results <- cluster_calc(data_train, 
                                    train_LSH_candidates, 
                                    binary_vector_train, 
                                    dup_train)
    
    
    lsh_train_results[[paste0(bands, ",", nr_sample)]] <- 
      tibble(f1_star = unique(train_LSH_candidates$f1_star), 
             pc = unique(train_LSH_candidates$pair_completeness), 
             pq = unique(train_LSH_candidates$pair_quality), 
             nr_comparisons = unique(train_LSH_candidates$number_comparisons), 
             tot_comb = train_comb,
             f1 = cluster_results[["f1"]], 
             band = bands, 
             sample = nr_sample)
    
    
    print("TEST")
    test_LSH_candidates <- calculate_lsh(sig_matrix_test, 
                                         bands) %>% 
      bind_rows() %>% distinct() %>% 
      calculate_performance(real_dup = dup_test)
    

    cluster_results_test <- cluster_calc_test(data_test, 
                                         test_LSH_candidates, 
                                         binary_vector_test, 
                                         method = cluster_results[["method"]],
                                         h = cluster_results[["h"]],
                                         dup_test)
    
    lsh_test_results[[paste0(bands, ",", nr_sample)]] <- 
      tibble(f1_star = unique(test_LSH_candidates$f1_star), 
             pc = unique(test_LSH_candidates$pair_completeness), 
             pq = unique(test_LSH_candidates$pair_quality), 
             nr_comparisons = unique(test_LSH_candidates$number_comparisons), 
             tot_comb = test_comb,
             f1 = cluster_results_test, 
             band = bands,  
             sample = nr_sample, 
             h = cluster_results[["h"]], 
             method = cluster_results[["method"]])
    
    
    
  }
  write.csv(bind_rows(lsh_test_results), 
            paste0("test", nr_sample, ".csv"), row.names = F)
  write.csv(bind_rows(lsh_train_results), 
            paste0("train", nr_sample, ".csv"), row.names = F)
}

lsh_train_results2 <- bind_rows(lsh_train_results)
write.csv(lsh_train_results2, "train_all.csv", row.names = F)
lsh_test_results2 <- bind_rows(lsh_test_results)
write.csv(lsh_test_results2, "test_all.csv", row.names = F)

train <- read.csv(paste0(dir1, "train_all.csv")) %>% 
  mutate(fraction = nr_comparisons/tot_comb) %>% 
  group_by(band) %>% 
  summarise(fraction = mean(fraction), 
            pq = mean(pq), 
            pc = mean(pc), 
            f1_star = mean(f1_star), 
            f1 = mean(f1), .groups = "drop") %>% 
  group_by(fraction) %>% 
  summarise(pq = mean(pq), 
            pc = mean(pc), 
            f1_star = mean(f1_star), 
            f1 = mean(f1), .groups = "drop")
  


test <- read.csv("test_all.csv") %>% 
  mutate(fraction = nr_comparisons/tot_comb) %>% 
  group_by(band) %>% 
  summarise(fraction = mean(fraction), 
            pq = mean(pq), 
            pc = mean(pc), 
            f1_star = mean(f1_star), 
            f1 = mean(f1), .groups = "drop") %>% 
  group_by(fraction) %>% 
  summarise(pq = mean(pq), 
            pc = mean(pc), 
            f1_star = mean(f1_star), 
            f1 = mean(f1), .groups = "drop") %>% 
  mutate(diff = f1 - f1_star)


ggplot() + 
  geom_line(data = test, aes(x = fraction, y = f1), color = "black") +
  xlab('Fraction of comparisons') +
  ylab('F1-measure')

ggplot() + 
  geom_line(data = test, aes(x = fraction, y = f1_star), color = "black") +
  xlab('Fraction of comparisons') +
  ylab('F1*-measure')

ggplot() + 
  geom_line(data = test, aes(x = fraction, y = pq), color = "black") +
  xlab('Fraction of comparisons') +
  ylab('Pair quality')

ggplot() + 
  geom_line(data = test, aes(x = fraction, y = pc), color = "black") +
  xlab('Fraction of comparisons') +
  ylab('Pair completenes')




ggplot() + 
  geom_line(data = train, aes(x = fraction, y = f1), color = "black") +
  xlab('Fraction of comparisons') +
  ylab('F1-measure')

ggplot() + 
  geom_line(data = train, aes(x = fraction, y = f1_star), color = "black") +
  xlab('Fraction of comparisons') +
  ylab('F1*-measure')

ggplot() + 
  geom_line(data = train, aes(x = fraction, y = pq), color = "black") +
  xlab('Fraction of comparisons') +
  ylab('Pair quality')

ggplot() + 
  geom_line(data = train, aes(x = fraction, y = pc), color = "black") +
  xlab('Fraction of comparisons') +
  ylab('Pair completenes')







