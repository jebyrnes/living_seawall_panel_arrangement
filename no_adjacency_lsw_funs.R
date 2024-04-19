#' -----------------------------------------------------
#' A series of functions to make a blocked design for 
#' a hexagonal panel treatment arrangement so that
#' no two panels touching have the same treatment AND
#' each row has one panel of each type with randomization
#' within rows
#' 
#' @author Jarrett Byrnes
#' -----------------------------------------------------

make_wall <- function(n_row, n_col, n_block, stopping = 100){
  blocks <- vector("list", n_block)
  blocks[[1]] <- make_block(n_row, n_col)
  
  # just in case
  if(n_block==1) return(blocks[[1]])
  

  
  for(b in 2:n_block){
    
    # updown for join of blocks
    # we need to alternate which test we use to see if adjacent
    # panels match depending on where we are in the blocks
    ud <- c("u", "d")[b %% 2 + 1]
    
    try_again <- TRUE
    tries <- 0
    
    while(try_again){
      # let's not let this go on forever
      tries <- tries+1
      if(tries>stopping) stop("Failed to find solution. Try again?")
      
      # make a new block and compare it to the last one
      candidate_block <- make_block(n_row, n_col)
      
      block_start <- candidate_block[,1] #start of new block
      block_end <- blocks[[b-1]][,n_col] #of old block
      
      ## need to correct for alternating of up and down to 
      ## next block FIXME- otherwise some failures produced sometimes
      ## I think 
      
      if(!(0 %in% (block_start - block_end) |
          (ud=="d" & 0 %in% (shift(block_start, 1) - block_end)) | 
         (ud == "u" &  0 %in% (block_start - shift(block_end, 1))) # don't need if up-down 
      )) try_again <- FALSE
      
    }
    
    blocks[[b]] <- candidate_block
  }
  
  # return
  return(blocks)
}


make_block <- function(n_row, n_col){
  max_dim <- max(n_row, n_col)
  
  wall <- matrix(rep(NA, n_row*n_col), nrow=n_row)
  fakewall <- matrix(rep(NA, max_dim^2), nrow=max_dim) #need this for gregor algorithm
  neighborhood <- gregor(fakewall)
  
  for(i in 1:n_row){ #row
    
    #treatment vector we restart each row
    trts <- shuffle(1:n_col)
    for(j in 1:n_col){ #col
      
      # if this was a vector, what's the index
      neighborhood_col_idx <- (j - 1) * max_dim + i
      #neighborhood_col_idx <- (i - 1) * max_dim + j
      
      #get surrounding neighborhood values
      surrounding_values <- neighborhood[,neighborhood_col_idx]
      possible_assignments <- trts[!(trts %in% surrounding_values)]
      
      
      if(length(possible_assignments)==0) {
        # print(wall)
        # print(fakewall)
        # print(neighborhood)
        # print(neighborhood_col_idx)
        # print(trts)
        # print(possible_assignments)
        return(make_block(n_row, n_col)) #failed this time, so keep trying
      }
      # add a shuffle?
      assign_idx <- 1
      
      # write the wall, new fakewall, 
      # increment the neighborhood, and increment treatments left
      wall[i,j] <- possible_assignments[assign_idx]
      fakewall[i,j] <- possible_assignments[assign_idx]
      neighborhood <- gregor(fakewall)
      trts <- trts[trts!=possible_assignments[assign_idx]]
      
    }
    
  }
  
  #return the wall
  wall
  
}

# from https://stackoverflow.com/questions/29105175/find-neighbouring-elements-of-a-matrix-in-r?newreg=3612709e3dcb48f5b38f64dcbfcec99b
# answer
gregor = function(mat) {
  n = nrow(mat)
  mat.pad = rbind(NA, cbind(NA, mat, NA), NA)
  ind = 2:(n + 1) # row/column indices of the "middle"
  neigh = rbind(N  = as.vector(mat.pad[ind - 1, ind    ]),
                NE = as.vector(mat.pad[ind - 1, ind + 1]),
                E  = as.vector(mat.pad[ind    , ind + 1]),
                SE = as.vector(mat.pad[ind + 1, ind + 1]),
                S  = as.vector(mat.pad[ind + 1, ind    ]),
                SW = as.vector(mat.pad[ind + 1, ind - 1]),
                W  = as.vector(mat.pad[ind    , ind - 1]),
                NW = as.vector(mat.pad[ind - 1, ind - 1]))
  return(neigh)
}

shuffle <- function(vec){
  sample(vec, length(vec), replace = FALSE)
}

# https://stackoverflow.com/questions/26997586/shifting-a-vector
#' function that shifts vector values to right or left
#'
#' @param x Vector for which to shift values
#' @param n Number of places to be shifted.
#'    Positive numbers will shift to the right by default.
#'    Negative numbers will shift to the left by default.
#'    The direction can be inverted by the invert parameter.
#' @param invert Whether or not the default shift directions
#'    should be inverted.
#' @param default The value that should be inserted by default.

shift <- function(x, n, invert=FALSE, default=NA){
  stopifnot(length(x)>=n)
  if(n==0){
    return(x)
  }
  n <- ifelse(invert, n*(-1), n)
  if(n<0){
    n <- abs(n)
    forward=FALSE
  }else{
    forward=TRUE
  }
  if(forward){
    return(c(rep(default, n), x[seq_len(length(x)-n)]))
  }
  if(!forward){
    return(c(x[seq_len(length(x)-n)+n], rep(default, n)))
  }
}



# put it all together

make_layout_no_adjacency <-  function(blocks = blocks,
                                      heights = heights,
                                      img = img,
                                      stopping = 100) {
    
    treatments <- length(img)
    
    d <- make_wall(heights, treatments, blocks, stopping = stopping)
    
    if(class(d)[1] == "matrix"){
      d <- d
    }else{
      d <- do.call(cbind, d)
    }
    
    d |> 
      as_tibble() |>
      mutate(height = 1:n()) |>
      pivot_longer(-height) |>
      mutate(name = gsub("V", "", name) |> as.numeric()) |>
      rename(column = name) |>
      #add block and img
      mutate(block = rep(1:blocks, treatments) |> sort() |> rep(heights),
             img = img[value])|>
      arrange(height, block) |>
      mutate(panel = 1:n())
    
    
  }

# function to check things

get_row_block <- function(g, treatments, blocks){
  st_centroid(g) |> 
    st_coordinates() |>
    as_tibble() |>
    mutate(id = g$id,
           height = safe_floor(Y)+1) |>
    arrange(height, X) |>
    mutate(block = rep(1:blocks, treatments) |> 
             sort() |> rep(max(height)), 
           name = 1:n()) 
  
  
}

# floor function that deals with the problem of floating
safe_floor <- function(x, tolerance = 1e-10) {
  if (is.integer(x)) {
    return(x)
  } else {
    return(trunc(x + tolerance))
  }
}

