
# install.packages('openssl)
library(openssl)

if('rsa_sign' %in% ls('package:openssl')) {
  print('openssl package loaded correctly')
} else {
  print('openssl package is not loaded correctly')
}

# function to generate a key pair
generate_key_pair <- function() {
  rsa_keygen(bits = 2048) # generate a 2048-bit RSA key pair
}

# function to sign data
sign_data <- function(private_key, data) {
  signature <- as.character(rsa_sign(private_key, charToRaw(data), 'sha256'))
  return(signature)
}

# function to verify a signature
verify_signature <- function(public_key, data, signature) {
  tryCatch({
    rsa_verify(public_key, charToRaw(data), base64_decode(signature), 'sha256')
  }, error = function(e) {
    return(FALSE)
  })
}

# define a block structure
create_block <- function(index, previous_hash, timestamp, data, hash, nonce = 0) {
  list(
    index = index,
    previous_hash = previous_hash,
    timestamp = timestamp,
    data = data,
    hash = hash,
    nonce = nonce # added for proof-of-work! 
  )
}

# function to calculate the hash of a block (using SHA-256)
calculate_hash <- function(index, previous_hash, timestamp, data, nonce = 0, signature = '') {
  input_string <- paste0(index, previous_hash, timestamp, data, nonce, signature)
  digest::digest(input_string, algo = 'sha256')
}

# function to create a genesis block
create_genesis_block <- function() {
  index <- 0
  previous_hash <- '0' # ... or some arbitrary initial hash
  timestamp <- as.character(Sys.time())
  data <- 'genesis block'
  nonce <- 0 # initial nonce
  hash <- calculate_hash(index, previous_hash, timestamp, data, nonce)
  create_block(index, previous_hash, timestamp, data, hash, nonce)
}

# function to create a new block (with proof-of-work!)
create_new_block <- function(previous_block, data, private_key, difficulty = 4)  { # added a difficulty parameter & signature!
  index <- previous_block$index + 1
  previous_hash <- previous_block$hash
  timestamp <- as.character(Sys.time())
  nonce <- 0
  signature <- sign_data(private_key, paste0(index, previous_hash, timestamp, data, nonce)) # sign the block data
  repeat {
    hash <- calculate_hash(index, previous_hash, timestamp, data, nonce, signature) # include signature in hash
    if(substr(hash, 1, difficulty) == paste(rep('0', difficulty), collapse = '')) { # proof-of-work condition
      break
    }
    nonce <- nonce + 1
  }
  create_block(index, previous_hash, timestamp, data, hash, nonce, signature)
}

# function to check if a block is valid
is_valid_block <- function(new_block, previous_block, difficulty = 4) { # added difficulty parameter
  if (previous_block$index + 1 != new_block$index) {
    return(FALSE)
  }
  if (previous_block$hash != new_block$previous_hash) {
    return(FALSE)
  }
  
  calculated_hash <- calculate_hash(
    new_block$index, 
    new_block$previous_hash, 
    new_block$timestamp, 
    new_block$data, 
    new_block$nonce)
  
  if (calculated_hash != new_block$hash) {
    return(FALSE)
  }
  if (substr(new_block$hash, 1, difficulty) != paste(rep('0', difficulty), collapse = '')) { # proof-of-work verification
    return(FALSE)
  } 
  if (!verify_signature(public_key, paste0(
    new_block$index, 
    new_block$previous_hash, 
    new_block$timestamp, 
    new_block$data,
    new_block$nonce), new_block$signature)) {
    return(FALSE)
  }
  return (TRUE)
}

# function to check if the entire blockchain is valid
is_valid_chain <- function(chain, public_key, difficulty = 4) { # added difficulty parameter
  if(length(chain) <= 1) {
    return(TRUE) # genesis block is always valid
  }
  for(i in 2:length(chain)) {
    if(!is_valid_block(chain[[i]], chain[[i - 1]], public_key, difficulty)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# example usage
key_pair <- generate_key_pair()
private_key <- key_pair
public_key <- key_pair$pubkey

# initialise the blockchain
blockchain <- list(create_genesis_block())

# add some blocks
blockchain[[2]] <- create_new_block(blockchain[[1]], 'transaction 1', private_key)
blockchain[[3]] <- create_new_block(blockchain[[2]], 'transaction 2', private_key)
blockchain[[4]] <- create_new_block(blockchain[[3]], 'transaction 3', private_key)

# print the blockchain
print(blockchain)

# check if the blockchain is valid
print(paste('Blockchain is valid:', is_valid_chain(blockchain, public_key)))
# how is this supposed to help with accounting?

# example of tampering with a block (for demonstration)
tamper_with_block <- function(chain, block_index, new_data) {
  if(block_index > length(chain) || block_index < 1){
    print('invalid block index')
    return(chain)
  }
  chain[[block_index]]$data <- new_data
  
  index <- chain[[block_index]]$index 
  previous_hash <- chain[[block_index]]$previous_hash
  timestamp <- chain[[block_index]]$timestamp
  data <- chain[[block_index]]$data
  #difficulty = nchar(gsub('0', '', substr(chain[[block_index]]$hash, 1, nchar(chain[[block_index]]$hash))))
  
  # calculate the number of leading zeros (difficulty)
  difficulty <- 0
  for(i in 1:nchar(chain[[block_index]]$hash)) {
    if(substr(chain[[block_index]]$hash, i, i) == '0') {
      difficulty <- difficulty + 1
    } else {
      break
    }
  }
  
  nonce = 0;
  repeat {
    hash <- calculate_hash(index, previous_hash, timestamp, data, nonce)
    if(substr(hash, 1, difficulty) == paste(rep('0', difficulty), collapse = '')) {
      break
    }
    nonce <- nonce + 1
  }
  chain[[block_index]]$hash <- hash
  chain[[block_index]]$nonce <- nonce
  
  # recalculate hashes for subsequent blocks
  for(i in (block_index + 1):length(chain)){
    chain[[i]] <- create_new_block(chain[[i-1]], chain[[i]]$data);
  }
  return(chain)
}

blockchain <- tamper_with_block(blockchain, 2, 'Tampered Data')

print(paste('blockchain valid after tampering:', is_valid_chain(blockchain)))

print(blockchain)

#### github ####

# https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r

library(usethis)

use_git_config(user.name = 'pohjanlaakso', user.email = 'pohjanlaakso@gmail.com')


####

# adding a digital copy to over a distributed network.










