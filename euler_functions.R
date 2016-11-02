sumDigits = function(aBigz) {
  require(gmp)
  sum(sapply(strsplit(as.character(aBigz), NULL), as.numeric)) 
}

findFactors = function(n) {
  # Includes 1 but not n
  ints = seq_len(n - 1)
  ints[!(n %% ints)]
}

# Inefficient:
isprime = function(x) length(findFactors(x)) == 1

mulitOrder = function(a, n, prec = 200) {
  require(Rmpfr)
  # Based on Wikipedia's multiplicative order page
  i = 1
  while (TRUE) {
    if(mpfr(a, prec)^i %% n == 1)
      break
    mpfr(a, prec)^i %% n
    (i = i + 1)
  }
  i
}

bitsNeeded = function(digits) digits / log10(2)
