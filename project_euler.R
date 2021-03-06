library(tidyverse)
library(gmp)  # as.bigz
source("euler_functions.R")

# 1
threes = seq(3, 999, by = 3)
fives = seq(5, 999, by = 5)
sum(unique(c(threes, fives)))

# 2
fib = integer(4e6)
fib[1:2] = 1:2
i = 3
while(TRUE) {
  x = sum(fib[c(i-2, i-1)])
  cat(x)
  if(x > 4e6)
    break
  fib[i] = x
  i = i + 1
}
sum(fib[fib %% 2 == 0])

# 3
two_factors = function(x) {
  if(x == 2) 
    return(x)
  factors = integer()
  exit = FALSE
  i = 2
  while(TRUE) {
    if(i %% 1000 == 0)
      cat('on', i, '\n')
    if(x %% i == 0) {
      factors[length(factors) + 1] = i
      factors[length(factors) + 1] = x / i
      break
    }
    if(exit)
      return(factors)
    i = i + 1
    if(i >= x - 1)
      break
  }
  if(length(factors))
    return(factors)
  x
}

prime_factors = function(y) {
  while(TRUE) {
    last_y = y
    y = unlist(lapply(y, two_factors))
    if(is.logical(all.equal(y, last_y)))
      break
  }
  return(y)
}

# 4
d3_factors = function(n, d3 = 999:100) {
  for(i in d3)
    if((n / i) %in% d3) {
      cat('found it. n =', n, 'i =', i)
      return(TRUE)
    }
  return(FALSE)
}

for(n in 999^2:100^2) {
  if(n != lapply(strsplit(as.character(n), NULL), function(l) paste(rev(l), collapse = "")))
    next
  # Only here if n is palendrome
  cat('trying', n, '\n')
  if(d3_factors(n))
    break
}

# 5
divisble_1to20 = function(n) {
  for(i in 20:11) {  # Each of the single digit numbers is tested by a larger multiple
    if((n %% i) != 0)
      return(FALSE)
  }    
  return(TRUE)
}

test = FALSE
i = 0
primes = c(2, 3, 5, 7, 11, 13, 17)
inc = prod(primes)
while(!test) {
  i = i + inc
  cat(i, '\t')
  test = divisble_1to20(i)
}
i

# 6
ss = function(n)
  sum(n)^2 - sum(n^2)
ss(1:100)

# 7
i = 1
n = 3
while(TRUE) {
  if(isprime(n)) {
    i = i + 1
  }
  if(i == 10001)
    break
  n = n + 2
}
cat('Prime number', i, 'is', n, '\n')

# 8
d = gsub('\\s', '', '73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450')
max = 0
for(i in 1:(nchar(d) - 13)) {
  x = prod(as.integer(unlist(strsplit(substr(d, i, i+12), NULL))))
  if(x > max)
    max = x
}
max

# 9
sp_path = function() {
  for(a in 1:1e3) 
    for(b in a:1e3)
      if(a + b + sqrt(a^2 + b^2) == 1000)
        return(c(a = a, b = b))
}
x = sp_path()
sqrt(sum(x^2)) * prod(x)

# 10
# Had a faster implementation of isprime, not sure where it came from. Wasn't mine.
sum_of_primes = function(set)
  sum(as.numeric(set[sapply(set, isprime)]))
sum_of_primes(1:2e6)

# 11
d = as.integer(unlist(strsplit('08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
                    49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
                    81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
                    52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
                    22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
                    24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
                    32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
                    67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
                    24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
                    21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
                    78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
                    16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
                    86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
                    19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
                    04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
                    88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
                    04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
                    20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
                    20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
                    01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48',
                               '\\s+')))
m = matrix(d, nrow = 20, byrow = TRUE)
max = 0

for(r in 1:nrow(m))
  for(c in 1:(ncol(m) - 3)) {
    cat('r =', r, 'c =', c, '\n')
    x = prod(m[r, c:(c+3)])
    if(x > max)
      max = x
  }

for(r in 1:(nrow(m) - 3))
  for(c in 1:(ncol(m))) {
    x = prod(m[r:(r+3), c])
    if(x > max) {
      max = x
      loc = c(r, c)
      
    }
  }

for(r in 1:(nrow(m) - 3))
  for(c in 1:(ncol(m) - 3)) {
    cat('r =', r, 'c =', c, '\n')
    x = prod(diag(m[r:(r+3), c:(c+3)]))
    if(x > max)
      max = x
  }

for(r in 4:nrow(m))
  for(c in 1:(ncol(m) - 3)) {
    cat('r =', r, 'c =', c, '\n')
    x = prod(sapply(0:3, function(i) m[r - i, c + i]))
    if(x > max)
      max = x
  }
max

# 12
library(gmp)
num_div = function(x, want_len = TRUE) {
  # Only works for numbers with at least two divisors besides self and one
  primes = factorize(x)
  div = c(1, unique(unlist(
    lapply(1:length(primes), function(n) 
      apply(combn(as.integer(primes), n), 2, prod))
  )))
  if(want_len)
    return(length(div))
  div
}

while(TRUE) {
  i = i + 1
  tri = sum(tri + i)
  ndiv = num_div(tri)
  cat('tri number', i, 'is', tri, 'and has', ndiv, 'divisors.\n')
  if(ndiv > 500)
    break
}

# 13
d = strsplit(c('37107287533902102798797998220837590246510135740250
                         46376937677490009712648124896970078050417018260538
                         74324986199524741059474233309513058123726617309629
                         91942213363574161572522430563301811072406154908250
                         23067588207539346171171980310421047513778063246676
                         89261670696623633820136378418383684178734361726757
                         28112879812849979408065481931592621691275889832738
                         44274228917432520321923589422876796487670272189318
                         47451445736001306439091167216856844588711603153276
                         70386486105843025439939619828917593665686757934951
                         62176457141856560629502157223196586755079324193331
                         64906352462741904929101432445813822663347944758178
                         92575867718337217661963751590579239728245598838407
                         58203565325359399008402633568948830189458628227828
                         80181199384826282014278194139940567587151170094390
                         35398664372827112653829987240784473053190104293586
                         86515506006295864861532075273371959191420517255829
                         71693888707715466499115593487603532921714970056938
                         54370070576826684624621495650076471787294438377604
                         53282654108756828443191190634694037855217779295145
                         36123272525000296071075082563815656710885258350721
                         45876576172410976447339110607218265236877223636045
                         17423706905851860660448207621209813287860733969412
                         81142660418086830619328460811191061556940512689692
                         51934325451728388641918047049293215058642563049483
                         62467221648435076201727918039944693004732956340691
                         15732444386908125794514089057706229429197107928209
                         55037687525678773091862540744969844508330393682126
                         18336384825330154686196124348767681297534375946515
                         80386287592878490201521685554828717201219257766954
                         78182833757993103614740356856449095527097864797581
                         16726320100436897842553539920931837441497806860984
                         48403098129077791799088218795327364475675590848030
                         87086987551392711854517078544161852424320693150332
                         59959406895756536782107074926966537676326235447210
                         69793950679652694742597709739166693763042633987085
                         41052684708299085211399427365734116182760315001271
                         65378607361501080857009149939512557028198746004375
                         35829035317434717326932123578154982629742552737307
                         94953759765105305946966067683156574377167401875275
                         88902802571733229619176668713819931811048770190271
                         25267680276078003013678680992525463401061632866526
                         36270218540497705585629946580636237993140746255962
                         24074486908231174977792365466257246923322810917141
                         91430288197103288597806669760892938638285025333403
                         34413065578016127815921815005561868836468420090470
                         23053081172816430487623791969842487255036638784583
                         11487696932154902810424020138335124462181441773470
                         63783299490636259666498587618221225225512486764533
                         67720186971698544312419572409913959008952310058822
                         95548255300263520781532296796249481641953868218774
                         76085327132285723110424803456124867697064507995236
                         37774242535411291684276865538926205024910326572967
                         23701913275725675285653248258265463092207058596522
                         29798860272258331913126375147341994889534765745501
                         18495701454879288984856827726077713721403798879715
                         38298203783031473527721580348144513491373226651381
                         34829543829199918180278916522431027392251122869539
                         40957953066405232632538044100059654939159879593635
                         29746152185502371307642255121183693803580388584903
                         41698116222072977186158236678424689157993532961922
                         62467957194401269043877107275048102390895523597457
                         23189706772547915061505504953922979530901129967519
                         86188088225875314529584099251203829009407770775672
                         11306739708304724483816533873502340845647058077308
                         82959174767140363198008187129011875491310547126581
                         97623331044818386269515456334926366572897563400500
                         42846280183517070527831839425882145521227251250327
                         55121603546981200581762165212827652751691296897789
                         32238195734329339946437501907836945765883352399886
                         75506164965184775180738168837861091527357929701337
                         62177842752192623401942399639168044983993173312731
                         32924185707147349566916674687634660915035914677504
                         99518671430235219628894890102423325116913619626622
                         73267460800591547471830798392868535206946944540724
                         76841822524674417161514036427982273348055556214818
                         97142617910342598647204516893989422179826088076852
                         87783646182799346313767754307809363333018982642090
                         10848802521674670883215120185883543223812876952786
                         71329612474782464538636993009049310363619763878039
                         62184073572399794223406235393808339651327408011116
                         66627891981488087797941876876144230030984490851411
                         60661826293682836764744779239180335110989069790714
                         85786944089552990653640447425576083659976645795096
                         66024396409905389607120198219976047599490197230297
                         64913982680032973156037120041377903785566085089252
                         16730939319872750275468906903707539413042652315011
                         94809377245048795150954100921645863754710598436791
                         78639167021187492431995700641917969777599028300699
                         15368713711936614952811305876380278410754449733078
                         40789923115535562561142322423255033685442488917353
                         44889911501440648020369068063960672322193204149535
                         41503128880339536053299340368006977710650566631954
                         81234880673210146739058568557934581403627822703280
                         82616570773948327592232845941706525094512325230608
                         22918802058777319719839450180888072429661980811197
                         77158542502016545090413245809786882778948721859617
                         72107838435069186155435662884062257473692284509516
                         20849603980134001723930671666823555245252804609722
                         53503534226472524250874054075591789781264330331690'),
             '\\s+')
d12 = substr(d[[1]], 1, 12)
substr(print(sum(as.numeric(d12)), digits = 10), 1, 10)

# 14
make_chain = function(n) {
  i = 1
  while(n != 1) {
    i = i + 1    
    if(n %% 2)
      n = 3 * n + 1 else
        n = n / 2
  }
  i
}

odds = seq(1, 1e6, 2)
system.time({lengths = sapply(odds, make_chain)})

# 15
n=2
grid_paths = function(n) {
  n = n + 1  # calculations are on intersections, not segments
  m = matrix(nrow = n, ncol = n)
  m[, 1] = 1
  m[1, ] = 1
  for(i in 2:n)
    for(j in 2:n)
      m[i, j] = m[i - 1, j] + m[i, j - 1]
  m[n, n]
}
grid_paths(20)

# 16
library(gmp)
num = as.bigz(2^1000)
# Going to need this again for 20
sumDigits(num)

# 17
# If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, 
# how many letters would be used?
# NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
# contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. 
# The use of "and" when writing out numbers is in compliance with British usage.

ones = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
teens = c("eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
tens = c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
ll = function(x) sum(nchar(x))

10 * (ll(ones) + ll("ten") + ll(teens) + 10 * ll(tens) + length(tens) * ll(ones)) +
  100 * ll(ones) + 900 * ll("hundred") + 891 * ll("and") + 
  ll("onethousand")

# 18
# From each point, what's the best way out? Lots of ways to end up on each location at each row, 
# but only the optimal from that point forward matters
d = readLines("data18.txt") 
d = lapply(d, function(x) 
  as.numeric(strsplit(x, " ")[[1]]))
ud = rev(d)
tot = vector("list", length(ud))
tot[[1]] = ud[[1]]
for (row in 2:length(tot))
  tot[[row]] = sapply(1:length(ud[[row]]), function(item) ud[[row]][item] + max(tot[[row - 1]][c(item, item + 1)]))
tot

# 19
# 1 Jan 1900 was a Monday.
# Thirty days has September,
# April, June and November.
# All the rest have thirty-one,
# Saving February alone,
# Which has twenty-eight, rain or shine.
# And on leap years, twenty-nine.
# A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
# How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
d = data_frame(
  day = rep(1L),
  month = rep(c(rep("jan", 31), rep("feb", 29), rep("mar", 31), rep("april", 30),
                rep("may", 31), rep("june", 30), rep("july", 31), rep("aug", 31),
                rep("sept", 30), rep("oct", 31), rep("nov", 30), rep("dec", 31)), 101),
  year = rep(1900:2000, each = 366),
  dayOfWeek = rep("A")
)
for (i in 2:nrow(d)) 
  d$day[i] = if(d$month[i - 1] != d$month[i]) 1 else d$day[i - 1] + 1
d = filter(d, !(month == "feb" & day == 29 & year %% 4 != 0))
d$dayOfWeek = rep(c("M", "T", "W", "R", "F", "Sa", "Su"), len = nrow(d))
d = filter(d, year > 1900)
sum(d$day == 1 & d$dayOfWeek == "Su")

# 20
library(gmp)
as.bigz(factorial(as.bigz(100))) %>% sumDigits()


# 21
d = data_frame(
  ns = 1:1e4,
  dn = sapply(ns, function(n) {
      ints = seq_len(n - 1)
      sum(ints[!(n %% ints)])
    })
)
d$partners = c(0, map_dbl(d$ns[2:nrow(d)], function(n) d$dn[d$ns[d$dn[n]]]))
filter(d, ns != dn, ns == partners) %>%
  summarise(sum(ns))

# 22
x = scan("p022_names.txt", what = "character", sep = ",", na.strings = "")
x = sort(x)
splitNames = stringr::str_split(x, "")
nameSum = map_int(splitNames, function(name) sum(map_int(name, ~ which(LETTERS == .x))))
sum(seq_along(splitNames) * nameSum)

# 23
# it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. 
# However, this upper limit cannot be reduced any further by analysis even though it is known that the 
# greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.
# 
# Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

ints = 1:28123  # integers we need to consider
facsum = map_int(ints, ~ sum(findFactors(.x)))  # Find the sum of factors of each integer
abund = ints[facsum > ints]   # Find the abundant numbers

system.time({  # 57"
  twos = combn(abund, 2, simplify = FALSE)   # Find all the two-combos of abundant numbers except identical pairs
  allTwos = c(twos, lapply(abund, rep, 2))  # Add identical pairs
  sum = map_int(allTwos, sum)  # Find the sum of each pair
})

# OR, matrix style:
system.time({  # 25"
  twos = combn(abund, 2, simplify = TRUE)
  allTwos = cbind(twos, sapply(head(abund), rep, 2))
  sum = colSums(allTwos)
})

uSums = unique(sum[sum <= 28123])  # Find the unique sums of abundant numbers
sum(ints[!ints %in% uSums])  # Sum the integers that aren't sums of two unique numbers

# 24
install.packages("combinat")
system.time({perms = combinat::permn(0:9)})  # 40"
system.time({   # 37
  nums = map(perms, ~ as.numeric(paste(.x, collapse = "")))
})
sort(flatten_dbl(nums))[[1e6]]

# 25
# 1000 digits is greater than 1e999, so
n1 = n2 = n = as.bigz(1)
i = 2
while( nchar(as.character(n)) < 1000) {
  i = i + 1
  n = n1 + n2
  n1 = n2
  n2 = n
  cat(i, "\t", nchar(as.character(n)), "\n")
}

# 26
# From wikipedia: for primes, p,
# The length of the repetend (period of the repeating decimal) of 1/p is equal to the order of 10 modulo p. 
# If 10 is a primitive root modulo p, the repetend length is equal to p − 1; if not, the repetend length is a factor of p − 1.

ps = which(sapply(1:1e3, isprime))
ps = ps[-c(1, 3)]

st = Sys.time()
dig = map_dbl(ps, ~ mulitOrder(10, .x, prec = bitsNeeded(1e3)))
Sys.time() - st  # 1.2 min
max(dig) 
# The above doesn't work. Way to do the multi order in log10?





# 54
library(stringr)

splitHands = function(x) {
  h = str_split(x, "\\s")[[1]]
  list(h[1:5], h[6:10])
}

compareHands = function(hands) {

  handRanks = sapply(hands, classHand)

  if(length(unique(handRanks)) > 1)
    return(which.max(handRanks)) else
      return(breakTie(hands))
  
}

breakTie = function(twohands) {
  # twohands is list of two
  ht = lapply(twohands, function(x) 
    sort(table(makeNumeric(x)), dec = FALSE))
  # Walk right to left in freq-table comparing names (cards)
  for(i in rev(seq_along(ht[[1]]))) {
    comp = c(names(ht[[1]])[i], names(ht[[2]])[i])
    # Check for tie
    if(do.call(identical, as.list(comp)))  
      next
    win = which.max(comp)
    return(win)
  }
}

classHand = function(hand) {
  numHand = makeNumeric(hand)
  handRank = rankHand(numHand)
  if(flushCheck(hand)) {
    if(handRank == 5) handRank = 9
    if(handRank == 1) handRank = 6
  }
  return(handRank)
}


flushCheck = function(hand) {
  length(unique(sapply(hand, str_sub, 2, 2))) == 1  
}

makeNumeric = function(hand) {
  dplyr::left_join(
    data.frame(char = sapply(hand, str_sub, 1, 1)),
    data.frame(num = 2:14,
               char = c(as.character(2:9), "T", "J", "Q", "K", "A")),
    by = "char"
  )[["num"]]
}

rankHand = function(numHand) {

  # 1 - high card
  # 2 - pair
  # 3 - 2 pair
  # 4 - three of a kind
  # 5 - straight
  # 6 - flush
  # 7 - full house
  # 8 - four of a kind
  # 9 - straightflush
  
  tt = table(numHand)
  
  if(length(tt) == 5) {
    # Check for straight
    gaps = diff(sort(numHand))
    if(isTRUE(all.equal(gaps, rep(1, 4))) | 
       isTRUE(all.equal(gaps, c(1, 1, 1, 9)))) {
      return(5)
    } else { return(1) }
  }
  
  if(length(tt) == 4) 
    return(2)
  
  if(length(tt) == 3) {
    # Check for two pair
    if(sum(tt == 2) == 2)
      return(3) else 
        if(sum(tt == 3) == 1)
          return(4)
  }
  
  if(any(tt) == 4)
    return(8)
  
  if(all.equal(as.numeric(sort(tt)), c(2, 3)))
    return(7)
  
  stop("You shouldn't be here.")
}
  
hands = readLines("p054_poker.txt")
hands = lapply(hands, splitHands)
winners = sapply(hands, compareHands)
sum(winners == 1)
