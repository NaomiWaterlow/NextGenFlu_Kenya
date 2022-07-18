f <- function(x, K, C = .1658, beta = .04, r, a) {
  K * C * x * exp(-beta * x )*exp(-r * (x - a))+ (1 - K) * exp(-r * (x - a))}

burden <- function(N, DW, A, L, K, r, a) {
  N * DW * integrate(f, lower= A, upper = A + L, K = K, r = r, a = a)$value}

yld.f <- function(x, r, a) {
  exp(-r * (x - a))}

yld.burden <- function(N, DW, L, r, a) {
  N * DW * integrate(yld.f, a = a, lower= a, upper = a + L, r = r)$value}

N = 1
DW = 
A = 5
L = 75.10618
K = 0
r = 0.035
a = 5

burden(N = N, DW = DW, A=A, L =L, K = K, r = r, a = a)
#f(x=19, K=K, r = r, a = a)

for (i in 1:nsample){
  YLDmilddaly[i,1] <- burden(N = urtcases[i,1], DW = milddaly[i], A = 0.5,  L = durn_illness/365, K = 0, r = r, a = 0.5)
  YLDmilddaly[i,2] <- burden(N = urtcases[i,2], DW = milddaly[i], A = 2.5,  L = durn_illness/365, K = 0, r = r, a = 2.5)
  YLDmilddaly[i,3] <- burden(N = urtcases[i,3], DW = milddaly[i], A = 10,   L = durn_illness/365, K = K, r = r, a = 10)
  YLDmilddaly[i,4] <- burden(N = urtcases[i,4], DW = milddaly[i], A = 17,   L = durn_illness/365, K = K, r = r, a = 17)
  YLDmilddaly[i,5] <- burden(N = urtcases[i,5], DW = milddaly[i], A = 34.5, L = durn_illness/365, K = K, r = r, a = 34.5)
  YLDmilddaly[i,6] <- burden(N = urtcases[i,6], DW = milddaly[i], A = 72.5, L = durn_illness/365, K = 0, r = r, a = 72.5)
  YLDmilddaly[i,7] <- rowSums(YLDmilddaly[i,1:6], na.rm = FALSE, dims = 1)
}

N = 100000
DW = 0.133
A = 10
L = 4/365
K = 0
r1 = 0.035
a = 10

N*DW*L*(exp(-r1*L))

r2 = 0.035
burden(N = N, DW = DW, A=A, L =L, K = K, r = r2, a = a)
yld.burden(N = N, DW = DW, L = L, r = r, a = a)

samples.combined.df %>% 
  select(Scenario, Year, Sample, total.YLD00.all.ages, total.YLD03.all.ages) %>% View()
