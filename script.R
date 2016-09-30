library(dlm); library(tidyverse)

d <- Nile

#モデル1 : Local level model with BFGS optimisation
#ローカルレベルモデル / ランダムウォークプラスノイズモデル。最適化はBFGS。
buildMdl1 <- function(theta){ #モデル構築
  dlmModPoly(order = 1, dV = exp(theta[1]), dW = exp(theta[2]))
}
fitMdl1 <- dlmMLE(d, c(0, 0), buildMdl1)　#パラメーターを推定。最適化手法はBFGS
Mdl1 <- fitMdl1$par %>% #最適化されたパラメーターを取り出す
  buildMdl1() %>% #最適化されたパラメーターをモデルに代入
  dlmFilter(d, .) %>% #カルマンフィルタにかける
  dlmSmooth() #スムージングする
plot(Nile, type = "o")
lines(Mdl1$s, col = 2)


