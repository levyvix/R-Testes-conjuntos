vpp = function(s,e,p){
  return((s*p)/(s*p+(1-e)*(1-p)))
}

vpn = function(s,e,p){
  return((e*(1-p))/(e*(1-p)+(1-s)*p))
}

#---------------SERIE-----------------------
sensibilidade_serie = function(sA,sB){
  return(sA*sB)
}
especificidade_serie = function(eA,eB){
  return(eA+eB-eA*eB)
}
# --------------PARALELO--------------------
sensibilidade_paralelo = function(sA,sB){
  return(sA+sB-sA*sB)
}
especificidade_paralelo = function(eA,eB){
  return(eA*eB)
}

# --------------VALORES---------------------
sensibilidade1 = 0.89
especificidade1 = 0.95

sensibilidade2 = 0.93
especificidade2 = 0.89

prevalencia = 0.025

# vpp e vpn individual de cada teste
# TESTE A
vpp(sensibilidade1,especificidade1,prevalencia)
vpn(sensibilidade1,especificidade1,prevalencia)

# TESTE B
vpp(sensibilidade2,especificidade2,prevalencia)
vpn(sensibilidade2,especificidade2,prevalencia)

# testes em conjunto (Série)
sensiblidade_conjunta_s = sensibilidade_serie(sensibilidade1,sensibilidade2);sensiblidade_conjunta
especificidade_conjunta_s = especificidade_serie(especificidade1,especificidade2);especificidade_conjunta
#vpp e vpn em conjunto (Serie)
vpp(sensiblidade_conjunta,especificidade_conjunta,prevalencia)
vpn(sensiblidade_conjunta,especificidade_conjunta,prevalencia)


# testes em conjunto (Paralelo)
sensiblidade_conjunta_p = sensibilidade_paralelo(sensibilidade1,sensibilidade2);sensiblidade_conjunta
especificidade_conjunta_p = especificidade_paralelo(especificidade1,especificidade2);especificidade_conjunta
#vpp e vpn em conjunto (Paralelo)
vpp(sensiblidade_conjunta,especificidade_conjunta,prevalencia)
vpn(sensiblidade_conjunta,especificidade_conjunta,prevalencia)


pv = data.frame(matrix(nrow = 0, ncol = 5))
names(pv) <- c("Prevalencia\nP(D+)", "VPP Serie\nP(D+|T+)", "VPN Serie\nP(D-|T-)","VPP Paralelo\nP(D+|T+)", "VPN Paralelo\nP(D-|T-)")
for(i in 1:1000){
  prevalencia = 1/(i*10)
  x = vpp(sensiblidade_conjunta_s,especificidade_conjunta_s,prevalencia)
  y = vpn(sensiblidade_conjunta_s,especificidade_conjunta_s,prevalencia)
  x1 = vpp(sensiblidade_conjunta_p,especificidade_conjunta_p,prevalencia)
  y1 = vpn(sensiblidade_conjunta_p,especificidade_conjunta_p,prevalencia)
  pv[i, 1] = prevalencia
  pv[i, 2:5] = c(x,y,x1,y1)
}

plot(pv)
