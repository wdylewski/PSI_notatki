podatek_Belki = function(przychod, koszt, typ_aktywa) {
  zysk = przychod - koszt
  
  if (zysk <= 0) {
    return(0)
  } else if (typ_aktywa == "akcje" || typ_aktywa == "obligacje") {
    return(zysk * 0.19)
  } else if (typ_aktywa == "kryptowaluty") {
    
    if (zysk <= 85528) {
      return(zysk * 0.18)
    } else {
      podatek_podstawa = 85528 * 0.18
      podatek_nadwyzka = (zysk - 85528) * 0.32
      return(podatek_podstawa + podatek_nadwyzka)
    }
    
  } else {
    return("Brak danych o podatku dla tego aktywa")
  }
}

# Testowanie funkcji 
print(podatek_Belki(15000, 10000, "akcje"))          
print(podatek_Belki(8000, 10000, "akcje"))           
print(podatek_Belki(150000, 50000, "kryptowaluty"))
print(podatek_Belki(1500, 1000, "lokata"))
