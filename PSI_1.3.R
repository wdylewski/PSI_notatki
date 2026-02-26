kostka = function(n) {
  wyniki = sample(1:6, size = n, replace = TRUE)
  return(wyniki)
}

rzuty_10 = kostka(10)
print("Wyniki dla 10 rzutów:")
print(table(rzuty_10))

rzuty_100 = kostka(100)
print("Wyniki dla 100 rzutów:")
print(table(rzuty_100))

rzuty_10000 = kostka(10000)
print("Wyniki dla 10000 rzutów:")
print(table(rzuty_10000))

