#conversion of the HTML files to a list format for interactive text

intro1 = readLines("introCal.html", warn = FALSE)
intro2 = readLines("introAlc.html", warn = FALSE)
intro3 = readLines("introHIV.html", warn = FALSE)

intros = list("Calcium" = intro1, "Alcohol" = intro2, "HIV" = intro3)

choice1 = readLines("threshCal.html", warn = FALSE)
choice2 = readLines("threshAlc.html", warn = FALSE)
choice3 = readLines("threshHIV.html", warn = FALSE)

choices = list("Calcium" = choice1, "Alcohol" = choice2, "HIV" = choice3)

#file used in the dynamic document for interactive text
save(intros, choices, file = "htmlChoices.rda")
