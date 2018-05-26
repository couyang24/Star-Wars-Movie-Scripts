library(wordcloud2)
wordcloud2(data = demoFreq)

wordcloud2(demoFreq, minRotation = -pi/6, maxRotation = -pi/6, minSize = 10,
           rotateRatio = 1)

figPath = system.file("examples/t.png",package = "wordcloud2")
wordcloud2(demoFreq, figPath = figPath, size = 1.5,color = "skyblue")
