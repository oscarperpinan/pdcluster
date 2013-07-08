pd.theme=custom.theme.2(pch=19, cex=0.7, symbol = brewer.pal(n=8, name = "Set1"))
pd.theme$strip.background$col='lightgray'
pd.theme$strip.shingle$col='transparent'

custom.theme.3=custom.theme(symbol = brewer.pal(n=8, name = "Set1"), pch=19, cex=0.6)
custom.theme.4=custom.theme(symbol = brewer.pal(n=9, name='YlOrRd'), pch=19, cex=0.3)
custom.theme.3$strip.background$col='gray'
custom.theme.4$strip.background$col='gray'
custom.theme.3$strip.shingle$col='transparent'
custom.theme.4$strip.shingle$col='transparent'
