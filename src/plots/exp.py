import numpy as np
import matplotlib.pyplot as pl

x = np.arange(1,7)
y = np.array([14, 32, 66, 132,262,520])
coef = np.polyfit(x, np.log(y), 1)
fit = np.poly1d(coef)

yn2 = 2**(x+3)
yfit = 2**(fit(x+4))

pl.plot(x, y, label="actual")
pl.plot(x, yn2, label="2^n")
pl.plot(x, yfit, label="fit")
pl.ylabel("type vars")
pl.xlabel("function")
pl.legend()

print(y)
print(yn2)
print(yfit)

pl.show()
