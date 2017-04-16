import pandas as pd
import keras as ks

train = pd.read_csv("/Users/Hongbo/Desktop/Machine-Learning/Zipcode/CNN for Zipcode/zip.train.csv", sep = " ")
print( train.shape )

test = pd.read_csv("/Users/Hongbo/Desktop/Machine-Learning/Zipcode/CNN for Zipcode/zip.test.csv", sep = " ")
print( test.shape)





