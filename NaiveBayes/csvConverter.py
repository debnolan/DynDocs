import numpy as np
import idx2numpy 
import csv

""" The idx files for the MNIST dataset can be downloaded at
http://yann.lecun.com/exdb/mnist/.  This python script can
then be used to convert them into two csv files.  The first
containing all 70,000 images (one row per image), and the
second containing all 70,000 labels (single row). """

trainImages = idx2numpy.convert_from_file('/home/simjay/workspace/NaiveBayes/others/mnist/train-images.idx3-ubyte')
trainLabels = idx2numpy.convert_from_file('/home/simjay/workspace/NaiveBayes/others/mnist/train-labels.idx1-ubyte')
testImages = idx2numpy.convert_from_file('/home/simjay/workspace/NaiveBayes/others/mnist/t10k-images.idx3-ubyte')
testLabels = idx2numpy.convert_from_file('/home/simjay/workspace/NaiveBayes/others/mnist/t10k-labels.idx1-ubyte')

trainImages = np.concatenate([trainImages.reshape(60000,784)])
testImages = np.concatenate([testImages.reshape(10000,784)])
trainLabels = np.concatenate([trainLabels])
testLabels = np.concatenate([testLabels])



with open('/home/simjay/workspace/NaiveBayes/mnistCSV/trainImages.csv', 'w') as csvfile:
	writer = csv.writer(csvfile, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
	for row in trainImages:
		writer.writerow(row)

print("done")

with open('/home/simjay/workspace/NaiveBayes/mnistCSV/testImages.csv', 'w') as csvfile:
	writer = csv.writer(csvfile, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
	for row in testImages:
		writer.writerow(row)

print("done")
			
with open('/home/simjay/workspace/NaiveBayes/mnistCSV/trainLabels.csv', 'w') as csvfile:
	writer = csv.writer(csvfile, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
	writer.writerow(trainLabels)

print("done")

with open('/home/simjay/workspace/NaiveBayes/mnistCSV/testLabels.csv', 'w') as csvfile:
	writer = csv.writer(csvfile, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
	writer.writerow(testLabels)

print("done")