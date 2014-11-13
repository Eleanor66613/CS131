
# Name: Jing ZHAO
#
# UID: 404426610
#
# People I interacted with:
#
# Resources I used:
#


import math
import struct

# PROBLEM 1

# parse the file named fname into a dictionary of the form 
# {'width': int, 'height' : int, 'max' : int, 'pixels' : (int * int * int) list}
def parsePPM(fname):
    f = open(fname, 'rb')
    f.readline()
    ppm = {}
    dimensions  = f.readline().split()
    ppm['width'] = int(dimensions[0])
    ppm['height'] = int(dimensions[1])
    ppm['max'] = int(f.readline())

    #pixels is a list of tuples
    pixels = []
    rgbB = f.read(3)
    while rgbB!="":
        pixels.append(struct.unpack('BBB',rgbB))
        rgbB = f.read(3)

    ppm['pixels'] = pixels
    f.close()

    return ppm

def parsePPM2(fname):
    mydict = {}
    f = open(fname, 'r')
    f.readline()
    
    ints = [int(x) for x in f.readline().split()]
    mydict['width'] = ints[0]
    mydict['height'] = ints[1]
    
    mydict['max'] = [int(x) for x in f.readline().split()][0]
    
    pixels = []
    data = f.read()
    for i in range(0, len(data)/3):
        pixels.append(struct.unpack('BBB', data[i*3:(i+1)*3]))
    mydict['pixels'] = pixels

    
    f.close()
    return mydict

# a test to make sure you have the right format for your dictionaries
def testParsePPM():
    return parsePPM("example.ppm") == {'width': 2, 'max': 255, 'pixels': [(10, 23, 52), (82, 3, 215), (30, 181, 101), (33, 45, 205), (40, 68, 92), (111, 76, 1)], 'height': 3}

# write the given ppm dictionary as a PPM image file named fname
# the function should not return anything
def unparsePPM(ppm, fname):
    f = open(fname, 'wb')
    f.write('P6\n')
    f.write(str(ppm['width']) + " " + str(ppm['height']) + "\n")
    f.write(str(ppm['max']) + "\n")
    #pixels is a list of strings(a sequence), we should use writelines instead of write()
    #the struct operation can make a tuple in this way
    pixels = [struct.pack('BBB',p[0],p[1],p[2]) for p in ppm['pixels']]
    f.writelines(pixels)
    f.close()


# PROBLEM 2
def negate(ppm):
    negppm = ppm.deepcopy()
    max = negppm['max']
    negppm['pixels'] = [(max-rgb[0],max-rgb[1],max-rgb[2]) for rgb in ppm['pixels']]
    return negppm


# PROBLEM 3
def mirrorImage(ppm):
    mirppm = ppm.deepcopy()
    width = mirppm['width']
    height = mirppm['height']
    num_pix = width * height
    rows = [mirppm['pixels'][i:i+width] for i in range(0,num_pix,width)]
   
    [e.reverse() for e in rows]
   
    mirppm['pixels'] = [e for row in rows for e in row]
    #print ppm['pixels']
    #print rows: each row now is a list
    #after reverse:element in each row is reversed(inner is not reversed)
    #print mirppm['pixels']: expand it to a one dimension list
    #test
    #[(10, 23, 52), (82, 3, 215), (30, 181, 101), (33, 45, 205), (40, 68, 92), (111, 76, 1)]
    #[[(10, 23, 52), (82, 3, 215)], [(30, 181, 101), (33, 45, 205)], [(40, 68, 92), (111, 76, 1)]]
    #[[(82, 3, 215), (10, 23, 52)], [(33, 45, 205), (30, 181, 101)], [(111, 76, 1), (40, 68, 92)]]
    #[(82, 3, 215), (10, 23, 52), (33, 45, 205), (30, 181, 101), (111, 76, 1), (40, 68, 92)]

    return mirppm
# PROBLEM 4

# produce a greyscale version of the given ppm dictionary.
# the resulting dictionary should have the same format, 
# except it will only have a single value for each pixel, 
# rather than an RGB triple.
def greyscale(ppm):
    greyDic = ppm.deepcopy()
    greyDic['pixels'] = [int(round(.299 * R + .587 * G + .114 * B)) for (R,G,B) in greyDic['pixels']]
    return greyDic

# take a dictionary produced by the greyscale function and write it as a PGM image file named fname
# the function should not return anything
def unparsePGM(pgm, fname):
    f = open(fname,'wb')
    f.write('P5\n')
    f.write(str(pgm['width']) + " " + str(pgm['height']) + "\n")
    f.write(str(pgm['max']) + "\n")
    pixels = [struct.pack('B',x) for x in pgm['pixels']]
    #print pixels: why not int
    f.writelines(pixels)
    f.close()

# PROBLEM 5

# gaussian blur code adapted from:
# http://stackoverflow.com/questions/8204645/implementing-gaussian-blur-how-to-calculate-convolution-matrix-kernel
def gaussian(x, mu, sigma):
  return math.exp( -(((x-mu)/(sigma))**2)/2.0 )

def gaussianFilter(radius, sigma):
    # compute the actual kernel elements
    hkernel = [gaussian(x, radius, sigma) for x in range(2*radius+1)]
    vkernel = [x for x in hkernel]
    kernel2d = [[xh*xv for xh in hkernel] for xv in vkernel]

    # normalize the kernel elements
    kernelsum = sum([sum(row) for row in kernel2d])
    kernel2d  =[[x/kernelsum for x in row] for row in kernel2d]
    return kernel2d

# blur a given ppm dictionary, returning a new dictionary  
# the blurring uses a gaussian filter produced by the above function
def gaussianBlur(ppm, radius, sigma):
    # obtain the filter
    #the filter is a list of list
    result = ppm.deepcopy()

    gfilter = gaussianFilter(radius, sigma)
    #print gfilter
    pixels = ppm['pixels']
    width = ppm['width']
    height = ppm['height']
    blurred = list(pixels)
    for i in range(0,height):
        for j in range(0,width):
            blurredR = 0.0
            blurredG = 0.0
            blurredB = 0.0
            for ii in range(-radius, radius+1):
                for jj in range(-radius, radius+1):
                    index_x = i+ii;
                    if(index_x < 0):
                        index_x = 0
                    elif(index_x >= height):
                        index_x = height -1

                    index_y = j+jj
                    if(index_y<0):
                        index_y = 0
                    elif(index_y >= width):
                        index_y = width-1

                    blurredR += pixels[index_x * width + index_y][0] * gfilter[radius+ii][radius+jj]
                    blurredG += pixels[index_x * width + index_y][1] * gfilter[radius+ii][radius+jj]
                    blurredB += pixels[index_x * width + index_y][2] * gfilter[radius+ii][radius+jj]
            blurred[i * width +j] = (int(round(blurredR)),int(round(blurredG)),int(round(blurredB)))
    result['pixels'] = blurred
    return result


def testFunction():
    #originalDic = parsePPM('example.ppm')
    originalDic = parsePPM('florence.ppm')
    negGateDic = negate(originalDic)
    negGatePPM = unparsePPM(negGateDic,'florenceNegate.ppm')

    mirrorDic = mirrorImage(originalDic)
    mirrorPPM = unparsePPM(mirrorDic,'florenceMirrow.ppm')

    GreyDic = greyscale(originalDic)
    GreyPPM = unparsePGM(GreyDic,'florenceGrey.ppm')

    BlurDic = gaussianBlur(originalDic,1,2.0)
    BlurPPM = unparsePPM(BlurDic,'GaussianBlur01.ppm')

    BlurDic = gaussianBlur(originalDic,2,2.0)
    BlurPPM = unparsePPM(BlurDic,'GaussianBlur02.ppm')

    BlurDic = gaussianBlur(originalDic,1,3.0)
    BlurPPM = unparsePPM(BlurDic,'GaussianBlur03.ppm')

def testFunction2():
    #originalDic = parsePPM('example.ppm')
    originalDic = parsePPM('example.ppm')
    negGateDic = negate(originalDic)
    negGatePPM = unparsePPM(negGateDic,'exampleNegate.ppm')
    
    mirrorDic = mirrorImage(originalDic)
    mirrorPPM = unparsePPM(mirrorDic,'exampleMirrow.ppm')
    
    GreyDic = greyscale(originalDic)
    GreyPPM = unparsePGM(GreyDic,'exampleGrey.ppm')
    
    BlurDic = gaussianBlur(originalDic,1,2.0)
    BlurPPM = unparsePPM(BlurDic,'exampleBlur01.ppm')
    
    BlurDic = gaussianBlur(originalDic,2,2.0)
    BlurPPM = unparsePPM(BlurDic,'exampleGaussianBlur02.ppm')
    
    BlurDic = gaussianBlur(originalDic,1,3.0)
    BlurPPM = unparsePPM(BlurDic,'exampleGaussianBlur03.ppm')

