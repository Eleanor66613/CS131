
# Name: Wen Shi
#
# UID:504007279
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
    d ={}
    filetype = f.readline()
    wh = f.readline().split()
    d["width"] = int(wh[0])
    d["height"] = int(wh[1])
    d["max"] = int(f.readline())

    pixelList = []
    pixels=f.readline()
    for i in range (0, len(pixels)/3):
        pixelList.append(struct.unpack('BBB',pixels[i*3]+pixels[i*3+1]+pixels[i*3+2]))
    d["pixels"] = pixelList
    f.close()
    return d


# a test to make sure you have the right format for your dictionaries
def testParsePPM():
    return parsePPM("example.ppm") == {'width': 2, 'max': 255, 'pixels': [(10, 23, 52), (82, 3, 215), (30, 181, 101), (33, 45, 205), (40, 68, 92), (111, 76, 1)], 'height': 3}

# write the given ppm dictionary as a PPM image file named fname
# the function should not return anything
def unparsePPM(ppm, fname):
    f = open(fname, 'w')
    f.write('P6\n')
    f.write(str(ppm["width"])+' '+str(ppm["height"])+'\n')
    f.write(str(ppm["max"])+'\n')
    for x in ppm["pixels"]:
        f.write(struct.pack('BBB', x[0],x[1],x[2]))
    f.close()


# PROBLEM 2
def negate(ppm):
    negatePPM = ppm.copy()
    maxColor = ppm['max']
    pixels = [(maxColor - x[0], maxColor - x[1], maxColor - x[2]) for x in ppm['pixels']]
    negatePPM['pixels'] = pixels
    return negatePPM


# PROBLEM 3
def mirrorImage(ppm):
    mirrorPpm = ppm.copy()
    height = mirrorPpm['height']
    width = mirrorPpm['width']
    pixels = ppm['pixels']
    for i in range(0,height):
        for j in range (0, width/2):
            temp = pixels[i*width + j]
            pixels[i*width + j] = pixels[(i+1)*width-j-1
            pixels[(i+1)*width-j-1] = temp
    mirrorPpm['pixels'] = pixels
    return mirrorPpm


# PROBLEM 4

# produce a greyscale version of the given ppm dictionary.
# the resulting dictionary should have the same format, 
# except it will only have a single value for each pixel, 
# rather than an RGB triple.
def greyscale(ppm):
    greyPpm = ppm.copy()
    pixels = [round(.299 * x[0] + .587 * x[1] + .114 * x[2]) for x in ppm['pixels']]
    greyPpm['pixels']=pixels
    return greyPpm    

# take a dictionary produced by the greyscale function and write it as a PGM image file named fname
# the function should not return anything
def unparsePGM(pgm, fname):
    f = open(fname, 'w')
    f.write('P5\n')
    f.write(str(pgm["width"])+' '+str(pgm["height"])+'\n')
    f.write(str(pgm["max"])+'\n')
    for x in pgm["pixels"]:
        f.write(struct.pack('B', x))
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
    kernel2d = [[x/kernelsum for x in row] for row in kernel2d]
    return kernel2d

# blur a given ppm dictionary, returning a new dictionary  
# the blurring uses a gaussian filter produced by the above function
def gaussianBlur(ppm, radius, sigma):
    # obtain the filter
    gfilter = gaussianFilter(radius, sigma)
    height = ppm['height']
    width = ppm['width']
    pixels = ppm['pixels']
    blurPpm = ppm.copy()
    blurPixels = []
    for i in range(radius,height-radius):
        for j in range(radius, width-radius):
            blurR = 0.0
            blurG = 0.0
            blurB = 0.0
            for x in range(-radius, radius+1):
                for y in range(-radius, radius+1):
                    blurR += pixels[(i+x)*width+j+y][0] * gfilter[x+radius][y+radius]
                    blurG += pixels[(i+x)*width+j+y][1] * gfilter[x+radius][y+radius]
                    blurB += pixels[(i+x)*width+j+y][2] * gfilter[x+radius][y+radius]
            blurPixels[i*width+j] = (int(round(blurR)), int(round(blurG)), int(round(blurB)))

    blurPpm ['pixels'] = blurPixels
    return blurPpm





