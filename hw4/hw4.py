
# Name: Wen Shi
#
# UID:504007279
#
# People I interacted with: JingZhao
#
# Resources I used:
#


import math
import struct

# PROBLEM 1

# parse the file named fname into a dictionary of the form 
# {'width': int, 'height' : int, 'max' : int, 'pixels' : (int * int * int) list}
def parsePPM(fname):
    f=open(fname, 'rb')
    fileType=f.readline()
    wh=f.readline().split()
    dictionary={}
    dictionary['width'] = int(wh[0])
    dictionary['height'] = int(wh[1])
    dictionary['max']=int(f.readline())
    pixels = f.read()
    p=[]
    for i in range(0, len(pixels)/3):
        p.append(struct.unpack('BBB', pixels[3*i]+pixels[3*i+1]+pixels[3*i+2]))
    dictionary['pixels']=p
    f.close()
    return dictionary


# a test to make sure you have the right format for your dictionaries
def testParsePPM():
    return parsePPM("example.ppm") == {'width': 2, 'max': 255, 'pixels': [(10, 23, 52), (82, 3, 215), (30, 181, 101), (33, 45, 205), (40, 68, 92), (111, 76, 1)], 'height': 3}

# write the given ppm dictionary as a PPM image file named fname
# the function should not return anything
def unparsePPM(ppm, fname):
    f=open(fname, 'wb')
    f.write('P6\n')
    f.write(str(ppm['width']) + ' ' + str(ppm['height']) + '\n')
    f.write(str(ppm['max']) + '\n')
    pixels = [struct.pack('BBB', x[0], x[1], x[2]) for x in ppm['pixels']]
    f.writelines(pixels)
    f.close()


# PROBLEM 2
def negate(ppm):
    negatePpm=ppm.copy()
    maxColor=ppm['max']
    pixels=[(maxColor-x[0], maxColor-x[1], maxColor-x[2]) for x in ppm['pixels']]
    negatePpm['pixels']=pixels
    return negatePpm


# PROBLEM 3
def mirrorImage(ppm):
    mirrorPpm = ppm.copy()
    pixels = ppm['pixels']
    width = ppm['width']
    height = ppm['height']
    for i in range (0, height):
        for j in range(0, width/2):
            temp = pixels[i*width+j]
            pixels[i*width+j] = pixels[(i+1)*width-j-1]
            pixels[(i+1)*width-j-1] = temp
    mirrorPpm['pixels'] = pixels
    return mirrorPpm



# PROBLEM 4

# produce a greyscale version of the given ppm dictionary.
# the resulting dictionary should have the same format, 
# except it will only have a single value for each pixel, 
# rather than an RGB triple.
def greyscale(ppm):
    greyPpm=ppm.copy()
    greyPpm['pixels'] = [round(0.299 * x[0] + 0.587 * x[1] + 0.114 * x[2]) for x in ppm['pixels']]
    return greyPpm

# take a dictionary produced by the greyscale function and write it as a PGM image file named fname
# the function should not return anything
def unparsePGM(pgm, fname):
    f=open(fname, 'wb')
    f.write('P5\n')
    f.write(str(pgm["width"])+' '+str(pgm["height"])+'\n')
    f.write(str(pgm["max"])+'\n')
    for x in pgm["pixels"]:
        f.write(struct.pack('B', x))


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
    blurPpm=ppm.copy()
    pixels = ppm['pixels']
    height = ppm['height']
    width = ppm['width']
    blurPixels = list(pixels)
    for i in range(0, height):
        for j in range(0, width):
            blurR = 0.0
            blurG = 0.0
            blurB = 0.0
            for x in range(-radius, radius+1):
                for y in range(-radius, radius+1):
                    index_x = i+x;
                    if(index_x < 0):
                        index_x = 0
                    elif(index_x >= height):
                        index_x = height -1

                    index_y = j+y
                    if(index_y<0):
                        index_y = 0
                    elif(index_y >= width):
                        index_y = width-1
                    blurR += pixels[index_x * width + index_y][0] * gfilter[radius+x][radius+y]
                    blurG += pixels[index_x * width + index_y][1] * gfilter[radius+x][radius+y]
                    blurB += pixels[index_x * width + index_y][2] * gfilter[radius+x][radius+y]
            blurPixels[i*width+j] = (int(round(blurR)), int(round(blurG)), int(round(blurB)))
    blurPpm['pixels'] = blurPixels
    return blurPpm

