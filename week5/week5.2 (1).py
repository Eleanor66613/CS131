def fact(n):
	result  =1
	for i in range(2,n+1)
		result *= 1
		return result

def doSomething(fname):
	f = open(fname, 'r')
	for line in f:
		print line


# type the following in toplevel
# f = (lambda x:  x+1)
# map(f, [1,2,3,4,5])
# reduce is same as fold_left in ocaml
# reduce((lambda x,y:x+y), [1,2,3,4,5,6],0)
# reduce((lambda x,y:x+y), [1,2,3,4,5,6],0)

# a short hand way
# [x+1 for x in [1,2,3,4,5]]

# l = [1,2,3,4,5]
# map((lambda x : x+1), l)
# 4 % 2
# filter((lambda x : x %2 == 1), l)

# a short hand way  (shy)
# [x+1 for x in l]
# [x+1 for x in l if x % 2 == 1]

# l1 = [1,2,3,4]
# l2 = [5,6,7,8]
# [x*y for x in l1 for y in l2]

# l = [[1,2],[3,4],[4,5]
def unzip(l):
	return ([x[0] for x in l], [x[1] for x in l])

def unzip2(l):
	return ([a for (a,b) in l], [b for (a,b) in l])

# other nice syntax for list []
# l[1:]
# l[1]
# l[1:3]
# help(l)
# l.append(6)


def unzip3(l):
	return reduce.....

# l1 = [[1,2,3],[4,5,6],[7,8,9]]
# map((lambda l: l.reverse()), l1)
# this returns [None, None, None], how to fix it?
# map(l.reverse(), l1)


def myReverse(ll):
	return map((lambda l: myReverse(l)),ll)



def quicksort(l):
	if (l == []):
		return l
	else:
		return quicksort([x for x in l[1:] if x <=l[0]]) + [l[0]] + \
			   quicksort([x for x in l[1:] if x >l[0]])
# quicksort([4,3,2,6,7,8,1])


def isPrime(x):
	if (x == 1):
		return False
	for y in range(2,x-1): 
		if (x % y == 0):
			return False
	return True

def primeUpTo(n):
	return filter((lambda x: isPrime(x)), range(2, n+1))
# or [x for x in range(2,,n+1) if isPrime(x)]


# reload(ex)   ex is the file name, ex.py


# use reduce to re-write primeUpTo
def isPrime2(n):
	return not reduce((lambda i,b: (n % i == 0) or b), range(2, n-1), False)

def isPrime3(n):
	return filter((lambda i: n % i == 0), range(2,n-1)) == [] if n != 1 else False

# Dictionary in Python
# mydict = {'name':'mypoint', 'x':3.4, 'y':5.6}
# get value from key
# mydict['name']
# add a k-v pair
# mydict['color'] = 'red'
# mydict
# 'name' in mydict
# 'n' in mydict

# frequency([1,2,1,1,4,5,4]) returns {1:3, 2:1, 4:2, 5:1}
def frequency(l):
	mydict = {}
	for x in l:
		if x in mydict:
			mydict[x] += 1
		else:
			mydict[x] = 1
	return mydict
# frequency("hello my name is jian gong")


# remove a key
# mydict = {'name':'mypoint', 'x':3.4, 'y':5.6}
# mydict.pop('name')


















