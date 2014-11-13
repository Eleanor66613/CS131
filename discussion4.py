
#list comprehension

#[f(x) for x in l] --->map
#[f(x) for x in l if x%2 ==0]
#map (f, l)
#how to define a member function in python?
#(lambda x,y:____)
#dictionary in python  {x: f(x) for x in l}
#reduce in python is fold_left

#def fold_right (f,l,i) :
		#reduce(lambda p, x: f(x,p),i, reverse(l))
		#i if l = [] else f (l[0], fold_right (f, l[1:],i))

# i if l = [] else j
#l [a:b] all the elements from index of a to b-1

#given [2,6] -> {2: [1,2], 6: [1,2,3,6]}
def divisors(l): 
	return {x:[k for k in range(1, x+1) if x%k==0] for x in l}

def smallestSublist (l):
	reduce(lambda p x: x if len(x)<len(p) else p, l[1:], l[0])

#eratosthenes sieve  埃氏筛

def sieve (n):
	l = range(1,n+1)
	p=[]
	for k in range(2,n+1):
		if l == []:
			return p
		elif k == l[0]:
			l=[x for x in l if x%k!=0]
				p.append(k)
		else:
			continue

def sieve (n):
	l = range(1,n+1)
	p=[]
	for k in range(2,n+1):
		if l == []:
			return p
		elif k == l[k-2]:
			for j in range(2,[n/k])：
				l[j*k-2]=None
		else:
			continue

time complexity:O(NLOGLOGN)


