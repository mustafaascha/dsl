
def qsort(lst):
	if len(lst) < 2:
		return lst
	pivot = lst[0]
	return qsort([x for x in lst if x < pivot]) + \
		   		 [x for x in lst if x == pivot] + \
		   qsort([x for x in lst if x > pivot])

print(qsort([1,3,2,4,5]))
