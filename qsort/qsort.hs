
qsort lst = 
    if length lst < 2 then lst
    else
        let pivot = lst !! 0
        in qsort([x | x <- lst, x < pivot]) ++ 
                 [x | x <- lst, x == pivot] ++ 
           qsort([x | x <- lst, x > pivot])

main = print (qsort [2,5,3,4,1])
