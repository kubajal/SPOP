rev xs = reverse xs []
reverse (x:xs []) = reverse (xs [x])
reverse (x:xs r:rs) = reverse (xs xs:r:rs)
reverse ([] acc) = acc

reverse [1,2,3]