h(x, y) = x /= u where u = y/2

g(x, y, z) = 
    let b = x /= y || z
    in case (y > 0, b) of
        (True, True) -> h(x, 2*x)
        (False, False) -> h(y, 2*y)
        _ -> b|| z

f(x, y) = x/y