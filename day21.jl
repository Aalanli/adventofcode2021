
function newScore(a, s)
    return mod(a + s - 1, 10) + 1
end

const m = 21

function simulateL(p1, p2, s1, s2)
    u = [0, 0]
    for (d1, n) in dirac1
        p11 = newScore(p1, d1)
        if (s1 + p11) >= m
            u += [n, 0]
        else
            u += n * simulateR(p11, p2, (s1 + p11), s2)
        end
    end
    return u
end

function simulateR(p1, p2, s1, s2)
    u = [0, 0]
    for (d1, n) in dirac1
        p21 = newScore(p2, d1)
        if (s2 + p21) >= m
            u += [0, n]
        else
            u += n * simulateL(p1, p21, s1, (s2 + p21))
        end
    end
    return u
end

dirac1 = [(3,1), (4,3), (5,6), (6,7), (7,6), (8,3), (9,1)]

universes = simulateL(4,10,0,0)
print(max(universes[1]...))
