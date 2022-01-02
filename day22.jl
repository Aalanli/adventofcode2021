
function parseIO(x)
    x1 = split(x, " ")
    if x1[1] == "on"
        s = 1
    else
        s = 0
    end
    b = map(f->map(f1->parse(Int64, f1), split(f[3:end], "..")), split(x1[2], ","))
    return (s, b)
end

input = begin
    i = open(f->read(f, String), "22.txt")
    map(parseIO, split(i, "\n"))
end


function fillCuboid!(m::Array{Int, 3}, x::Tuple{Int, Vector{Vector{Int}}})
    (bVal, xyz) = x
    xyz = map(f->f.+51,xyz)
    for c in xyz
        if c[1] < 1 || c[2] > 101
            return m
        end
    end
    m[xyz[1][1]:xyz[1][2], xyz[2][1]:xyz[2][2], xyz[3][1]:xyz[3][2]] .= bVal
end
    
cuboid = fill(0, (101, 101, 101))

for i in input
    fillCuboid!(cuboid, i)
end

print(sum(cuboid))

struct Cuboid
    x::Tuple{Int,Int}
    y::Tuple{Int,Int}
    z::Tuple{Int,Int}
    v::Bool
end

mutable struct MergedCuboid
    x::Tuple{Int,Int}
    y::Tuple{Int,Int}
    z::Tuple{Int,Int}
    sub::Vector{Cuboid}
end

function volume(a::Cuboid) :: Int
    x = abs(a.x[2] - a.x[1]) + 1
    y = abs(a.y[2] - a.y[1]) + 1
    z = abs(a.z[2] - a.z[1]) + 1
    return x * y * z
end

function volume(a::Vector{Cuboid}) :: Int
    sum(volume.(a))
end

function within(a::Cuboid, b::Cuboid) :: Bool
    for (i, j) in zip([a.x, a.y, a.z], [b.x, b.y, b.z])
        v1 = (i[1] <= j[1] <= i[2] || i[1] <= j[2] <= i[2])
        (j, i) = (i, j)
        v2 = (i[1] <= j[1] <= i[2] || i[1] <= j[2] <= i[2])
        if !(v1 || v2)
            return false
        end
    end
    return true
end

function within(a::Cuboid, b::MergedCuboid) :: Bool
    for i in b.sub
        if !within(a, i)
            return false
        end
    end
    return true
end

function enclose(a::Cuboid, b::Cuboid) :: Bool  # is b enclosed by a
    for (i, j) in zip([a.x, a.y, a.z], [b.x, b.y, b.z])
        v1 = (i[1] <= j[1] && j[2] <= i[2])
        if !v1
            return false
        end
    end
    return true
end

function enclose(a::Cuboid, b::MergedCuboid) :: Bool  # is set b enclosed by a
    for i in b.sub
        if !enclose(a, i)
            return false
        end
    end
    return true
end

function cutSingleDim(a::Tuple{Int, Int}, b::Tuple{Int, Int}) :: Vector{Tuple{Int, Int}}
    b1 = a[1] < b[1] <= a[2]
    b2 = a[1] <= b[2] < a[2]
    if b1 && b2
        return [(a[1], b[1]-1), (b[2]+1, a[2]), (b[1], b[2])]
    elseif b1
        return [(a[1], b[1]-1), (b[1], a[2])]
    elseif b2
        return [(b[2]+1, a[2]), (a[1], b[2])]
    else
        return [(a[1], a[2])]
    end
end

function cut(a::Cuboid, b::Cuboid) :: Vector{Cuboid} # use b to cut a
    x = [Cuboid(i, a.y, a.z, a.v) for i in cutSingleDim(a.x, b.x)]
    y = (j=x[end]; [Cuboid(j.x, i, j.z, a.v) for i in cutSingleDim(j.y, b.y)])
    z = (j=y[end]; [Cuboid(j.x, j.y, i, a.v) for i in cutSingleDim(j.z, b.z)])
    return cat(x[begin:end-1], y[begin:end-1], z[begin:end-1], dims=1)
end


function coalesce(xs::Vector{Cuboid}) :: Vector{Cuboid}
    s = [xs[1]] # the set of colesced cuboids / no intersections
    for a in xs[2:end] # adding a to s
        st = []
        if a.v # a is 'on'
            notConsume = true
            for b in s
                if enclose(b, a)
                    notConsume = false
                    push!(st, b)
                elseif within(a, b) && notConsume
                    append!(st, cut(b, a))
                elseif !enclose(a, b)
                    push!(st, b)
                end
            end
            if notConsume
                push!(st, a)
            end
        else
            for b in s
                if within(a, b)
                    append!(st, cut(b, a))
                else
                    push!(st, b)
                end
            end
        end
        s = st
    end
    return s
end


function main()
    cubes::Vector{Cuboid} = begin
        h = []
        for (b,xyz) in input
            push!(h, Cuboid((xyz[1][1],xyz[1][2]), (xyz[2][1],xyz[2][2]), (xyz[3][1],xyz[3][2]), b))
        end
        h
    end
    v = coalesce(cubes)
    println(volume(v))
end

main()
