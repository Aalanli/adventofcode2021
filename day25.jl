
function getInput() :: Matrix{Char}
    input = split(open(f->read(f, String), "25.txt"), "\n")
    lenC = size(input, 1)
    lenR = length(input[1])
    arr = Matrix{Char}(undef, lenC, lenR)
    for i in 1:lenC
        for j in 1:lenR
            arr[i, j] = input[i][j]
        end
    end
    return arr
end

function wrapR(n, max)
    return (n - 1) % max + 1
end

function wrapAll(n, max)
    return wrapR(max + wrapR(n, max), max)
end

function show(arr::Matrix{Char})
    for i in 1:size(arr,1)
        for j in 1:size(arr,2)
            print(arr[i, j])
        end
        println("")
    end
    println("")
end

function stepX!(arr::Matrix{Char}) :: Tuple{Matrix{Char}, Bool}
    (yDim, xDim) = size(arr)
    changed = false
    firstRow = arr[:, 1]
    lastRow = arr[:, end]
    for i in 1:yDim
        j = 1
        while j < xDim
            if arr[i, j] == '>'
                jN = wrapR(j+1, xDim)
                if arr[i, jN] == '.'
                    changed = true
                    arr[i, j] = '.'
                    arr[i, jN] = '>'
                    j += 1
                end
            end
            j += 1
        end
        if lastRow[i] == '>' && firstRow[i] == '.'
            changed = true
            arr[i, xDim] = '.'
            arr[i, 1] = '>'
        end
    end
    return (arr, changed)
end

function stepY!(arr::Matrix{Char}) :: Tuple{Matrix{Char}, Bool}
    (yDim, xDim) = size(arr)
    changed = false
    firstRow = arr[1, :]
    lastRow = arr[end, :]
    for i in 1:xDim
        j = 1
        while j < yDim
            if arr[j, i] == 'v'
                jN = wrapR(j+1, yDim)
                if arr[jN, i] == '.'
                    changed = true
                    arr[j, i] = '.'
                    arr[jN, i] = 'v'
                    j += 1
                end
            end
            j += 1
        end
        if lastRow[i] == 'v' && firstRow[i] == '.'
            changed = true
            arr[yDim, i] = '.'
            arr[1, i] = 'v'
        end
    end
    return (arr, changed) 
end

stepOnce!(arr) = begin
    (arr, c1) = stepX!(arr)
    (arr, c2) = stepY!(arr)
    return (arr, c1 || c2) 
end

stepN(arr) = begin
    step = 1
    (arr, c) = stepOnce!(arr)
    while c
        (arr, c) = stepOnce!(arr)
        step += 1
    end
    return step
end

mat = getInput()
stepN(mat)