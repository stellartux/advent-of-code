if basename(pwd()) != "22"
    cd("2018/22")
end

depth = 510
height = 10
width = 10

erosionlevel(M, y, x) = (M[y, x] + depth) % 20183

M = zeros(UInt, (height + 1, width + 1))
M[:, 1] .= range(start=0, step=16807, length=height + 1)
M[1, :] .= range(start=0, step=48271, length=width + 1)
for j = 2:height+1
    for i = 2:width+1
        M[j, i] = erosionlevel(M, j - 1, i) * erosionlevel(M, j, i - 1)
    end
end
M[height+1, width+1] = 0

R = UInt8[erosionlevel(M, y + 1, x + 1) % 3 for x = 0:width, y = 0:height]

Int(sum(R))
