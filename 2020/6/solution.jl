begin
  groups = split(strip(read("input.txt", String)), "\n\n")

  groupanswers = group -> length(filter(isletter, unique(group)))

  println(mapreduce(groupanswers, +, groups))

  function sharedanswers(group)
    g = string.(split(group, '\n'))
    if length(g) == 1
      return groupanswers(g[1])
    else
      return count(letter -> all(contains(letter), g[2:end]), g[1])
    end
  end

  println(mapreduce(sharedanswers, +, groups))
end
