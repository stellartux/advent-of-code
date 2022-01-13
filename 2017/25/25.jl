if basename(pwd()) == "aoc"
    cd("2017/25")
end

function transpile(filename::AbstractString)
    """
    function turingmachine()
        cursor = 0
        tape = Set{Int}()
    """ *
    replace(readchomp(filename),
        r"Begin in state (.)\." => s"    state = '\1'",
        r"Perform a diagnostic checksum after (\d+) steps\." => s"    for _ in 1:\1",
        r"In state (.):" => s"if state == '\1'",
        "If the current value is 0:" => "      if !(cursor in tape)",
        "If the current value is 1:" => "      else",
        "- Write the value 1." => "        push!(tape, cursor)",
        "- Write the value 0." => "        delete!(tape, cursor)",
        "- Move one slot to the left." => "        cursor -= 1",
        "- Move one slot to the right." => "        cursor += 1",
        r"- Continue with state (.)\." => s"        state = '\1'",
        r"(?<!steps\.)\r?\n\r?\n" => "\n        end\n    else",
        r"(?!steps\.)\r?\n\r?\n" => "\n\n        "
    ) *
    "\n        end\n    end\n    \nend\n    length(tape)\nend"
end

part1(filename::AbstractString) = eval(Meta.parse(transpile(filename)))()
