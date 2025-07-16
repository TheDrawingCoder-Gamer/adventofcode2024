content = File.read("core/shared/src/main/resources/y2020/day05.txt")

def parse(input)
  lines = input.strip().split("\n")
  values = []
  for line in lines do
    line = line.strip()

    line.gsub!(/F/, "0")
    line.gsub!(/B/, "1")
    line.gsub!(/L/, "0")
    line.gsub!(/R/, "1")

    values.push(line.to_i 2)
  end
  return values
end

def part1(input) 
  return input.max
end

def part2(input)
  resWindow = input.sort.each_cons(2).detect { |chunk| chunk[1] - chunk[0] > 1 }
  return resWindow[1] - 1
end

data = parse(content)
puts part1(data)
puts part2(data)
