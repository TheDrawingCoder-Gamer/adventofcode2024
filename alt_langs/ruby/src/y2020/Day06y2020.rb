require 'set'

content = File.read('core/shared/src/main/resources/y2020/day06.txt')

def parse(input)
  input.strip.split("\n\n").map { |block|
    block.strip.split("\n").map { |person| 
      person.strip.chars.to_set
    }
  }
end

def part1(input)
  input.map { |block| 
    block.reduce { |l, r| l | r }.size
  }.sum
end

def part2(input)
  input.map { |block|
    block.reduce { |l, r| l & r }.size
  }.sum
end

data = parse(content)

puts part1(data)
puts part2(data)
