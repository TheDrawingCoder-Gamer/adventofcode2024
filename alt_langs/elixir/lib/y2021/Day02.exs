defmodule Day02 do
  def parse_int!(str) do
    case Integer.parse(str) do
      {int, ""} -> int
      _ -> nil
    end
  end

  def parse(input) do
    for line <- String.split(String.trim(input), "\n"),
      line != "" do
      parts = String.split(String.trim(line), " ")
      dir = Enum.at(parts, 0)
      num = Enum.at(parts, 1) |> parse_int!
      case dir do
        "forward" -> {:forward, num}
        "up" -> {:up, num}
        "down" -> {:down, num}
      end
    end
  end

  def part1(input) do
    {horz, depth} = input |> Enum.reduce({0, 0}, fn x, {h, d} -> 
      case x do
        {:forward, num} -> {h + num, d}
        {:down   , num} -> {h, d + num}
        {:up     , num} -> {h, d - num}
      end
    end)
    depth * horz
  end

  def calc_depth([], _, depth, x), do: {x, depth}

  def calc_depth([head | tail], aim, depth, x) do
    case head do
      {:forward, num} -> calc_depth(tail, aim, depth + aim * num, x + num)
      {:down   , num} -> calc_depth(tail, aim + num, depth, x)
      {:up     , num} -> calc_depth(tail, aim - num, depth, x)
    end
  end

  def part2(input) do
    {x, depth} = calc_depth(input, 0, 0, 0)
    x * depth
  end
end

input = File.read("core/shared/src/main/resources/y2021/day02.txt")

data = Day02.parse(input)

IO.puts Day02.part1(data)

IO.puts Day02.part2(data)


