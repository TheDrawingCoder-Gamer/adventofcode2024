defmodule Day01 do
  def parse_int!(str) do
    case Integer.parse(str) do
      { v, "" } -> v
      :error -> nil
    end
  end
  def measurement_basic(list) do
    Enum.chunk_every(list, 2, 1, :discard) 
      |> Enum.count(fn a -> Enum.at(a, 0) < Enum.at(a, 1) end)
  end
  def part1(list) do
    measurement_basic(list)
  end
  def sliding_sum(list) do
    Enum.chunk_every(list, 3, 1, :discard)
      |> Enum.map(fn a -> Enum.sum(a) end)
  end
  def part2(list) do
    list |> sliding_sum |> measurement_basic 
  end
  def parse(str) do
    for line <- String.split(String.trim(str), "\n"), 
      line != "", 
      do: Day1.parse_int!(String.trim(line))
  end
end

input = File.read!("core/shared/src/main/resources/y2021/day01.txt")
data = Day01y2021.parse(input)

IO.puts(Day01.part1(data))
IO.puts(Day01.part2(data))
