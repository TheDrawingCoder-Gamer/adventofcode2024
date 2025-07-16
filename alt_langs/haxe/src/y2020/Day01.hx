package y2020;

import sys.io.File;
using StringTools;
class Day01 {
	static public function part1(ls: Array<Int>): Int {
		for (xi => x in ls) {
			for (yi => y in ls) {
				if (xi != yi && x + y == 2020) {
					return x * y;
				}
			}
		}
		return -1;
	}
	static public function part2(ls: Array<Int>): Int {
		for (xi => x in ls) {
			for (yi => y in ls) {
				if (xi != yi) {
					for (zi => z in ls) {
						if (xi != zi && yi != zi && x + y + z == 2020) {
							return x * y * z;
						}
					}
				}
			}
		}
		return -1;
	}
	static public function parse(str: String): Array<Int> {
		return str.trim().split("\n").map((a) -> Std.parseInt(a.trim()));
	}

	static public function main() {
		final input = File.getContent("core/shared/src/main/resources/y2020/day01.txt");
		final data = parse(input);
		trace(part1(data));
		trace(part2(data));
	}
}
