package y2020;

import sys.io.File;
import haxe.Int64;
using StringTools;

class Day03 {
    static function parse(str: String): Array<Array<Bool>> {
	
       return [for (line in str.trim().split("\n")) {
	    line = line.trim();
	    [for (char in line.split("")) char == "#"];
	}];
    }

    static function testSlope(input: Array<Array<Bool>>, slopeX: Int, slopeY: Int): Int64 {
	var xPos = 0;
	var yPos = 0;
	var treeBonk = 0;
	while (yPos + slopeY < input.length) {
	    xPos += slopeX;
	    xPos %= input[0].length;
	    yPos += slopeY;

	    if (input[yPos][xPos]) {
		treeBonk++;
	    }
	}

	return treeBonk;
    }

    static function part1(input: Array<Array<Bool>>): Int64 {
	return testSlope(input, 3, 1);
    }

    static function part2(input: Array<Array<Bool>>): Int64 {
	final slopes = [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]];
	var res: Int64 = 1;
	for (slope in slopes) {
	    res *= testSlope(input, slope[0], slope[1]);
	}
	return res;
    }

    static public function main() {
	final file = File.getContent("core/shared/src/main/resources/y2020/day03.txt");
	final data = parse(file);
	
	trace(part1(data));
	trace(part2(data));
    }
}
