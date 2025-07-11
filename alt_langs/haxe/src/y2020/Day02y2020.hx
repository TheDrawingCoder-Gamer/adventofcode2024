package y2020;

import sys.io.File;

using StringTools;


typedef PolicyEntry = {
    var min: Int;
    var max: Int;
    var char: String;
    var password: String;
};

class Day02y2020 {

    static public function parse(str: String): Array<PolicyEntry> {
        var policies: Array<PolicyEntry> = [];
	for (line in str.trim().split("\n")) {
	    final reg =  ~/(\d+)-(\d+) ([a-z]): ([a-z]+)/;
	    final goodLine = line.trim();
	    reg.match(goodLine);
	    final entry: PolicyEntry = {
		min: Std.parseInt(reg.matched(0)), 
		max: Std.parseInt(reg.matched(1)), 
		char: reg.matched(3), 
		password: reg.matched(4)
	    };
	    policies.push(entry);
	}
	return policies;
    }

    static public function part1(policies: Array<PolicyEntry>): Int {
	var count = 0;
	for (entry in policies) {
	    final size = entry.password.split("").filter((it) -> it == entry.char).length;
	    if (size >= entry.min && size <= entry.max) {
		count += 1;
	    }
	}
	return count;
    }

    static public function part2(policies: Array<PolicyEntry>): Int {
	var count = 0;
	for (entry in policies) {
	    final l = entry.password.charAt(entry.min - 1) == entry.char;
	    final r = entry.password.charAt(entry.max - 1) == entry.char;
	    // xor
	    if ((l || r) && !(l && r)) {
		count += 1;
	    }
	}
	return count;
    }

    static public function main() {
	final file = File.getContent("core/shared/src/main/resources/y2020/day02.txt");
	final data = parse(file);
	trace(part1(data));
	trace(part2(data));
    }
}
