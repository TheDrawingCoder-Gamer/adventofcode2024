package gay.menkissing.bench.util

def fuzzyEquals(l: Double, r: Double, by: Double): Boolean = {
  math.abs(r - l) <= by
}
