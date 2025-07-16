package gay.menkissing.hash

import java.security.MessageDigest

object MD5:
  def calc(v: Array[Byte]): Array[Byte] =
    val md = MessageDigest.getInstance("MD5")
    md.digest(v)
    
