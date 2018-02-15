/**
  * Created by David on 2017-01-28.
  */

/**
  * Let us see what goes wrong when a stream cipher key is used more than once. Below are eleven hex-encoded
  * ciphertexts that are the result of encrypting eleven plaintexts with a stream cipher, all with the same stream
  * cipher key. Your goal is to decrypt the last ciphertext, and submit the secret message within it as solution.
  * *
  * Hint: XOR the ciphertexts together, and consider what happens when a space is XORed with a character in [a-zA-Z].
  */
object OneTimePad {

  // identify some number of

  // create a random value that will cover the

  // just XOR the
  // Below are eleven hex-encoded ciphertexts that are the result of encrypting eleven plaintexts with a stream cipher
  // encrypt plaintext with a stream cipher to produce a hex-encoded cipher text

  // Hex

  // 0000 = 0
  // 0001 = 1
  // 0010 = 2
  // 0011 = 3
  // 0100 = 4
  // 0101 = 5
  // 0110 = 6
  // 0111 = 7
  // 1000 = 8
  // 1001 = 9
  // 1010 = a
  // 1011 = b
  // 1100 = c
  // 1101 = d
  // 1110 = e
  // 1111 = f


  def extractKey(i: Int, a1: Array[Integer], a2: Array[Integer], encodedMessage: Array[Integer], keys: Array[Integer], conflictingKeys: Array[List[Integer]]): Boolean = {
    val c1 = a1(i)
    val c2 = a2(i)
    val space = Integer.valueOf("20", 16)
    val result = c1 ^ c2 ^ space

    println("---")
    printValues("c1", c1)
    printValues("c2", c2)
    printValues("space", space)
    printValues("result", result)

    if (isAlpha(result)) {
      println(s"\tindex: ${i}")
      val k1 = c1 ^ space
      val k2 = c2 ^ space
      val k3 = c1 ^ result
      val k4 = c2 ^ result

      printValues("k1", k1)
      printValues("k1 ^ c1", k1 ^ c1)
      printValues("k1 ^ c2", k1 ^ c2)

      printValues("k2", k2)
      printValues("k2 ^ c1", k2 ^ c1)
      printValues("k2 ^ c2", k2 ^ c2)

      if (i < encodedMessage.length) {
        val testValue1 = k1 ^ encodedMessage(i)
        printValues("k1", k1)
        printValues("k1 ^ a3(i)", k1 ^ encodedMessage(i))

        val testValue2 = k2 ^ encodedMessage(i)
        printValues("k2", k2)
        printValues("k2 ^ a3(i)", k2 ^ encodedMessage(i))

        if (isAlpha(testValue1) && isAlpha(testValue2)) {
          throw new Exception("Two possible keys")
        } else if (isAlpha(testValue1)) {
          setKey(i, keys, k1, encodedMessage, conflictingKeys)
        } else if (isAlpha(testValue2)) {
          setKey(i, keys, k2, encodedMessage, conflictingKeys)
        } else if (isAlphaOrSpace(testValue1)) {
          println("------------------------ isSpace!!")
          setKey(i, keys, k1, encodedMessage, conflictingKeys)
        } else if (isAlphaOrSpace(testValue2)) {
          println("------------------------ isSpace!!")
          setKey(i, keys, k2, encodedMessage, conflictingKeys)
        } else {
          println("------------------------ No key set")
        }
      }
    }
    true
  }

  def setKey(i: Integer, keys: Array[Integer], k: Integer, m: Array[Integer], conflictingKeys: Array[List[Integer]]): Unit = {
    if (keys(i) == 0 || keys(i) == k) keys(i) = k
    else {
      println("key conflict >")
      printValues("k", k)
      printValues(s"k ^ m(${i})", k ^ m(i))
      printValues(s"keys(${i})", keys(i))
      printValues(s"keys(${i}) ^ m(${i})", keys(i) ^ m(i))
      conflictingKeys(i) = conflictingKeys(i) :+ k
      println("< key conflict")
    }
  }

  def printValues(message: String, i: Integer): Unit = {
    println(f"${message}%-20s ${i}%3d ${Integer.toHexString(i)}%2s ${format(i)}%8s ${Character.toChars(i)(0)}")
  }

  def isAlpha(b: Integer): Boolean = {
    (b >= 65 && b <= 90) || (b >= 97 && b <= 122)
  }

  def isAlphaOrSpace(b: Integer): Boolean = {
    isAlpha(b) || b == 32
  }


  def convert(s: String): Array[Integer] = {
    if (s.length % 2 != 0) throw new Exception("String is does not contain an even number of characters")
    else {
      val a = new Array[Integer](s.length / 2)
      for {
        i <- 0 until s.length by 2
      } yield {
        val pair = s.substring(i, i + 2)
        a(i / 2) = Integer.valueOf(pair.toString, 16)
      }
      a
    }
  }

  def main(args: Array[String]): Unit = {


    //    val cipherText = new Array[String](10)
    val cipherText = new Array[String](11)
    cipherText(0) = "315c4eeaa8b5f8aaf9174145bf43e1784b8fa00dc71d885a804e5ee9fa40b16349c146fb778cdf2d3aff021dfff5b403b510d0d0455468aeb98622b137dae857553ccd8883a7bc37520e06e515d22c954eba5025b8cc57ee59418ce7dc6bc41556bdb36bbca3e8774301fbcaa3b83b220809560987815f65286764703de0f3d524400a19b159610b11ef3e"
    //cipherText(0) = "315c4eeaa8b5f8"
    cipherText(1) = "234c02ecbbfbafa3ed18510abd11fa724fcda2018a1a8342cf064bbde548b12b07df44ba7191d9606ef4081ffde5ad46a5069d9f7f543bedb9c861bf29c7e205132eda9382b0bc2c5c4b45f919cf3a9f1cb74151f6d551f4480c82b2cb24cc5b028aa76eb7b4ab24171ab3cdadb8356f"
    //cipherText(1) = "234c02ecbbfbaf"
    cipherText(2) = "32510ba9a7b2bba9b8005d43a304b5714cc0bb0c8a34884dd91304b8ad40b62b07df44ba6e9d8a2368e51d04e0e7b207b70b9b8261112bacb6c866a232dfe257527dc29398f5f3251a0d47e503c66e935de81230b59b7afb5f41afa8d661cb"
    cipherText(3) = "32510ba9aab2a8a4fd06414fb517b5605cc0aa0dc91a8908c2064ba8ad5ea06a029056f47a8ad3306ef5021eafe1ac01a81197847a5c68a1b78769a37bc8f4575432c198ccb4ef63590256e305cd3a9544ee4160ead45aef520489e7da7d835402bca670bda8eb775200b8dabbba246b130f040d8ec6447e2c767f3d30ed81ea2e4c1404e1315a1010e7229be6636aaa"
    cipherText(4) = "3f561ba9adb4b6ebec54424ba317b564418fac0dd35f8c08d31a1fe9e24fe56808c213f17c81d9607cee021dafe1e001b21ade877a5e68bea88d61b93ac5ee0d562e8e9582f5ef375f0a4ae20ed86e935de81230b59b73fb4302cd95d770c65b40aaa065f2a5e33a5a0bb5dcaba43722130f042f8ec85b7c2070"
    cipherText(5) = "32510bfbacfbb9befd54415da243e1695ecabd58c519cd4bd2061bbde24eb76a19d84aba34d8de287be84d07e7e9a30ee714979c7e1123a8bd9822a33ecaf512472e8e8f8db3f9635c1949e640c621854eba0d79eccf52ff111284b4cc61d11902aebc66f2b2e436434eacc0aba938220b084800c2ca4e693522643573b2c4ce35050b0cf774201f0fe52ac9f26d71b6cf61a711cc229f77ace7aa88a2f19983122b11be87a59c355d25f8e4"
    cipherText(6) = "32510bfbacfbb9befd54415da243e1695ecabd58c519cd4bd90f1fa6ea5ba47b01c909ba7696cf606ef40c04afe1ac0aa8148dd066592ded9f8774b529c7ea125d298e8883f5e9305f4b44f915cb2bd05af51373fd9b4af511039fa2d96f83414aaaf261bda2e97b170fb5cce2a53e675c154c0d9681596934777e2275b381ce2e40582afe67650b13e72287ff2270abcf73bb028932836fbdecfecee0a3b894473c1bbeb6b4913a536ce4f9b13f1efff71ea313c8661dd9a4ce"
    cipherText(7) = "315c4eeaa8b5f8bffd11155ea506b56041c6a00c8a08854dd21a4bbde54ce56801d943ba708b8a3574f40c00fff9e00fa1439fd0654327a3bfc860b92f89ee04132ecb9298f5fd2d5e4b45e40ecc3b9d59e9417df7c95bba410e9aa2ca24c5474da2f276baa3ac325918b2daada43d6712150441c2e04f6565517f317da9d3"
    cipherText(8) = "271946f9bbb2aeadec111841a81abc300ecaa01bd8069d5cc91005e9fe4aad6e04d513e96d99de2569bc5e50eeeca709b50a8a987f4264edb6896fb537d0a716132ddc938fb0f836480e06ed0fcd6e9759f40462f9cf57f4564186a2c1778f1543efa270bda5e933421cbe88a4a52222190f471e9bd15f652b653b7071aec59a2705081ffe72651d08f822c9ed6d76e48b63ab15d0208573a7eef027"
    cipherText(9) = "466d06ece998b7a2fb1d464fed2ced7641ddaa3cc31c9941cf110abbf409ed39598005b3399ccfafb61d0315fca0a314be138a9f32503bedac8067f03adbf3575c3b8edc9ba7f537530541ab0f9f3cd04ff50d66f1d559ba520e89a2cb2a83"
    cipherText(10) = "32510ba9babebbbefd001547a810e67149caee11d945cd7fc81a05e9f85aac650e9052ba6a8cd8257bf14d13e6f0a803b54fde9e77472dbff89d71b57bddef121336cb85ccb8f3315f4b52e301d16e9f52f904"
    //cipherText(10) = "32510ba9babebb"
    // val hb = XOR cipherText(i)(j) and cipherText(i+1)(j) and '0010'
    // val lb = XOR cipherText(i)(j+1) and cipherText(i+1)(j+1) and '0000'
    // if char(hb,lb) is within [a-zA-Z] then we know that either cipherText(i) or cipherText(i+1) contains a space
    val maxLength = cipherText.map(_.length).max

    val m = convert(cipherText(10))
    val keys = new Array[Integer](maxLength / 2)
    for {i <- 0 until keys.length} yield {
      keys(i) = 0
    }
    val conflictingKeys: Array[List[Integer]] = new Array[List[Integer]](maxLength / 2)
    for {i <- 0 until conflictingKeys.length} yield {
      conflictingKeys(i) = List()
    }

    println("==")
    for {
      i <- 0 until m.length
    } yield {
      printValues("Message: ", m(i))
    }

    println(m.mkString(" "))

    for {
      i <- 0 until 10
    } yield {
      println(s"cipherText(${i}).length= ${cipherText(i).length}")
    }

    /*    for {
          i<- 0 until (cipherText.length-1)
        } yield {
          val a1 = convert(cipherText(i))
          val a2 = convert(cipherText(i+1))
          for {
            i <- 0 until Math.min(a1.length, a2.length)
          } yield {
            extractKey(i, a1, a2, m, keys, conflictingKeys)
          }
          println("i=" + i + " > " + keys.mkString(" "))
        }
    */
    for {
      i <- 0 until (cipherText.length - 1)
      j <- 0 until (cipherText.length - 1)
      if (i != j)
    } yield {
      val a1 = convert(cipherText(i))
      val a2 = convert(cipherText(j))
      for {
        i <- 0 until Math.min(a1.length, a2.length)
      } yield {
        extractKey(i, a1, a2, m, keys, conflictingKeys)
      }
      println("i=" + i + " > " + keys.mkString(" "))
    }

    println("Length: " + cipherText(10).length)
    println("Length: " + cipherText(10).length / 2)
    println(keys.mkString(" "))
    var message = new String()
    for {
      i <- 0 until cipherText(10).length / 2
    } yield {
      //      println(format(keys(i)))
      //      printValues("Key: ", keys(i))
      if (keys(i) > 0 && conflictingKeys(i).isEmpty) {
        val c = keys(i) ^ m(i)
        printValues("Out: ", c)
        message = message + Character.toChars(c)(0)
      } else {

        val numOfOptions = 1 + conflictingKeys(i).length
        val options: Array[String] = new Array[String](numOfOptions)
        options(0) = "" + Character.toChars(keys(i) ^ m(i))(0)
        for {j <- 1 until options.length} yield {
          options(j) = "" + Character.toChars(conflictingKeys(i)(j - 1) ^ m(i))(0)
        }
        println("Conflict: " + options.mkString(" "))
        message = message + ' '
      }
    }
    println("conflictingKeys")
    conflictingKeys.foreach(println(_))
    println("message: " + message)

    System.exit(0)

  }

  def print(i: Int, message: Array[Byte], key: Array[Byte], cipher: Array[Byte]): Unit = {
    println(s"Index: ${i}")
    print(i, message)
    print(i, key)
    print(i, cipher)
  }

  def print(i: Int, ba: Array[Byte]): Unit = {
    println(f"${ba(i).toChar} byte: ${format(ba(i))} ${ba(i)}%03d")
  }


  def constructKey(l: Int): Array[Byte] = {
    var k: Array[Byte] = new Array[Byte](l)
    (0 until l).foreach(i => k(i) = scala.util.Random.nextInt(127).toByte)
    k
  }

  def format(i: Integer): String = {
    var s = Integer.toString(i, 2)
    for {
      i <- s.length to 7
    } yield {
      s = '0' + s
    }
    s
  }

  def format(b: Byte): String = {
    var s = Integer.toString(b, 2)
    for {
      i <- s.length to 7
    } yield {
      s = '0' + s
    }
    s
  }

  def format(b: Byte, l: Int): String = {
    var s = Integer.toString(b, 2)
    for {
      i <- s.length to l
    } yield {
      s = '0' + s
    }
    s
  }

}
