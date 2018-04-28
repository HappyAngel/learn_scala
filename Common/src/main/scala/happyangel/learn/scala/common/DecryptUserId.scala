package happyangel.learn.scala.common

import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by xionglei on 18-4-9.
  */
object DecryptUserId {
    private val searchPattern = new Regex("userId: ([0-9]+), nickName: (.*), encryptedUserId: (.*)$", "userId", "nickName", "encryptedUserId")

   def decryptUserId(logDataFilePathList: List[String], encryptedUserIdList: List[(String, Int)]) = {
        val encryptedUserIdToUserIdMap: Map[String, (String, String)] = logDataFilePathList.flatMap { logFile =>
                Source.fromFile(logFile).getLines().flatMap { line =>
                    searchPattern.findAllMatchIn(line).map { mat =>
                        (mat.group("encryptedUserId").trim, (mat.group("userId"), mat.group("nickName")))
                    }
                }
            }.toMap

       encryptedUserIdList.map {
           case (k: String, v: Int) =>
               val res = encryptedUserIdToUserIdMap.get(k)
               if (res.isDefined) {
                   (res.get._1, res.get._2, v)
               } else {
                   ("None", "", v)
               }
       }
   }

}

object Run extends App {
    val encryptedUserIdList = List(
        "jvXnNV0nXHcqM0wQ4oY2kA==",
        "aeFDK/GyJS2E2S1m/ZLmxQ==",
        "I73QfiOxEHstQvW6XKhbDQ==",
        "lqBehnvCA7RwwTZniHM/hA==",
        "f/jbQNcXnqYbzhHEk2omdQ==",
        "OmgyZ0GEnGoNUb7wz+THKg==",
        "p5Z0TMihyT0n5MizDDiVdA==",
        "rsYa6sYQxf0NEZcya+sV/w==",
        "xlMvfXD7Vgri7WmYI5Mlxg==",
        "UzwmjoBOlPgWB0xsLF2qrw==",
        "Bwq+CviCDzCfyPMqAdYnRQ==",
        "hJoE3R/Jlx4ZxFvmEcDS+g==",
        "ILggNJdMa+RWvY03Q5iOag==",
        "X2ZIM557txOOzDm8iW7/aA==",
        "i23DJ/xKtJg+KyOEz2YvTA==",
        "s8E4FiY+ieIsIuxcKpC0dg==",
        "sDvs3mRp4FLMRfFbFlyaMA==",
        "hedQt2fLap4eOq6dxrm3UQ==",
        "2OqYjU4czUCfJ8Qkk/9Iig==",
        "s+3SoC/HihgzG4tsfuKXXA==",
        "Fh5VREv8WWOxKSbFvhFnhQ==",
        "lciINj8KvDuGQpXl59MTPQ==",
        "t69R0F94nGHiPKgT3Izo6w==",
        "YAiCZSBICXkyfV+8LHvPxA==",
        "MvI9qo9H/n12wRmW730p1A==",
        "F539xAlK4L5wcDoxUCoqtA==",
        "O0DQwWClsMyHOpn4ZEQs/g==",
        "T/rAAOJaGhgNVTroughuVg==",
        "Q1KiCExOl8kU3xSdK2GmFw==",
        "6ceB+Y8fjlC55w40z3ZDxQ==",
        "8FvrDYo2cNOxaS7k6UA7+g==",
        "1aTa/k9Ul1twuGfUAJJozQ==",
        "pwi5vyM+5LS94MHEgjAK/A==",
        "CWa9wA0DKV0MjRnsvNv11g==",
        "yXILBcqyRGf457dLgxA3jA==",
        "48bh7QSBOa1p07xNIMjE0Q==",
        "97IOMOHemObSsGbbgbvT0Q==",
        "zKafhwwM5fx7vYa9mnc/JQ==",
        "r4A1a7isfayvEGbje87kbQ==",
        "rqOBO/1WcsnMGQAgII0Zig==",
        "YPmkaTrTkMlyuSHBYwF+tg==",
        "Tv2cQeTa5ek+La8wd3Nwpg==",
        "j7Dyg7HGNAa+kwwR4iou/Q==",
        "9K/NMk/bUslK0iz0UXSjuQ==",
        "IDalwsjwyKvKMC5HFRLkEg==",
        "7dnridCcbkjpCX0Sh16WSQ==",
        "RVB8/DMvKTr48GDvb80XWA==",
        "n0GUXuDpZNAvliu53CASWA==",
        "9n57NDXOjgLHHV/Uw/CmrA==",
        "qTuaDzRJtLSCextBve3O8g==",
        "7lbr+7WpWdHyim+1Ms24DA==",
        "y52mYy5mOzQGs29y9slQVQ==",
        "vv6vNfSKH31QV9iVR/4yiQ==",
        "dqK/RIHTpx2ccrLxFKR61g==",
        "lKFD7ytdBLlpnMcnW6XISg==",
        "2efPXULRygoLQx5NJddCEg==",
        "YLguqS7DR/+YEC9VsGmZDQ==",
        "v9gTUHVFjsYZvdwqTBaHdw==",
        "fVbkP6hbG2qxv2mI0fkr6w==",
        "JVDv8U+MTnNhi5QgVRrtfw==",
        "+1+M1zoFSkexab0/hskYyg==",
        "MLDKNXuq5eQdEdUtFzwxvg==",
        "6YPhYXvu9V5xu/zK60Emxw==",
        "r58Nt4rT8sGmERbZP3RuIg==",
        "MdLy14obeYqOMouN6WRsLw==",
        "OSy4Vxx9Eo8IswzyFRTN/A==",
        "V1HhINvKxLU+7F62fVpxYQ==",
        "LJHgKmNPGSv3axPyptq6Uw==",
        "puKUpGLdKjoGVpkQSBrFFw==",
        "nLHlotVrd6+AjnChYBcWnA==",
        "ASdZYY1ymqmwk0FXyJ4tmA==",
        "NM1wg0YHRhqiWf5JnuZBow==",
        "6+Pws0fpsGp0AyC5JCwzuw==",
        "2OZlY1QOXhzJ7MbNZRJyyA==",
        "UN4Ts3f1XFGCWrnphR7PVg==",
        "J9DcJSany2gjbNp7P2Kgtg==",
        "Ki1KinZmtg6/fe+afgTllg==",
        "ZnI/Uz2Vedt9k2TxzlSCdw==",
        "OZYoM3tb04uO066nkTbDkg==",
        "nK4/uIvPGAo/Y8FqMb8ioQ=="
    )

    val res = DecryptUserId.decryptUserId(List("/home/mi/c3_1_4-10.log", "/home/mi/c3_1_4-11.log", "/home/mi/c4_1_4-10.log", "/home/mi/c4_1_4-11.log", "/home/mi/c3_2_4-10.log", "/home/mi/c3_2_4-11.log", "/home/mi/c4_2_4-10.log", "/home/mi/c4_2_4-11.log"), encryptedUserIdList.zipWithIndex).filter { case (k,_, _) => k != "None"}
    println(res.map { case (k,nickName, index) =>
            s"$k\t$nickName\t$index"
    }.mkString("\n"))
}
