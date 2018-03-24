package happyangel.learn.scala.redis

import com.lambdaworks.redis.RedisClient
import com.lambdaworks.redis.pubsub.RedisPubSubAdapter

/**
  * Created by LeiXi on 2018/3/24.
  */
object Main extends App{

  val redisClient = RedisClient.create("redis://@localhost:6379/0")
  val connection = redisClient.connectPubSub()

  connection.addListener(new Listener)

  val sync = connection.sync()
  sync.subscribe("fool")

  Thread.sleep(100000)
  connection.close()
  redisClient.shutdown()
}

class Listener extends RedisPubSubAdapter[String, String] {
  override def message(channel: String, message: String): Unit = {
    println(s"Channel: $channel received message: $message")
  }
}
