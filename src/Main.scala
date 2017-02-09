/**
 * Created by nathan on 12/9/16.
 * Main is just the stub to get it all off the ground
 * renamed object to simulate so when sbt/packInstall is run
 * you can run this from ~/local/bin/simulate
 */
//noinspection ScalaFileName

object simulate {

  def main(args: Array[String]): Unit = {

    val context = new Context(new Conf(args))

    if (context.generateWeightsGamesToPlay > 0)
      GameRunner.generateWeights(context)
    else
      GameRunner.play(context)

  }

}

