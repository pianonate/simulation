/**
 * Created by nathan on 12/9/16.
 * Main is just the stub to get it all off the ground
 * renamed object to simulate so when sbt/packInstall is run
 * you can run this from ~/local/bin/simulate
 *
 * i haven't resolved the issue that i have a class name taht i like called simulation
 * so while the whole project is called simulation, you have to run it with
 *
 * $ simulate
 *
 * at the command line otherwise a module called simulation would shadow the class called simulation
 * todo - think about a new name for simulation class
 */

import org.slf4j.LoggerFactory
import ch.qos.logback.classic.LoggerContext

//noinspection ScalaFileName

object simulate {


  def main(args: Array[String]): Unit = {

    val context = new Context(new Conf(args))

    sys.addShutdownHook(
      {

      context.logger.info("stopping simulation")
      val loggerContext: LoggerContext  = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
      loggerContext.stop()
      println("goodbye - i hoped you enjoyed this simulation")
    }
    )

    if (context.generateWeightsGamesToPlay > 0)
      GameRunner.generateWeights(context)
    else
      GameRunner.play(context)




  }




}

