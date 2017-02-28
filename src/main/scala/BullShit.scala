/**
 * Created by nathan on 1/13/17.
 * just for fun
 */
class BullShit(val rounds: Counter, gameTimer: GameTimer) extends Iterable[String] {

  private var currentBullShit: String = BullShit.getBullshit
  private val showForSeconds = 10
  private var showForRounds = showForSeconds

  // for some reason when debugging tests, Bullshit.size is called - i don't know why but it goes into an infinite loop
  // providing a useless implementation here as a hack.  maybe someday i will learn why this is happening
  override def size = 0

  private def setShowForRounds() = {
    // how many rounds should we show this bullshit?
    val roundsPerSecond = math.floor(rounds.value / gameTimer.elapsedSeconds).toInt
    showForRounds = showForSeconds * roundsPerSecond
  }

  def iterator = new Iterator[String] {
    def hasNext = true
    def next: String = {

      // val current = showForRounds
      showForRounds -= 1
      val newBullShit = showForRounds == 0

      if (newBullShit) {
        // only update rounds per second every time you get a new one
        // so that it establishes a consistent time for however fast the program is currently running
        setShowForRounds()
        currentBullShit = BullShit.getBullshit
      }

      // (  if (newBullShit) "new: " + currentBullShit else "old: " + currentBullShit ) + " " + roundsPerSecond + " " + (rounds.value % (showForSeconds * roundsPerSecond))
      /*StringFormats.CYAN +*/ currentBullShit/* + ("." * current)*/ /*+ StringFormats.SANE*/
    }
  }
}

object BullShit {

  def getBullshit: String = {

    def getBS(a: Array[String]): String = a(scala.util.Random.nextInt(a.length))

     getBS(verbs) + " " + getBS(adjectives) + " " + getBS(nouns) + " " + getBS(prepositions) + " " + getBS(endingPhrases)

  }

  private val endingPhrases = Array(
    "expanding the business",
    "generating fomo",
    "breaking down silos",
    "wrapping our heads around uncertainty",
    "faking it until making it",
    "enhancing data driven decision making",
    "driving a win-win",
    "being a team player",
    "circling back",
    "thinking outside the box",
    "increasing bandwidth",
    "right-sizing it",
    "managing the optics",
    "enabling creative destruction",
    "getting boots on the ground",
    "dealing with it",
    "getting our house in order",
    "answering the $64,000 question",
    "taking a 30,000ft view",
    "squaring the circle",
    "milking the cash cow",
    "synergizing",
    "stepping up to the plate",
    "eating our own dog food",
    "monetizing",
    "strategizing",
    "doing a SWOT analysis",
    "riding the perfect storm",
    "putting lipstick on a pig",
    "setting a stake in the ground",
    "focusing on the customer",
    "driving results",
    "improving visibility",
    "sticking to our core competency",
    "facing the headwinds",
    "doing a one-off",
    "being a change agent",
    "doing the due diligence",
    "leaving money on the table",
    "tearing down internal silos",
    "moving the needle",
    "putting a face to a name",
    "employing the 80/20 rule",
    "managing expectations",
    "driving deliverables",
    "moving forward",
    "demonstrating an open door policy",
    "making it viral",
    "burning the candle on both ends",
    "adding value",
    "creating the go-to-market plan",
    "drinking from the fire hose",
    "doing a level-set",
    "blocking and tackling",
    "avoiding getting thrown under the bus",
    "closing the loop",
    "following up on next steps",
    "picking the low-hanging fruit",
    "getting a few quick wins",
    "addressing the elephant in the room",
    "standardizing best practices",
    "looping you in",
    "moving it up and to the right",
    "going through a re-org",
    "increasing customer mindshare",
    "comparing apples to apples",
    "peeling the onion",
    "phoning it in"

  )

  private val prepositions = Array(
    "by",
    "through",
    "while"
  )

  private val verbs = Array(
    "activating",
    "aggregating",
    "architecting",
    "benchmarking",
    "branding",
    "cultivating",
    "delivering",
    "deploying",
    "disintermediating",
    "driving",
    "embracing",
    "empowering",
    "enabling",
    "engaging",
    "engineering",
    "enhancing",
    "evolving",
    "expediting",
    "exploiting",
    "extending",
    "facilitating",
    "fucking up",
    "generating",
    "growing",
    "harnessing",
    "implementing",
    "incentivizing",
    "incubating",
    "innovating",
    "integrating",
    "iterating",
    "leveraging",
    "matrixing",
    "maximizing",
    "meshing with",
    "mirroring",
    "monetizing",
    "morphing",
    "optimizing",
    "orchestrating",
    "productizing",
    "redefining",
    "reinventing",
    "repurposing",
    "revolutionizing",
    "scaling",
    "strategizing",
    "streamlining",
    "syndicating",
    "synergizing",
    "targeting",
    "transforming",
    "transitioning",
    "unleashing",
    "visualizing",
    "whiteboarding"
  )

  private val adjectives = Array(
    "24/7",
    "agile",
    "B2B",
    "B2C",
    "anarchic",
    "back-end",
    "best-of-breed",
    "bleeding-edge",
    "collaborative",
    "compelling",
    "cross-channel",
    "customized",
    "cutting-edge",
    "distributed",
    "dot-com",
    "dynamic",
    "efficient",
    "end-to-end",
    "enterprise",
    "extensible",
    "facebook",
    "frictionless",
    "frivolous",
    "front-end",
    "global",
    "granular",
    "holistic",
    "impactful",
    "innovative",
    "integrated",
    "interactive",
    "intuitive",
    "killer",
    "leading-edge",
    "lean",
    "magnetic",
    "mission-critical",
    "next-generation",
    "one-to-one",
    "open-source",
    "out-of-the-box",
    "plug-and-play",
    "proactive",
    "real-time",
    "revolutionary",
    "robust",
    "scalable",
    "seamless",
    "sexy",
    "sticky",
    "social",
    "strategic",
    "synergistic",
    "transparent",
    "turn-key",
    "ubiquitous",
    "user-centric",
    "value-added",
    "vertical",
    "viral",
    "virtual",
    "visionary",
    "web-enabled",
    "wireless",
    "world-class"
  )

  private val nouns = Array(
    "action-items",
    "AI",
    "algorithms",
    "applications",
    "architectures",
    "bandwidth",
    "blogispheres",
    "channels",
    "cloud infrastructure",
    "communities",
    "content",
    "convergence",
    "deliverables",
    "experiences",
    "eyeballs",
    "fake news stories",
    "infrastructures",
    "initiatives",
    "interfaces",
    "internet of things",
    "machine learning",
    "markets",
    "methodologies",
    "metrics",
    "media",
    "mindshare",
    "models",
    "networks",
    "neural networks",
    "optimizations",
    "paradigms",
    "partnerships",
    "platforms",
    "relationships",
    "ROI",
    "synergies",
    "solutions",
    "startups",
    "systems",
    "technologies",
    "tweets",
    "users",
    "walled gardens",
    "web services"
  )

}
