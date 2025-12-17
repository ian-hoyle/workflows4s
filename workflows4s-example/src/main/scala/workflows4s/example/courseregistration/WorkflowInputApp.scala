package workflows4s.example.courseregistration

import scala.io.StdIn
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*
import workflows4s.example.courseregistration.CourseRegistrationWorkflow.*
import workflows4s.example.courseregistration.CourseRegistrationWorkflow.Signals.*
import workflows4s.runtime.InMemorySyncRuntime
import workflows4s.wio.internal.DebugRenderer

/**
 * Command-line application to accept and parse JSON input for CourseRegistration workflow.
 */
object WorkflowInputApp extends App {

  // Define expected wrapper type for parsing
  case class InputWrapper[T](
    `type`: String,
    data: T
  )

  // Setup the workflow
  val runtime = InMemorySyncRuntime.default[Context.Ctx](workflow, RegistrationState.Empty)
  val wfInstance = runtime.createInstance("interactive-user")

  // Continue running until terminated
  var running = true

  println("Welcome to CourseRegistration Workflow Input App")
  println("""Enter JSON input in the format: { "type":"BrowsingRequest", "data": { "studentId":"12234", "semester":"Spring"}}""")
  println("""Available types: BrowsingRequest, PriorityRequest { "type":"PriorityRequest", "data": { "courseRequirement":"CR1", "priorities":["CS101-Smith", "CS101-Johnson"]}}""")
  println("Special commands: 'show' to display workflow state, 'exit' to quit")

  // Main loop for accepting user input
  while (running) {
    print("> ")
    val input = StdIn.readLine()

    if (input == null || input.toLowerCase == "exit") {
      running = false
      println("Exiting...")
    } else if (input.toLowerCase == "show") {
      // Show current workflow state
      val debugString          = DebugRenderer.getCurrentStateDescription(wfInstance.getProgress)
      println("=== Current Workflow State ===" + debugString)
      println(s"Current workflow state: ${wfInstance.queryState()}")
    } else {
      processInput(input)
    }
  }

  /**
   * Process the JSON input and handle based on the type
   */
  def processInput(jsonString: String): Unit = {
    // First parse the string to determine the type
    parse(jsonString).flatMap(_.hcursor.get[String]("type")) match {
      case Right("BrowsingRequest") =>
        handleBrowsingRequest(jsonString)
      case Right("PriorityRequest") =>
        handlePriorityRequest(jsonString)
      case Right(unknownType) =>
        println(s"Error: Unknown type '$unknownType'. Expected 'BrowsingRequest' or 'PriorityRequest'")
      case Left(error) =>
        println(s"Error parsing input: ${error.getMessage}")
        println("Expected format: { \"type\": \"RequestType\", \"data\": { ... } }")
    }
  }

  /**
   * Handle BrowsingRequest JSON input
   */
  private def handleBrowsingRequest(jsonString: String): Unit = {
    decode[InputWrapper[BrowsingRequest]](jsonString) match {
      case Right(wrapper) =>
        val request = wrapper.data
        println(s"Delivering BrowsingRequest signal: $request")
        try {
          wfInstance.deliverSignal(Signals.startBrowsing, request)
          println("Signal delivered successfully!")
          println(s"New state: ${wfInstance.queryState()}")
        } catch {
          case e: Exception =>
            println(s"Error delivering signal: ${e.getMessage}")
        }
      case Left(error) =>
        println(s"Error parsing BrowsingRequest: ${error.getMessage}")
        running = false
    }
  }

  /**
   * Handle PriorityRequest JSON input
   */
  def handlePriorityRequest(jsonString: String): Unit = {
    decode[InputWrapper[PriorityRequest]](jsonString) match {
      case Right(wrapper) =>
        val request = wrapper.data
        println(s"Delivering PriorityRequest signal: $request")
        try {
          wfInstance.deliverSignal(Signals.setPriorities, request)
          println("Signal delivered successfully!")
          println(s"New state: ${wfInstance.queryState()}")
        } catch {
          case e: Exception =>
            println(s"Error delivering signal: ${e.getMessage}")
        }
      case Left(error) =>
        println(s"Error parsing PriorityRequest: ${error.getMessage}")
        running = false
    }
  }
}
