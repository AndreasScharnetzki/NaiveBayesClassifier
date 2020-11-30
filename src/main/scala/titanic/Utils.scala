package titanic

import java.io.PrintWriter
import java.util.concurrent.ThreadLocalRandom

import scala.util.Try


object Utils {

  // Regular Expressions for extracting the information
  val DATA_ACCESS_PATTERN_test = """(\d+),(\d),"(.+)",(male|female),([0-9]*\.[0-9]+|[0-9]+|d*),(\d*),(\d*),(.*),([0-9]*\.[0-9]+|[0-9]+|d*),(.*),(\w*)""".r
  val DATA_ACCESS_PATTERN_train = """(\d+),(\d),(\d),"(.+)",(male|female),([0-9]*\.[0-9]+|[0-9]+|d*),(\d*),(\d*),(.*),([0-9]*\.[0-9]+|[0-9]+|d*),(.*),(\w*)""".r

  // Reading text file
  // Stores the information in a map consisting of a property name (key) and its value
  def loadDataCSV(filename: String): List[Map[String, Any]] = {

    val stream = getClass.getResourceAsStream("/" + filename)
    val src = scala.io.Source.fromInputStream(stream)
    val iter = src.getLines().drop(1) //skip first line (property names)

    val result = (for (row <- iter) yield readData(row)).toList

    src.close
    result.flatMap(_ match { case p: Option[Map[String, Any]] => p })
  }


  // Extracting all information storing it into a Map[String,Any]
  def readData(line: String): Option[Map[String, Any]] = {

    def toInt(key: String, s: String): Option[(String, Int)] = Try(s.toInt).toOption.map((key, _))

    def toFloat(key: String, s: String): Option[(String, Float)] = Try(s.toFloat).toOption.map((key, _))

    def toString(key: String, s: String): Option[(String, String)] =
      if (s.nonEmpty) Some((key, s)) else None

    def createPassengerMap(t1: String, t2: String, t3: String, t4: String, t5: String, t6: String, t7: String,
                           t8: String, t9: String, t10: String, t11: String, t12: String): Option[Map[String, Any]] = {

      val l = List(
        toInt("passengerID", t1),
        toInt("survived", t2),
        toInt("pclass", t3),
        toString("name", t4),
        toString("sex", t5),
        toFloat("age", t6),
        toInt("sibsp", t7),
        toInt("parch", t8),
        toString("ticket", t9),
        toFloat("fare", t10),
        toString("cabin", t11),
        {
          if (t12.length > 0) Some(("embarked", t12(0))) else None
        })
      Some(l.flatMap(_ match { case p: Option[(String, Any)] => p }).toMap)
    }

    val result = line match {
      case DATA_ACCESS_PATTERN_test(t1, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) =>
        createPassengerMap(t1, "-1", t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)

      case DATA_ACCESS_PATTERN_train(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) => {
        createPassengerMap(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
      }

    }
    result
  }

  // Method for printing a passenger in a readable manner
  def printPassenger(p: Map[String, Any]): Unit = {

    println("\n---------------------------------------------------------------------")
    println("passengerID:" + p.getOrElse("passengerID", -1))
    println("survived:" + p.getOrElse("survived", -1))
    println("pclass:" + p.getOrElse("pclass", -1))
    println("name:" + p.getOrElse("name", "-"))
    println("sex:" + p.getOrElse("sex", "-"))
    println("age:" + p.getOrElse("age", -1))
    println("sibsp:" + p.getOrElse("sibsp", -1))
    println("parch:" + p.getOrElse("parch", -1))
    println("ticket:" + p.getOrElse("ticket", "-"))
    println("fare:" + p.getOrElse("fare", -1))
    println("cabin:" + p.getOrElse("cabin", -1))
    println("embarked:" + p.getOrElse("embarked", '-'))
    println("---------------------------------------------------------------------\n")
  }

  /**
   * Returns all missing values by attribute
   *
   * @param passengers list of passengers attribute maps
   * @param attList    A mapping from attributes to missing values count
   * @return
   *
   * src: https://stackoverflow.com/questions/31175283/elegant-way-handling-both-missing-key-and-null-values-from-scala-map
       /*
    val withoutNulls = passengers.map(p => p.filter{ case (k,v) => v != null } )
    val withoutFullEntries = passengers diff withoutNulls
   */
   */
  def countAllMissingValues(passengers: List[Map[String, Any]], attList: List[String]): Map[String, Int] = {

    //for every map in the list make an unzip version so only the keys for each map remain inside the list,
    //use diff() to create a list for each map of which arguments are missing
    //create the output map that counts the occurrences of each attributes name that is missing

    passengers.map(x => x.keys.toList).flatMap(y => attList diff y).groupBy(identity).mapValues{_.size}
  }

  //produces sometimes an missing argument list error - can be ignored
  def applyModel[CLASS, ID](model: (Map[String, Any], String) => (ID, CLASS),
                            testdata: Seq[Map[String, Any]], idKey: String): Seq[(ID, CLASS)] = {

    testdata.map(d => model(d, idKey))
  }

  def createSubmitFile[ID, CLASS](filename: String, data: Seq[(ID, CLASS)], header: String): Unit = {
    val pw = new PrintWriter(filename)
    pw.println(header)
    data.foreach(e => pw.println(e._1.toString + "," + e._2.toString))
    pw.close()
  }

  //====================================================================================================================

  //                                          [DATA PREPARATION]

  //====================================================================================================================

  //use random int to flag if you want to generate a random Value or use the one passed in method signature [true = use random; false = use valueToBeAssigned]
  def fillEmptyEntries(missingKey: String, valueToBeAssigned: Any, originalData: List[Map[String, Any]], useRandomValue: Boolean): List[Map[String, Any]] = {

    val intervall = findInterval(missingKey, originalData)
    val min = intervall(0)
    val max = intervall(1)

    val random: ThreadLocalRandom = ThreadLocalRandom.current()

    val withKey = originalData.filter(x => x.contains( missingKey ))
    //1) getting all maps that do NOT contain the age key
    val withoutKey = originalData diff withKey
    //2) adding the age(Key) and assign avg_age(value) to those maps
    val withAddedKey = withoutKey.map( x => x + ( missingKey -> (if (! useRandomValue) {
      valueToBeAssigned
    } else {
      random.nextDouble(min, max + 1)
    })))
    //3) merging both Lists[Maps[String, Any]]
    (withKey ++ withAddedKey).sortBy(_("passengerID").toString.toInt)  //src: https://stackoverflow.com/questions/29672800/how-to-sort-a-list-of-maps-in-scala
  }

  //used to find min and max values of given attribute
  def findInterval(attributeName: String, inputData: List[Map[String, Any]]) : List[Double] ={
    val max = inputData.filter(x => x.contains(attributeName)).map( y=> y(attributeName).toString.toDouble).max
    val min = inputData.filter(x => x.contains(attributeName)).map( y=> y(attributeName).toString.toDouble).min

    List(min, max)
  }

  //precondition: no missing keys in inputdata
  def categorize(key: String, inputData : List[Map[String, Any]], categorizeFunction: (Any) => Any ) : List[Map[String, Any]] ={
    inputData.map(x => x + (key -> categorizeFunction(x(key))))
  }


  def categorizeAge(age: Any): Any = {
    age match {
      case child      if (age.toString.toDouble <= 12.0) => "child"
      case adolescent if (age.toString.toDouble > 12.0 && age.toString.toDouble <= 18.0) => "adolescent"
      case adult      if (age.toString.toDouble > 18.0 && age.toString.toDouble <= 67.0) => "adult"
      case senior     if (age.toString.toDouble > 67.0) => "senior"
    }
  }

  def categorizeParentsAndChildren(parch: Any): Any = {
    parch match {
      case none         if (parch.toString.toInt == 0) => "no family members"
      case oneToTwo     if (parch.toString.toInt > 0 && parch.toString.toInt <= 2) => "one or two family members"
      case moreThanTwo  if (parch.toString.toInt > 2) => "at least three family members"
    }
  }

  def categorizeSiblingsOrSpouses(sibSp: Any): Any = {
    sibSp match {
      case none         if (sibSp.toString.toInt == 0) => "no sibling or spouse"
      case oneToTwo     if (sibSp.toString.toInt > 0 && sibSp.toString.toInt <= 2) => "one or two siblings/spouse"
      case moreThanTwo  if (sibSp.toString.toInt > 2) => "more than two siblings/spouse"
    }
  }


  //====================================================================================================================

  //                                          [NAIVE BAYES]

  //====================================================================================================================

  def condProb(classAttribute: String,
               classAttributesValue: Any,

               conditionAttribute: String,
               conditionValue: Any,

               inputData: List[Map[String, Any]]): Double = {

    //count occurrence of conditionAttribute
    val listOfConditionOccurrence = inputData.filter(y => y(conditionAttribute) == conditionValue)
    val countOfConditionOccurrence = listOfConditionOccurrence.length

    //count occurrence of classAttribute WITHIN set that have conditionAttribute
    val countOfMatches = listOfConditionOccurrence.count(x => x(classAttribute) == classAttributesValue)
    //print(classAttribute, classAttributesValue, countOfMatches,"/", countOfConditionOccurrence, conditionAttribute, conditionValue, countOfConditionOccurrence, "/", inputData.length)
    countOfMatches.toDouble / countOfConditionOccurrence.toDouble
  }

  //==================================================================================================================

  //use this for PriorProbabilities "how likely is it, given the fact you take a random dataentry to get sb with the conditions passed to the function?"
  def getPriorProbability(attributeName: String, attributeValue: Any, data: List[Map[String, Any]]): Double = {
    data.count(x => x(attributeName) == attributeValue) / data.flatMap(x => x.toList).count(y => y._1.equals(attributeName)).toDouble
  }

  //==================================================================================================================

  //returns categories of possible values for given attribute
  def getCategories(attribute: String, data: List[Map[String, Any]]): List[Map[String, Any]] = {
    data.map(x => x.filter(y => y._1.equals(attribute))).distinct
  }

  //==================================================================================================================

  //use this one to make a selection of keyvalue-pairs
  def selectData(listOfAttributes: List[String], inputData: List[Map[String, Any]]): List[Map[String, Any]] = {
    for (eachMapInInputData <- inputData) yield {
      listOfAttributes.map(x => x -> eachMapInInputData.getOrElse(x, "no Entry")).toMap
    }
  }

  //==================================================================================================================

  def training(trainingData: List[Map[String, Any]],
               className: String, //"survived"
               listOfAttributes: List[String]):

  Map[(Any, Any), Double] = {

    val result = for (classValue <- getCategories(className, trainingData)) yield {
      for (attributeName <- listOfAttributes) yield {
        for (attributeValue <- getCategories(attributeName, trainingData)) yield {
          ((classValue(className), attributeValue(attributeName)), scala.math.log(condProb(className, classValue(className), attributeName, attributeValue(attributeName), trainingData))
          )
        } //survived -> value; sex, pclass ect.
      }
    }
    result.flatten.flatten.toMap
  }

  //==================================================================================================================

  def classify( trainingData : List[Map[String, Any]],
                testData: List[Map[String, Any]],
                className: String,
                listOfAttributes: List[String]): //keys

  List[Any] = {

    //list of maps with All combinations of conditional probabilities
    val cpOfTraining = training(trainingData, className, listOfAttributes) //key of these maps consists of attribute values

    // calc prior properties of className with each attribute (List[Map[valueOfgivenKey, pp]])
    val listOfPP = getCategories(className, trainingData).map(x => (x(className), getPriorProbability(className, x(className), trainingData))) //List[Map[1 -> pp ], Map[0 -> pp]]

    //contains a map for every dataSet with key value pairs
    val selectedData = selectData(listOfAttributes, testData) //list[Map[String,Any]]

    //===start calculation here
    /*
    1) find all matching attributes (e.g.) given one class (survived = 1) for given list of attributes
    */

    val result =
      for (dataSet <- selectedData) yield {
        for (classNamePP <- listOfPP) yield {
          Map( classNamePP._1 -> (
            for (eachAttribute <- listOfAttributes) yield {
              cpOfTraining(classNamePP._1, dataSet(eachAttribute))
            })).map(z => z._1 -> ((z._2.sum) + scala.math.log(classNamePP._2)))
        }
      }   //x = list containing maps with both attributeValues -> assignedLikelihood e.g. (0, -0.234183064587)
    result.map(x => x.flatten.toMap.maxBy(_._2)._1)
  }

  def naiveBayes( trainingData: List[Map[String, Any]],
                  testData: List[Map[String, Any]],

                  className: String,

                  listOfAttributes: List[String]): // ("sex", "pclass", "fare")

  List[Any] = {

    //RUN FOREST RUN!
    classify(trainingData, testData, className, listOfAttributes)
  }

  //use this method to create tuples2 of (passengerID, classAttribute (survived 1/0) )
  def mapResultWithID(result: List[Any], testData: List[Map[String, Any]]) : List[(Any, Any)] ={
    testData.map(x=> x("passengerID")).zip(result)
  }

  def main(args: Array[String]): Unit = {

    val train = Utils.loadDataCSV("train.csv")
    val test = Utils.loadDataCSV("test.csv")

    //replace missing entries with random / determined value
    val updatedTrain = Utils.fillEmptyEntries("age",0, train,true)
    val trainCategorizedAge = Utils.categorize("age", updatedTrain, Utils.categorizeAge)
    val trainCategorizedParch = Utils.categorize("parch", trainCategorizedAge, Utils.categorizeParentsAndChildren)
    val trainCategorizedAll = Utils.categorize("sibsp", trainCategorizedParch, Utils.categorizeSiblingsOrSpouses)


    val updatedTest = Utils.fillEmptyEntries("age",0,test,true)
    val testCategorizedAge = Utils.categorize("age", updatedTest, Utils.categorizeAge)
    val testCategorizedParch = Utils.categorize("parch", testCategorizedAge, Utils.categorizeParentsAndChildren)
    val testCategorizedAll = Utils.categorize("sibsp", testCategorizedParch, Utils.categorizeSiblingsOrSpouses)

    //For different calculations change listOfAttributes
    val listOfAttributes = List("sex","pclass", "age")
    //val listOfAttributes_2 = List("sex", "age")                                 //this one returns best prediction!!!
    //val listOfAttributes_3 = List("sex","pclass", "age", "parch", "sibsp")

    //val result = Utils.mapResultWithID(Utils.naiveBayes(trainCategorizedAll, testCategorizedAll,"survived", listOfAttributes), test)
    val result = Utils.naiveBayes(trainCategorizedAll, testCategorizedAll, "survived", listOfAttributes)
    //Utils.createSubmitFile("gender_age_submission.csv",result,"passengerID, survived")
    print(result)
  }
}