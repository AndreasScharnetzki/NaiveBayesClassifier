import java.io.PrintWriter

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
   * /*
   * val withoutNulls = passengers.map(p => p.filter{ case (k,v) => v != null } )
   * val withoutFullEntries = passengers diff withoutNulls
   */
   */
  def countAllMissingValues(passengers: List[Map[String, Any]], attList: List[String]): Map[String, Int] = {

    //for every map in the list make an unzip version so only the keys for each map remain inside the list,
    //use diff() to create a list for each map of which arguments are missing
    //create the output map that counts the occurrences of each attributes name that is missing

    passengers.map(x => x.keys.toList).flatMap(y => attList diff y).groupBy(identity).mapValues {
      _.size
    }
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

}

//=======================================[IHR DIE HIER EINTRETET, LASST ALLE HOFFNUNG FAHREN]===========================
val temp = Utils.loadDataCSV("train.csv")
val training = Utils.loadDataCSV("train.csv")

val passengerIDsOfTheSurvivors = temp.map(x => x.filterKeys(_.equals("survived")).filter(y => y._2 == 1)).zipWithIndex.map {
  case (k, v) => (k, v + 1)
}.filter(a => a._1.nonEmpty).unzip._2

passengerIDsOfTheSurvivors.map(x => training.drop(x))

def getDataWith(attributeName: String, attributeValue: Any): List[Map[String, Any]] = {
  training.filter(m => m("sex") == "male")

  val passengerIDsOfTheSurvivors = temp.map(x => x.filterKeys(_.equals(attributeName)).filter(y => y._2 == attributeValue)).zipWithIndex.map {
    case (k, v) => (k, v + 1)
  }.filter(a => a._1.nonEmpty).unzip._2

  passengerIDsOfTheSurvivors.flatMap(x => training.drop(x))

  /*
  //get tuples with specified attributeName
  //filters by attributeValue
  //creates index -> change for ID-retrieve
  //eliminates empty tuples that hadn't satisfied predicate
  //intersect with original content
  */
}



//======================================================================================================================


def countDataWith(filename: String, attributeName: String, attributeValue: Any): Int = {
  val temp = Utils.loadDataCSV(filename)
  temp.count(m => m(attributeName) == attributeValue)
}

//use this for PriorProbabilities
def getProbabilities(attributeName: String, attributeValue: Any, data: List[Map[String, Any]]): Double = {
  data.count(x => x(attributeName) == attributeValue) / data.flatMap(x => x.toList).count(y => y._1.equals(attributeName)).toDouble
}
getProbabilities("pclass", 3, training)
getProbabilities("sex", "male", training)


//count percentage of how many male passengers survived
training.filter(y => y("sex") == "male").count(x => x("survived") == 1) / training.length.toDouble
training.filter(y => y("sex") == "male").count(x => x("survived") == 0) / training.length.toDouble
training.filter(y => y("pclass") == 3).count(x => x("survived") == 1) / training.length.toDouble
training.filter(y => y("pclass") == 3).count(x => x("survived") == 0) / training.length.toDouble

//val categories = training.flatMap(x => x.keySet).map(y => y -> List()).toMap


//passengerID, warscheinlichste Klasse für gegebenen Datenpunkt/-satz :Map[String, String]
//def nb(attributeName: String, data: List[Map[String, Any]], test: List[Map[String, Any]]) = {

val data = Utils.loadDataCSV("train.csv")
val test = Utils.loadDataCSV("test.csv")

val survived = data.filter(x => x("survived") == 1) //List[Map[String, Any]]
val dead = data.filter(x => x("survived") == 0) //List[Map[String, Any]]

val numberOfSurvivors = survived.length
val numberOfDead = dead.length
val numberOfAll = data.length

val probabilitySurvive = numberOfSurvivors / numberOfAll.toDouble
val probabilityDead = numberOfDead / numberOfAll.toDouble

//attList = keyset
//val numberOfKeyAttributes = training.flatMap(x => x.keySet).distinct.size // 12
//val allKeyAttributes = training.flatMap(x => x.keySet).distinct

//val remaining = data.filter(x => x.size == numberOfKeyAttributes).size


//val emptyKeyValuePairs = training.flatMap(x => x.keySet).map(y => y -> "foo").toMap


//write a function that iterates over list
/*

1) check each map, if size == 12 -> ignore
2) else: get each key of incomplete map, make intersection with list of complete keys
3) take intersection create new map with missingKeys -> nil; merge with old map

 */
//List[Map[String, Any]]
//val incompleteEntries = data.filter(x => x.size != 12)

//val tempo = incompleteEntries.flatMap(x => allKeyAttributes.map(y => if (!x.contains(y)) x + (y -> null))).filter(_ != ())


// CALCULATING AVG_AGE


val allEntriesContaining_AgeEntry = data.filter(x => x.contains("age"))
val sumOfAges = allEntriesContaining_AgeEntry.map(x => x("age").toString.toDouble).sum
val avgAge = sumOfAges / data.length.toDouble


//ASSIGNING AVG_AGE TO THOSE ENTRIES THAT DO NOT CONTAIN "AGE"-KEY

//get all entries + replace those that are lacking the "age"-key with avg_value
val listOfAllAges = data.map(x => x.getOrElse("age", avgAge))

// prepare original data
//1) getting all maps that do NOT contain the age key
val allEntriesNOTContainingAgeKey = data diff allEntriesContaining_AgeEntry
//2) adding the age(Key) and assign avg_age(value) to those maps
val addedAgeKeyToMapsThatLackThisKey = allEntriesNOTContainingAgeKey.map(x => x + ("age" -> avgAge))
//3) merging both Lists[Maps[String, Any]]
val finalList = allEntriesContaining_AgeEntry ++ addedAgeKeyToMapsThatLackThisKey


//BAYES
/*
Problem: All attributes need to be categorized

Find

=== P(survived|Sex) ===

P(survived = 1 | sex = male)
P(survived = 1 | sex = female)

P(survived = 0 | sex = male)
P(survived = 0 | sex = female)

=== P(survived|Age) ===

P(survived = 1 | age >= avg_Age)
P(survived = 1 | age < avg_Age)

P(survived = 0 | age >= avg_Age)
P(survived = 0 | age < avg_Age)

*/
//example: training.filter(y => y("sex") == "male").count(x => x("survived") == 1) / training.length.toDouble

/*
condProb("survived", 1, "sex", "female", data)
condProb("survived", 0, "sex", "female", data)

condProb("survived", 1, "sex", "male", data)
condProb("survived", 0, "sex", "male", data)

condProb("survived", 1, "pclass", 1, data)
condProb("survived", 0, "pclass", 1, data)

condProb("survived", 1, "pclass", 2, data)
condProb("survived", 0, "pclass", 2, data)

condProb("survived", 1, "pclass", 3, data)
condProb("survived", 0, "pclass", 3, data)

 */

def fillEmptyEntries(missingKey: String, valueToBeAssigned: Any, originalData: List[Map[String, Any]]): List[Map[String, Any]] = {

  val withKey = originalData.filter(x => x.contains(missingKey))

  //1) getting all maps that do NOT contain the age key
  val withoutKey = originalData diff withKey
  //2) adding the age(Key) and assign avg_age(value) to those maps
  val withAddedKey = withoutKey.map(x => x + (missingKey -> valueToBeAssigned))
  //3) merging both Lists[Maps[String, Any]]
  withKey ++ withAddedKey
}


def naiveBayes( trainingData: List[Map[String, Any]],
                testData: List[Map[String, Any]],

                className: String,

                listOfAttributes: List[String]): // ("sex", "pclass", "fare")

                List[Any] = {

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

  //====================================================================================================================

  //use this for PriorProbabilities "how likely is it, given the fact you take a random dataentry to get sb with the conditions passed to the function?"
  def getPriorProbability(attributeName: String, attributeValue: Any, data: List[Map[String, Any]]): Double = {
    data.count(x => x(attributeName) == attributeValue) / data.flatMap(x => x.toList).count(y => y._1.equals(attributeName)).toDouble
  }

  //====================================================================================================================

  //returns categories of possible values for given attribute
  def getCategories(attribute: String, data: List[Map[String, Any]]): List[Map[String, Any]] = {
    data.map(x => x.filter(y => y._1.equals(attribute))).distinct
  }

  //====================================================================================================================

  //use this one to make a selection of keyvalue-pairs
  def selectData(listOfAttributes: List[String], inputData: List[Map[String, Any]]): List[Map[String, Any]] = {
    for (eachMapInInputData <- inputData) yield {
      listOfAttributes.map(x => x -> eachMapInInputData.getOrElse(x, "no Entry")).toMap
    }
  }

//======================================================================================================================

  def training(trainingData: List[Map[String, Any]],
               className: String, //"survived"
               listOfAttributes: List[String]):

  Map[(Any, Any), Double] = {

    val result = for (classValue <- getCategories(className, trainingData)) yield {
      for (attributeName <- listOfAttributes) yield {
        for (attributeValue <- getCategories(attributeName, trainingData)) yield {
          ((classValue(className), attributeValue(attributeName)), condProb(className, classValue(className), attributeName, attributeValue(attributeName), trainingData)
          )
        } //survived -> value; sex, pclass ect.
      }
    }
    result.flatten.flatten.toMap
  }

//======================================================================================================================

  def classify( testData: List[Map[String, Any]],
                className: String,
                listOfAttributes: List[String]): //keys

                List[Any] = {

    //list of maps with All combinations of conditional probabilities
    //I AM A FAT MAP ((merkmal1, merkmal 2) -> cp)
    val cpOfTraining = training(trainingData, className, listOfAttributes) //key of these maps consists of attribute values

    // calc prior properties of className with each attribute (List[Map[valueOfgivenKey, pp]])
    val listOfPP = getCategories(className, trainingData).map(x => (x(className), getPriorProbability(className, x(className), trainingData))) //List[Map[1 -> pp ], Map[0 -> pp]]

    //contains a map for every dataSet with key value pairs
    val selectedData = selectData(listOfAttributes, testData) //list[Map[String,Any]]

    //===start calculation here
    /*
    1) find all matching attributes (e.g.) given one class (survived = 1) for given list of attributes

    example (attributelist contains "sex", "pclass") finde jetzt alle attribute für beide fälle (tot, überlebend), verrechne diese getrennt voneinander, gib den klasse der größeren von beiden zuürck
     */


    //i want to have List[Map[0 -> 0.9378465, 1 -> 0.077986345]]
    val result =
      for (dataSet <- selectedData) yield {
        for (classNamePP <- listOfPP) yield {
          Map( classNamePP._1 -> (
          for (eachAttribute <- listOfAttributes) yield {
            cpOfTraining(classNamePP._1, dataSet(eachAttribute))
          })).map(z => z._1 -> (scala.math.log(z._2.sum) + scala.math.log(classNamePP._2)))
        }
      }   //x = list containing two maps each
    result.map(x => x.flatten.toMap).map(y => y.maxBy(_._2)).map(_._1)
  }

  //RUN FOREST RUN!
  classify(testData, className, listOfAttributes)
}

naiveBayes(data, test, "survived", List("sex", "pclass"))






