package test

import org.scalatest.FunSuite
import titanic._

class UtilsTest extends FunSuite {

  // load datsets                    
  val train = Utils.loadDataCSV("train.csv")
  val test = Utils.loadDataCSV("test.csv")
  val all = train ++ test

  val miniDataTRAINING = train.takeWhile(x => x("passengerID") != 16) // ~15 datasets
  val miniDataTEST = test.takeWhile(x => x("passengerID") != 896)       // ~3 datasets


  test("Test size of the datesets") {

    assert(train.size === 891)
    assert(test.size === 418)
  }

  test("Count missing values test") {

    val attList = List("passengerID", "pclass", "survived", "name", "sex", "age", "sibsp", "parch",
      "ticket", "fare", "cabin", "embarked")

    val train_mv = Utils.countAllMissingValues(train, attList)
    val test_mv = Utils.countAllMissingValues(test, attList)
    assert(train_mv("cabin") == 687 && train_mv("age") == 177 && train_mv("embarked") == 2)
    assert(test_mv("cabin") == 327 && test_mv("age") == 86 && test_mv("fare") == 1)
  }


  test("fillEmptyEntries good test") {

    val attList = List("passengerID", "pclass", "survived", "name", "sex", "age", "sibsp", "parch",
      "ticket", "fare", "cabin", "embarked")

    val train_mv = Utils.countAllMissingValues(train, attList)
    val filledList = Utils.fillEmptyEntries("age", 9001, train, false)

    //original dataset contains 177 entries with missing age attribute, filledList doesn't contain age as missing key-value-pair :)
    assert(train_mv("age") == 177 && !Utils.countAllMissingValues(filledList, List("age")).contains("age"))
  }

  test("fillEmptyEntries run random") {

    val attList = List("passengerID", "pclass", "survived", "name", "sex", "age", "sibsp", "parch",
      "ticket", "fare", "cabin", "embarked")

    val train_mv = Utils.countAllMissingValues(train, attList)
    val filledList = Utils.fillEmptyEntries("age", 0, train, true)
    print(filledList)

    //original dataset contains 177 entries with missing age attribute, filledList doesn't contain age as missing key-value-pair :)
    assert(train_mv("age") == 177 && !Utils.countAllMissingValues(filledList, List("age")).contains("age"))
  }

  test("conditional Probability") {

    assert(Utils.condProb("survived", 1, "sex", "female", train) +
      Utils.condProb("survived", 0, "sex", "female", train) == 1)

    assert(Utils.condProb("survived", 1, "sex", "male", train) +
      Utils.condProb("survived", 0, "sex", "male", train) == 1)

    assert(Utils.condProb("survived", 1, "pclass", 1, train) +
      Utils.condProb("survived", 0, "pclass", 1, train) == 1)

    assert(Utils.condProb("survived", 1, "pclass", 2, train) +
      Utils.condProb("survived", 0, "pclass", 2, train) == 1)

    assert(Utils.condProb("survived", 1, "pclass", 3, train) +
      Utils.condProb("survived", 0, "pclass", 3, train) == 1)

  }

  test("conditional Probability Epsilon") {

    val trueValue = Utils.condProb("survived", 1, "sex", "female", train)

    assert(Utils.condProb("survived", 1, "sex", "female", train) <= trueValue + 0.0000000001 &&
      Utils.condProb("survived", 1, "sex", "female", train) >= trueValue - 0.0000000001)

  }

  test("getPriorProbability goodTest") {
    assert(BigDecimal(Utils.getPriorProbability("survived", 1, train)).setScale(3, BigDecimal.RoundingMode.HALF_UP) == 0.384)
  }

  test("getCategories") {
    assert(Utils.getCategories("survived", train) == List(Map("survived" -> 0), Map("survived" -> 1)))
  }

  test("selectData") {
    assert(Utils.selectData(List("survived"), train).distinct == List(Map("survived" -> 0), Map("survived" -> 1)))
  }

  test("training") {
    //minidata contains the first 15 dataSets of training dataset
    //7/8 female passengers survived, 8/8 male passengers died

    assert(Utils.training(train, "survived", List("sex")) ==
      Map(  (0,"male")    -> -0.20937397059045276,
            (0,"female")  -> -1.3549438312358146,
            (1,"male")    -> -1.666494384278956,
            (1,"female")  -> -0.2983545323425529)
    )
  }

  test("classify") {
    // original "survival" attribute of first 15 passengers List(0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1)
    //to check if we are correct we used the training dataset twice

    assert(Utils.classify(miniDataTRAINING, miniDataTRAINING, "survived", List("sex", "pclass")) ==
      List(0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1))
  }

  test ("findInterval") {
    assert(Utils.findInterval("age", train) == List(0.42, 80.0))
  }

  test ("categorizeAge"){
    val filledList = Utils.fillEmptyEntries("age", 0, train, true)

    def categorizeAge(age: Any): Any ={
      age match {
        case child      if (age.toString.toDouble <= 12.0) => "child"
        case adolescent if (age.toString.toDouble > 12.0 && age.toString.toDouble <= 18.0) => "adolescent"
        case adult      if (age.toString.toDouble > 18.0 && age.toString.toDouble <= 67.0) => "adult"
        case senior     if (age.toString.toDouble > 67.0) => "senior"
      }
    }

    val categorizedData = Utils.categorize("age", filledList, categorizeAge)

    val numberOfCategorizesEntries =
      categorizedData.count(x => x("age") == "child")       +
      categorizedData.count(x => x("age") == "adolescent")  +
      categorizedData.count(x => x("age") == "adult")       +
      categorizedData.count(x => x("age") == "senior")

    assert(numberOfCategorizesEntries == train.length)
  }

  test ("mapResultWithID") {
    assert(Utils.mapResultWithID(Utils.naiveBayes(miniDataTRAINING, miniDataTEST, "survived", List("sex", "pclass")), miniDataTEST) ==
      List((892,0), (893,1), (894,0), (895,0)))
  }
}