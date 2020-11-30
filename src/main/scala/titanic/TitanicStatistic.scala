package titanic

import vegas._
import scalafx.application.Platform

object TitanicStatistic extends App {

  // load datasets
  val train = Utils.loadDataCSV("train.csv")
  val test = Utils.loadDataCSV("test.csv")
  val all = train ++ test

  println("Train Dataset:" + train.size + " Elements")
  println("Test Dataset:" + test.size + " Elements")
  println("whole Dataset:" + all.size + " Elements")

  Platform.implicitExit_=(false)
  val chart1 = Vegas("Passengers split by sex").
    withData(train).
    mark(Bar).
    encodeX("sex", Ordinal, axis = Axis(title = "Sex")).
    encodeY("passengerID", Quantitative, AggOps.Count, axis = Axis(title = "Passengers"))

  val passengers = train.size
  val survivedPass = train.count(m => m("survived") == 1)
  val rate = survivedPass.toDouble / passengers
  println("probability of surviving:" + rate)

  val chart2 = Vegas("Passengers classified by survival").
    withData(train).
    mark(Bar).
    addTransform("survival", "datum.survived == 0 ? \"Dead\" : \"Alive\"").
    encodeX("survival", Ordinal, axis = Axis(title = "Survival")).
    encodeY("passengerID", Quantitative, AggOps.Count, axis = Axis(title = "Passengers"))

  val chart3 = Vegas("Survival split by sex").
    withData(train).
    mark(Bar).
    addTransform("survival", "datum.survived == 0 ? \"No\" : \"Yes\"").
    encodeY("passengerID", Quantitative, AggOps.Count, axis = Axis(title = "Passengers")).
    encodeX("sex", Ord).
    encodeColor("survival", Nominal, scale = Scale(rangeNominals = List("#EA98D2", "#659CCA")))

  val chart4 = Vegas("Survival split by sex").
    withData(train).
    mark(Bar).
    addTransform("survival", "datum.survived == 0 ? \"No\" : \"Yes\"").
    encodeY("passengerID", Quantitative, AggOps.Count, axis = Axis(title = "Passengers")).
    encodeX("sex", Ord).
    encodeColor("survival", Nominal, scale = Scale(rangeNominals = List("#EA98D2", "#659CCA"))).
    configMark(stacked = StackOffset.Normalize)

  val chart5 = Vegas("Age Distribution split by Survival").
    withData(train).
    mark(Bar).
    addTransform("survival", "datum.survived == 0 ? \"No\" : \"Yes\"").
    addTransform("Age Description", "datum.age <= 4 ? \"1-Infant\" : datum.age<=15 ? \"2-Child\" : datum.age<=50 ? \"3-Adult\" : \"4-Old\"").
    encodeX("Age Description", Ordinal).
    encodeY("passengerID", Quantitative ,AggOps.Count,axis=Axis(title="Passengers")).
    encodeColor("survival", Nominal, scale=Scale(rangeNominals=List("#EA98D2", "#659CCA")))

  val chart6 = Vegas("Survival split by PClass").
    withData(train).
    mark(Bar).
    addTransform("survived", "datum.survived == 0 ? \"No\" : \"Yes\"").
    addTransform("pclass", "datum.pclass == 1 ? \"first\" : datum.pclass == 2 ? \"second\" : datum.pclass == 3 ? \"third\" : \"noClass\"").
    encodeX("survived", Nominal).
    encodeY("passengerID", Quantitative ,AggOps.Count,axis=Axis(title="Passengers")).
    encodeColor("pclass", Ordinal, scale=Scale(rangeNominals=List("#EA98D2", "#659CCA")))

  val chart7 = Vegas("Survival rate by age").
    withData(train).
    mark(Bar).
    addTransform("survived", "datum.survived == 0 ? \"No\" : \"Yes\"").
    encodeX("age", Quant, bin=Bin(step=10.0,min= 0.0),axis=Axis(title="Age")).
    encodeY("passengerID", Quantitative ,AggOps.Count,axis=Axis(title="Passengers" ,format="s")).
    encodeColor("survived", Nominal, scale=Scale(rangeNominals=List("#EA98D2", "#659CCA"))).
    configMark(stacked = StackOffset.Normalize)

  val chart8 = Vegas("Survival rate by pclass").
    withData(train).
    mark(Bar).
    addTransform("survived", "datum.survived == 0 ? \"No\" : \"Yes\"").
    encodeX("pclass", Ord).
    encodeY("passengerID", Quantitative ,AggOps.Count,axis=Axis(title="Passengers" ,format="s")).
    encodeColor("survived", Nominal, scale=Scale(rangeNominals=List("#EA98D2", "#659CCA"))).
    configMark(stacked = StackOffset.Normalize)


  VegasUtils.showAllInBrowser(List(chart1, chart2, chart3, chart4, chart5, chart6, chart7, chart8))
}