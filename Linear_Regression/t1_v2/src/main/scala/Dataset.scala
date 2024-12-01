import scala.annotation.tailrec

class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m

  override def toString: String = {
    data.map(row => row.mkString(", ")).mkString("\n")
  }

  def selectColumn(col: String): Dataset = {
    val idx = data.head.indexOf(col)
    val column = data.map(row => List(row(idx)))

    new Dataset(column)
  }

  def selectColumns(cols: List[String]): Dataset = {
    val indexes = cols.map(col => data.head.indexOf(col))
    val columns = data.map(row => indexes.map(idx => row(idx)))

    new Dataset(columns)
  }


  def split(percentage: Double): (Dataset, Dataset) = {
    val sortedData = data.tail.sortBy(_.head)
    
    val testSize = Math.ceil(percentage * size).toInt
    val step = Math.ceil(1.0 / percentage).toInt

    @tailrec
    def helper(sortedData: List[List[String]], transfer: Int, train: List[List[String]],
               test: List[List[String]]): (List[List[String]], List[List[String]]) = {
      sortedData match {
        case Nil => (train, test)
        case head :: tail => {
          if (transfer % step == 0) helper(tail, transfer + 1, train, test :+ head)
          else helper(tail, transfer + 1, train :+ head, test)
        }
      }
    }
    
    val (trainingSet, testingSet) = helper(sortedData, 1, List(data.head), List(data.head))
    
    (new Dataset(trainingSet), new Dataset(testingSet))

  }


  def size: Int = data.length - 1
  def getRows: List[List[String]] = data.tail
  def getHeader: List[String] = data.head
}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val dataFile = scala.io.Source.fromFile(csv_filename)

    val dataLines = dataFile.getLines().toList
    val data = dataLines.map(_.split(",").map(_.trim).toList)

    new Dataset(data)
  }

  def apply(ds: List[List[String]]): Dataset = new Dataset(ds)
}
