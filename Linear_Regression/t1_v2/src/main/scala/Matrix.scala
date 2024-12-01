type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  def transpose: Matrix = {
    data match {
      case None => new Matrix(None)
      case Some(mat) => new Matrix(Some(mat.transpose))
    }
  }

  def map(f: Double => Double): Matrix = m match {
    case None => new Matrix(None)
    case Some(mat) => new Matrix(Some(mat.map(row => row.map(f))))
  }


  def *(other: Matrix): Matrix = {
    (data, other.data) match {
      case (None, _) => Matrix(None)
      case (_, None) => Matrix(None)
      case (Some(mat1), Some(mat2)) => if (width != other.height) return new Matrix(None)
        
        val prod = mat1.map { row =>
          other.transpose.data.get.map { col =>
            row.zip(col).map { case (r, c) => r * c }.sum
          }
        }
        
        new Matrix(Some(prod))
    }
  }

  def ++(x: Double): Matrix = {
    m match {
      case None => new Matrix(None)
      case Some(data) =>
        val append = data.map(row => row :+ x)
        
        new Matrix(Some(append))
    }
  }

  def -(other: Matrix): Matrix = (this.m, other.data) match {
    case (None, _) => Matrix(None)
    case (_, None) => Matrix(None)
    case (Some(mat1), Some(mat2)) => if (mat1.length != mat2.length || width != other.width) return new Matrix(None)
      val diff = (mat1 zip mat2).map {
        case (row1, row2) => (row1 zip row2).map {
          case (elem1, elem2) => elem1 - elem2
        }
      }
      
      new Matrix(Some(diff))
  }

  def /(divisor: Double): Matrix = m match {
    case None => new Matrix(None)
    case Some(mat) => new Matrix(Some(mat.map(row => row.map(_ / divisor))))
  }

  def data: Option[Mat] = m

  def height: Option[Int] = {
    data match {
      case None => None
      case Some(mat) => Some(mat.length)
    }
  }

  def width: Option[Int] = {
    data match {
      case None => None
      case Some(mat)  => Some(mat.head.length)
    }
  }


  override def toString: String = m match {
    case None => "Error"
    case Some(matrix) => matrix.map(_.mkString(" ")).mkString("\n")
  }
}

object Matrix {
  def apply(data: Mat): Matrix = new Matrix(Some(data))
  def apply(data: Option[Mat]): Matrix = new Matrix(data)
  def apply(dataset: Dataset): Matrix = new Matrix(Some(dataset.getRows.map(_.map(_.toDouble))))
}
