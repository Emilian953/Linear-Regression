import Matrix.*

import scala.annotation.tailrec

object Regression {

  def calculate_error(X: Matrix, y: Matrix, W: Matrix): Double = {
    val m = if (X.height.isDefined) X.height.get else 0
    val predicted_values = X * W
    val error = predicted_values - y
    val sumErrors = error.data.getOrElse(List(List(0.0))).flatten.foldLeft(0.0)(_ + _)
    if (m > 0) sumErrors / m else 0.0
  }

  def gradientDescent(X: Matrix, y: Matrix, W: Matrix, alpha: Double, num_iters: Int): Matrix = {
    val m = y.height.getOrElse(0).toDouble

    @tailrec
    def loop(iter: Int, W: Matrix): Matrix = {
      if (iter <= 0) W
      else {
        val predictions = X * W
        val errors = predictions - y
        val gradient = (X.transpose * errors) / m
        val adjustment = gradient.map(_ * alpha)
        val W_adjusted = W - adjustment

        loop(iter - 1, W_adjusted)
      }
    }

    loop(num_iters, W)
  }

  def regression(dataset_file: String,
                attribute_columns: List[String],
                value_column: String,
                test_percentage: Double,
                alpha: Double,
                gradient_descent_steps: Int): (Matrix, Double) = {

    val dataset = Dataset.apply(dataset_file)
    val (trainingDataset, validationDataset) = dataset.split(test_percentage)
    
    val X = Matrix.apply(trainingDataset.selectColumns(attribute_columns)).++(1.0)
    val target_col = Matrix.apply(trainingDataset.selectColumn(value_column))
    
    val W_init = new Matrix(Some(List.fill(attribute_columns.size + 1)(List(0.0))))
    
    val W = gradientDescent(X, target_col, W_init, alpha, gradient_descent_steps)
    
    val X_validation = Matrix.apply(validationDataset.selectColumns(attribute_columns)).++(1.0)
    val target_validation = Matrix.apply(validationDataset.selectColumn(value_column))
    
    val mean_error = calculate_error(X_validation, target_validation, W)
    
    (W, mean_error)
  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}