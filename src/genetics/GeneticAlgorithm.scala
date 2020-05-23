package genetics
import scala.util.Random

object GeneticAlgorithm {

  /**
   * Uses a genetic algorithm to optimize a generic problem
   *
   * @param incubator     Determines how instances of type T are created from a List of Doubles (genes)
   * @param costFunction  Determines the cost for a given instance of T
   * @param numberOfGenes The size of the List expected by the incubator
   * @tparam T The type to be optimized
   * @return An instance of T with minimal cost
   */
  def geneticAlgorithm[T](incubator: List[Double] => T, costFunction: T => Double, numberOfGenes: Int): T = {
    def make_animal(numberOfGenes: Int): List[Double] ={
      val animal: List[Double] = List.fill(numberOfGenes)(Random.nextDouble() * (Random.nextInt(200) - 100) )
      animal
    }

    def get_random[T](incubator: List[Double] => T, numberOfGenes: Int, costFunc: T => Double): T ={
      println("get_random")
      val animal_1: List[Double] = make_animal(numberOfGenes)
      val animal_2: List[Double] = make_animal(numberOfGenes)
      val animal_3: List[Double] = make_animal(numberOfGenes)
      val animal_4: List[Double] = make_animal(numberOfGenes)
      val animal_5: List[Double] = make_animal(numberOfGenes)
      val population: List[T] = List(incubator(animal_1), incubator(animal_2), incubator(animal_3), incubator(animal_4), incubator(animal_5))
      val best: T = do_cost[T](population, costFunc, incubator: List[Double] => T, numberOfGenes, animal_1, animal_2, animal_3, animal_4, animal_5)
      best
    }

    def do_cost[T](populated: List[T], costFunc: T => Double, incubator: List[Double] => T, numberOfGenes: Int, animal_one: List[Double], animal_two: List[Double], animal_three: List[Double], animal_four: List[Double], animal_five: List[Double]): T = {
      println("do_cost")
      val costList: List[Double] = List(costFunc(populated(0)), costFunc(populated(1)), costFunc(populated(2)))
      val costList_sorted: List[Double] = costList.sorted
      val minCost: Double = costList.reduceLeft(_ min _)
      val secondCost: Double = costList(1)
      val minIndex: Int = costList.indexOf(minCost)
      val secondIndex: Int = costList.indexOf(secondCost)
      val ogBest: T = populated(minIndex)
      val secondAnimal: T = populated(secondIndex)
      val ogAnimal_Double: List[Double] = findBestDouble[T](ogBest, animal_one, animal_two, animal_three, incubator)
      val secondAnimal_Double: List[Double] = findBestDouble[T](secondAnimal, animal_one, animal_two, animal_three, incubator)
      val bestAnimal_Double: List[Double] = ogAnimal_Double//bestMutate[T](incubator: List[Double] => T, numberOfGenes, ogAnimal_Double, secondAnimal_Double)
      println("MutateBest", bestAnimal_Double)
      val bestAnimal: T = incubator(bestAnimal_Double)
      val bestAnimal_1: T = bestAnimal
      val bestAnimal_2Double: List[Double] = mutate[T](bestAnimal_Double, incubator: List[Double] => T, numberOfGenes)
      val bestAnimal_2: T = incubator(bestAnimal_2Double)
      val bestAnimal_3Double: List[Double] = mutate[T](bestAnimal_Double, incubator: List[Double] => T, numberOfGenes)
      val bestAnimal_3: T = incubator(bestAnimal_3Double)
      val bestAnimal_4Double: List[Double] = mutate[T](bestAnimal_Double, incubator: List[Double] => T, numberOfGenes)
      val bestAnimal_4: T = incubator(bestAnimal_4Double)
      val bestAnimal_5Double: List[Double] = mutate[T](bestAnimal_Double, incubator: List[Double] => T, numberOfGenes)
      val bestAnimal_5: T = incubator(bestAnimal_5Double)
      val population: List[T] = List(bestAnimal_1, bestAnimal_2, bestAnimal_3, bestAnimal_4, bestAnimal_5)
      if (minCost < 0.1){
        bestAnimal
      }
      else {
        val bestOne = do_cost[T](population, costFunc, incubator, numberOfGenes, bestAnimal_Double, bestAnimal_2Double, bestAnimal_3Double, bestAnimal_4Double, bestAnimal_5Double)
        println("bestOne", bestOne)
        bestOne
      }
    }

    def bestMutate[T](incubate: List[Double] => T, numGene: Int, OG: List[Double], second: List[Double]): List[Double] ={
      //val newMutate: List[Double] = OG zip second map { case (a, b) => (a + (b/5))/2}
      OG
    }

    def mutate[T](animal: List[Double], incubator: List[Double] => T, numberOfGenes: Int): List[Double] = {
      println("mutate")
      val og_animal: List[Double] = make_animal(numberOfGenes)
      val mutated_3: List[Double] = og_animal zip animal map { case (a, b) => (a + b)/2 }
      val mutated_2: List[Double] = mutated_3 zip animal map { case (a, b) => (a + b)/2 }
      val mutated_1: List[Double] = mutated_2 zip animal map { case (a, b) => (a + b)/2 }
      val mutated: List[Double] = mutated_1 zip animal map { case (a, b) => (a + b)/2 }
      mutated
    }

    def findBestDouble[T](best: T, doubles: List[Double], doubles1: List[Double], doubles2: List[Double], incubate: List[Double] => T): List[Double] ={
      println("findDouble")
      if (incubate(doubles) == best){
        doubles
      }
      else if (incubate(doubles1) == best){
        doubles1
      }
      else{
        doubles2
      }
    }
    val best: T = get_random[T](incubator, numberOfGenes, costFunction)
    best
  }
}

