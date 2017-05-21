import scala.io.Source

//Création de la classe point
class Point(dx: Double, dy: Double) {
  val x: Double = dx
  val y: Double = dy

  //Methode toString pour afficher un opoint
  override def toString(): String = {
    "(" + x + ", " + y + ")"
  }

  //Methode pour calculer la distance par rapport a un autre point
  def dist(p: Point): Double = {
    return x * p.x + y * p.y
  }
}


object kmeans extends App {
/**********************************************************************	
**************************     Fonction   *****************************	
**********************************************************************/	
  def moyenneCluster(listePoints: List[Point]): Point = {
  	//calcul la moyenne de tout les points et retourne le résultat avec un objet point
    val sommeDesPoints = listePoints.reduceLeft((a: Point, b: Point) => new Point(a.x + b.x, a.y + b.y))
    
    /*var sommeDesPointsX: Double = 0
    var sommeDesPointsY: Double = 0
    var i = 0
    while( i < listePoints.length){
    	sommeDesPointsY = sommeDesPointsY + listePoints(i).y
    	sommeDesPointsX = sommeDesPointsX + listePoints(i).x
    	i = i + 1
    }
    //System.out.println("SOMME: " + sommeDesPointsX + ", " + sommeDesPointsY)
	return new Point(sommeDesPointsX / listePoints.length, sommeDesPointsY / listePoints.length)*/
    
    return new Point(sommeDesPoints.x / listePoints.length, sommeDesPoints.y / listePoints.length)
  }

  //Fonction qui permet d'afficher les clusters et means prenant la liste des points en paramètre
  def afficher(listePoints: Map[Int, List[Point]]) {
    for (clusterNumber <- listePoints.keys.toSeq.sorted) {
      System.out.println("  Cluster " + clusterNumber)

      val meanPoint = moyenneCluster(listePoints(clusterNumber))
      System.out.println("  Mean: " + meanPoint)


      /*var i = 0
      while(i < listePoints(clusterNumber).length - 1) {
        System.out.println("        " + listePoints(clusterNumber)(i))
        i = i + 1
      }*/
    }
  }//fin de la fonction afficher

  def repeter(clusters: Map[Int, List[Point]]): Map[Int, List[Point]] = {
    val unzippedClusters = (clusters: Iterator[(Point, Int)]) => clusters.map(cluster => cluster._1)

    /*var i = 0
    while(unzippedClusters > i){
    	System.out.println(unzippedClusters)
    	i = i + 1
    }*/

    //trouver les moyennes de tout le clusters
    val means =
      (clusters: Map[Int, List[Point]]) =>	//Elle applique une fonction donnée à tous les éléments d’une liste, map construit la liste des résultats.
        for (clusterIndex <- clusters.keys)
          yield moyenneCluster(clusters(clusterIndex))
          
    //trouver l'index le plus proche
    def lePlusProche(p: Point, means: Iterable[Point]): Int = {
      val distances = for (center <- means) yield p.dist(center)
      return distances.zipWithIndex.min._2
    }

    //étape d'affectation, un cluster est un groupe
    val newClusters = listePoints.groupBy((p: Point) => lePlusProche(p, means(clusters)))
    afficher(newClusters)
    return newClusters
  }//fin de la fonction repeter

/**********************************************************************	
************************** Fin Fonction   *****************************	
**********************************************************************/	

  val k: Int = 4

  //stocker les valeurs du fichier dans un tableau de tableau
  val donnee = Source.fromFile("DS2.csv").getLines.toArray.map(_.split(","))

  var listePoints: List[Point] = List(
  		new Point(donnee(0)(0).toDouble, donnee(0)(1).toDouble)
  	)

  	/*var z = 1
  	while (z < donnee.length){
  		System.out.print("(")
  	   	System.out.print(donnee(z)(0))
  	   	System.out.print(", ")
  	  	System.out.print(donnee(z)(1))
  	  	System.out.print(")")
  	  	System.out.println(" ")
  	  	z = z + 1
  	}*/
  	/*var w: Int = 0
  	while (w < donnee.length){
  		System.out.println(w+ "= " + donnee(w).toList)
  		w = w + 1
  	}*/

  	//var i = donnee.length-1
  	var i = 8
  	var x = 0					//variable buff pour initialiser objet point
  	var y = 0					//variable buff pour initialiser objet point
  	while (i > 0){				//on décremente pour ajouter les données et 0 est deja mis a l'initialisation
  		var x = donnee(i)(0)
  		var y = donnee(i)(1)
  		listePoints ::= new Point(x.toDouble, y.toDouble)
  		i = i - 1
  	}
  	
  	listePoints.foreach{ println }  
  	System.out.println("TEST")

  	var l: List[Point] = listePoints.sortBy(		//liste créer initialiser avec la liste listePoints et trier
      p => (p.x + " " + p.y).hashCode())			//si deux objets sont égaux en invoquant la méthode equals() alors leur méthode hashCode() doit renvoyer la même valeur pour les deux objets

  	listePoints = l 								//on remet les valeur dans la liste listePoints après les avoir tier
    listePoints.foreach{ println }   			//afficher 

    //x._2 pour ce référer à des champs dans un tuple avec numérotations
    //zipWithIndex rajoute un index pour chaque variable présente dans la liste
    //yield retourne une nouvelle collection à partir de la boucle
    val clusters = listePoints.zipWithIndex.groupBy(x => x._2 % k) transform (  //Transforme toutes les valeurs associées
        (i: Int, p: List[(Point, Int)]) => for (x <- p) yield x._1)


  System.out.println("Initial State: ")
  afficher(clusters)

  //nombre d'itération à éfectuer
  var clusterDeTest = clusters
  i = 0
  while(i <= 5) {
    System.out.println("Iteration: " + i)
    clusterDeTest = repeter(clusterDeTest)
    i = i + 1
  }
}