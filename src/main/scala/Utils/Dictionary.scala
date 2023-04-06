package Utils

/**
* Contains the dictionary of the application, which is used to validate, correct and normalize words entered by the
* user.
*/
object Dictionary:
  // This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
  // we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
  val dictionary: Map[String, String] = Map(
    // Interjections
    "salut" -> "bonjour",
    "bonjour" -> "bonjour",
    "hello" -> "bonjour",
    "yo" -> "bonjour",
    "svp" -> "svp",
    "stp" -> "svp",

    // Questions
    "quel"-> "quel",
    "quelle"-> "quel",
    "combien" -> "combien",
    // Articles
    "je" -> "je",
    "j" -> "je",
    "me" -> "me",
    "le" -> "le",
    "//la" -> "le",
    "mon" -> "mon",
    //"ma" -> "mon",
    "de" -> "de",
    // Adjectives  
    "assoiffé" -> "assoiffe",
    "assoiffée" -> "assoiffe",
    "affamé" -> "affame",
    "affamée" -> "affame",

    // Actions
    "suis" -> "etre",
    "est" -> "etre",
    "veux" -> "vouloir",
    "aimerais" -> "vouloir",
    "souhaite" -> "vouloir",
    "souhaiterais" -> "vouloir",
    "veut" -> "vouloir",
    "couter" -> "couter",
    "coute" -> "couter",
    "commander" -> "commander",
    "connaitre" -> "connaitre",
    "appeler" -> "appeler",

    // Logic Operators
    "et" -> "et",
    "ou" -> "ou",

    //Products
    "bière" -> "biere",
    "bières" -> "biere",
    "croissant" -> "croissant",
    "croissants" -> "croissant",
    "maison"  -> "maison",
    "cailler"  -> "cailler",
    "farmer" -> "farmer",
    "boxer"  -> "boxer",
    "wittekop"  -> "wittekop",
    "punkipa"  -> "punkipa",
    "jackhammer" -> "jackhammer",
    "ténébreuse" -> "tenebreuse",
    "solde" -> "solde",
    "prix" -> "prix",
    // DONE - Part 2 Step 1
  )
end Dictionary
