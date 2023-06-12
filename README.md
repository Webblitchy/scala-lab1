# Tenderbot
## Description du projet 
Ce code est notre implémentation du laboratoire proposé sur 1 semestre durant le cours de SCALA en 2022-2023 à la HEIG-vd.
Le code fournit une implémentation d'un bot qui répond à des phrases en langage naturel. Nous avons donc implémenté une grammaire, un parser et un interpréteur (Analyser) pour répondre à des phrases en langage naturel. Dans la dernière partie du laboratoire, nous avons intégré ce bot dans un système de chat disponible par HTTP.

## Choix d'architecture
De nombreuses classes étaient fournies au début de chaque partie, nous avons trouvé les classes suffisamment bien spécifiée pour ne pas avoir besoins d'en faire de nouvelles, nous avons donc suivis assez fidèlement le modèle proposé par le laboratoire.
Nous utilisons la class Parser pour générer une suite de token (ExprTree) ou une erreur (si nous n'avons pas prévu cette suite de mots).
L'ExprTree permet de représenter la phrase avec un arbre normalisé et nous pouvons analyser cet arbre grâce à la class AnalyserService qui va nous permettre de répondre à la phrase. La logique du bot est implémentée dans la class Analyser, cette class porte peut-être mal son nom, car la logique du bot n'est pas dans une class bot, mais dans l'analyser. Cependant il nous semblait cohérent qu'après analyse de la phrase, une réponse soit générer et il nous semblait pas efficace de refaire après analyse la logique du bot ailleurs. Cette organisation semblait aussi amenée naturellement par les consignes du laboratoire.

## Choix d'implémentation  
Nous avons décider de proposer des méthodes d'aide dans la class Analyser afin qu'elle permettent de fournir une aide.
Nous avons ainsi fait les méthodes`list_of_item` et `simplifyTree`. La première permet de récupérer la liste des items de la phrase (en vérifiant que l'ExprTree est une commande) et la seconde permet de simplifier l'arbre en supprimant les nœuds inutiles. De cette manière, nous avons un moyen simple de connaitre la commande à effectuer et les éléments qui la compose.

## Gestion des futurs  
Nous avons choisi de partager les tâches entre l'analyzer et le messageRoute. Un futur contenant le message a envoyé est généré par l'analyzer. Ce futur est immédiatement retourné à l'instance messageRoute et nous pouvons donc envoyé le message avec la commande partiel, complète ou une erreur. Nous avons choisi cette implémentation afin d'avoir la logique dans l'analyzer (qui représente le bot). Nous trouvons que c'est plus simple de s'y retrouver ainsi. Comme nous retournons un futur, nous avons choisis de faire une nouvelle méthode et de ne pas appeler la fonction reply du bot. En effet, les types de retours ne sont pas les mêmes, nous avons envisagé de retourner une Either afin de pouvoir retourner une fois un futur (si c'est une commande et une fois une string), mais nous n'y voyions pas d'avantage, car après chaque appel à `reply` nous aurions dû faire un `match`, ici nous faisons une seule foi un match au début et nous appelons `reply` ou `buy` en fonction du type que nous voulons. La dernière manière que nous avons envisagé est de faire un visiteur pattern ce qui permettrait au bot d'envoyer un message directement en ayant une référence sur l'instance messageRoute, mais nous avons trouvé que cela rendait le code plus compliqué à lire et à comprendre.

