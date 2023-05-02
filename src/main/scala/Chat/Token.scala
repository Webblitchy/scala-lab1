package Chat

enum Token:
  case // Terms
       BONJOUR, 
       SVP,
       
       // Questions
       QUEL,
       COMBIEN,
       
       // Articles
       JE,
       ME,
       LE,
       MON,
       DE,

       // Adjectives
       ASSOIFFE,
       AFFAME,
       // Actions
       ETRE,
       VOULOIR,
       COUTER,
       COMMANDER,
       CONNAITRE,
       APPELER,
       // Logic Operators
       ET,
       OU,
       // Products
       PRODUIT,
       MARQUE,
       PRIX,
       SOLDE,
       // Util
       PSEUDO,
       NUM,
       EOL,
       UNKNOWN,
       BAD
end Token
