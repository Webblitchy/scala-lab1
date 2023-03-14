import org.scalatest.*
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpec
import prop.*

import java.io.ByteArrayOutputStream
import Utils.{Dictionary, SpellCheckerService, SpellCheckerImpl}
import Chat.TokenizerService
import Chat.Token

class BotTenderTokenizerInputSuite extends AnyPropSpec with TableDrivenPropertyChecks with should.Matchers {
    val spellCheckerSvc: SpellCheckerService = new SpellCheckerImpl(Dictionary.dictionary)
    val tokenizerSvc: TokenizerService = new TokenizerService(spellCheckerSvc)
    
    val evaluateInput = MainTokenizer.evaluateInput(tokenizerSvc)

    // You can use this test to debug any input
    property("inputting") {
        evaluateInput("quitter")
    }

    property("inputting 'quitter'") {
        // capture output for testing therefore it is not shown in the terminal
        val outCapture = new ByteArrayOutputStream
        Console.withOut(outCapture) {
            evaluateInput("quitter") should equal(false)
        }
        outCapture.toString() should include ("Adieu.")
    }

    property("inputting 'santé !'") {
        evaluateInput("santé !") should equal(true)
    }
    property("Distance choucroute chourave should equal 4 ") {
        spellCheckerSvc.stringDistance("choucroute", "chourave") should equal(4)
    }
    property("Distance Joris Eliott should equal 6" ) {
        spellCheckerSvc.stringDistance("Joris", "Eliott") should equal(6)
    }
    property("Distance Eliott Eliot should equal 1" ) {
        spellCheckerSvc.stringDistance("Eliott", "Eliot") should equal(1)
    }
    property("Distance Animaux animals should equal 3") {
        spellCheckerSvc.stringDistance("Animaux", "animals") should equal(3)
    }

    property("inputting 'salut'") {
        spellCheckerSvc.getClosestWordInDictionary("bières") should equal("biere")
    }
    property("veux->vouloir"){
        spellCheckerSvc.getClosestWordInDictionary("veux") should equal("vouloir")
    }
    property(" input to output"){
        val tokenizer = tokenizerSvc.tokenize("salut je veux une bière")
        val currentToken: (String, Token) = tokenizer.nextToken()
        currentToken._1 should equal("bonjour")
        currentToken._2 should equal(Token.BONJOUR)
    }
}
